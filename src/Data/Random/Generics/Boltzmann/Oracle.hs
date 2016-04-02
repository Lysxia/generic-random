{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes #-}
module Data.Random.Generics.Boltzmann.Oracle where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.State
import Data.Data
import Data.Functor
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as IntSet
import Data.Traversable
import GHC.Prim (Any)
import Unsafe.Coerce
import Data.Random.Generics.Boltzmann.Solver

data SomeData where
  SomeData :: Data a => a -> SomeData

-- | Map every type to its constructors, annotated with the types of their fields.
--
-- Primitive types and empty types are mapped to an empty constructor list, and
-- can be distinguished using @Data.Data.dataTypeRep@ on the attached
-- @SomeData@.
type H = HashMap TypeRep (SomeData, [(Constr, [TypeRep])])

-- | The type of the first argument of @Data.Data.gunfold@.
type GUnfold m = forall b r. Data b => m (b -> r) -> m r

-- | Find all types that may be types of subterms of a value of type @a@.
--
-- This will loop if there are infinitely many such types.
collectTypes :: Data a => a -> H
collectTypes a = collectTypesM a `execState` HashMap.empty

-- | Traversal of the definition of a datatype.
collectTypesM :: Data a => a -> State H ()
collectTypesM a = do
  h <- get
  let
    t = typeRep [a]
    d = dataTypeOf a
  unless (HashMap.member t h) $
    void . mfix $ \tyInfo -> do
      put (HashMap.insert t (SomeData a, tyInfo) h)
      if isAlgType d then do
        let
          cs = dataTypeConstrs d
          collect :: GUnfold (StateT [TypeRep] (State H))
          collect f' = f' >>= \f ->
            let b = ofType f
            in lift (collectTypesM b) >> modify' (typeRep [b] :) $> f undefined
        forM cs $ \c -> do
          ts <- gunfold collect return c `asTypeOf` return a `execStateT` []
          return (c, ts)
      else
        return []

-- | Type annotation. The produced value should not be evaluated.
ofType :: (b -> a) -> b
ofType _ = undefined

-- | We map types to integers to interface with the vector-based system solver.
--
-- Every type has an index @i >= 1@; the variable @X i@ represents its
-- generating function @C(x)@, and @X (i + n)@ its derivative @C'(x)@, where
-- @n@ is the total number of types present and recorded in the first
-- component of @Index@.
--
-- @X 0@ is the parameter @x@ of the generating functions.
type Index = (Int, HashMap TypeRep Int)

indexH :: H -> Index
indexH = mapAccumL (\i _ -> (i + 1, i + 1)) 0

(?), (?.) :: Index -> TypeRep -> Int

-- | Index of @C(x)@.
(_, h) ? t = h HashMap.! t

-- | Index of @C'(x)@.
(n, h) ?. t = h HashMap.! t + n

toEquations :: Index -> H -> [Equation]
toEquations ix h = HashMap.toList h >>= \(t, tyDesc) ->
  let
    e = toEquation ix t tyDesc
    e' = toEquation' ix e
  in
    [e, e']

-- | Equation defining the generating function @C(x)@ of a type.
--
-- If it is primitive, @C(x) = 1@ (i.e., its values have size 0).
toEquation :: Index -> TypeRep -> (SomeData, [(constr, [TypeRep])]) -> Equation
toEquation ix t (a_, tyInfo) = (X (ix ? t), eqn tyInfo)
  where
    eqn [] =
      case a_ of
        SomeData a ->
          case (dataTypeRep . dataTypeOf) a of
            AlgRep _ -> Zero
            _ -> One
    eqn xs = ((X 0 *) . sum . fmap (toProd . snd)) xs
    toProd = prod' . fmap (\t -> X (ix ? t))

-- | Equation defining the derivative @C'(x) of a type, from that of @C(x)@.
toEquation' :: Index -> Equation -> Equation
toEquation' (n, _) (X i, e2) = (X (i+n), e2')
  where
    e2' = sum [ d y * differentiate y e2 | y <- (IntSet.toList . collectXs) e2 ]
    d 0 = 1
    d i = X (i + n)
toEquation' _ _ = error "Expected equation produced by toEquation"

-- | The expected size @n@ of an object produced by a Boltzmann sampler with
-- generating function @C(x)@ is @n = x * C'(x) / C(x)@.
--
-- Given a target n, we will solve that equation for x.
sizeEquation :: Index -> TypeRep -> Int -> Equation
sizeEquation ix t n = (fromIntegral n * X (ix ? t), X 0 * X (ix ?. t))
