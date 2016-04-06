{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Random.Generics.Boltzmann.Oracle where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Functor
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as IntSet
import Data.Traversable
import GHC.Prim ( Any )
import Numeric.LinearAlgebra ( (!), vector )
import Unsafe.Coerce
import Data.Random.Generics.Boltzmann.PowerSeries
import Data.Random.Generics.Boltzmann.Solver

data SomeData where
  SomeData :: Data a => a -> SomeData

instance Show SomeData where
  show _ = "_"

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
-- If it is empty, @C(x) = 0@.
toEquation :: Index -> TypeRep -> (SomeData, [(constr, [TypeRep])]) -> Equation
toEquation ix t (a_, tyInfo) = (X (ix ? t), eqn tyInfo)
  where
    eqn [] =
      case a_ of
        SomeData a ->
          case (dataTypeRep . dataTypeOf) a of
            AlgRep _ -> Zero
            _ -> One
    eqn xs = X 0 * (sum' . fmap (toProd . snd)) xs
    toProd = prod' . fmap (\t -> X (ix ? t))

-- | Equation defining the derivative @C'(x) of a type, from that of @C(x)@.
toEquation' :: Index -> Equation -> Equation
toEquation' (n, _) (X i, e2) = (X (i+n), e2')
  where
    e2' = sum [ d y * differentiate y e2 | y <- (IntSet.toList . collectXs) e2 ]
    d 0 = 1
    d i = X (i + n)
toEquation' _ _ = error "Expected equation produced by toEquation"

type PowerSeries' = PowerSeries Integer

-- | Compute all generating functions @C(x)@.
gfEval :: [Equation] -> HashMap Int PowerSeries'
gfEval es = pss
  where
    pss = HashMap.fromList . zip [0 ..] $
      x 1 : fmap (eval (pss HashMap.!) . snd) es

-- | The expected size @n@ of an object produced by a Boltzmann sampler with
-- generating function @C(x)@ is @n = x * C'(x) / C(x)@.
--
-- Given a target n, we want to solve that equation for x.
sizeEquation :: Index -> TypeRep -> Int -> Equation
sizeEquation ix t n = (fromIntegral n * X (ix ? t), X 0 * X (ix ?. t))

-- | The whole system.
--
-- Since only one equation (the first one) depends on the size parameter, as
-- well as the target type, the others are thunked as soon as the first two
-- parameters are passed to this function.
sizedSystem :: Index -> H -> TypeRep -> Int -> [Equation]
sizedSystem ix h = \t n -> sizeEquation ix t n : es
  where
    es = toEquations ix h

type Oracle = HashMap Int Double

makeOracle :: H -> Index -> TypeRep -> Int -> Oracle
makeOracle h ix@(n, _) t size = HashMap.fromList
  [ (i, (v ! 0) ^ order i * eval (v !) e) | (X i, e) <- c ]
  where
    sz = (fromIntegral size * X i, xC')
      where
        i = ix ? t
        j = ix ?. t
        xC' | order i == 0 = X 0 * X j
            | otherwise = X j
    -- Equations defining C_i(x) for all types with indices i
    c = fmap (uncurry (toEquation ix)) (HashMap.toList h)
    -- Equations defining derivatives C_i'(x)
    c' = fmap (toEquation' ix) c
    -- C_i(x) as power series
    cPS = gfEval c
    -- C_i'(x) as power series
    c'PS = HashMap.fromList $
      fmap (\i -> (i + n, differentiateSeries (cPS HashMap.! i))) [1 .. n]
    ps = cPS `HashMap.union` c'PS
    fps = fmap factor ps
    order i = fst (fps HashMap.! i)
    leadCoef i =
      case snd (fps HashMap.! i) of
        [] -> 0 :: Double
        a : _ -> fromInteger a
    -- For @0 < i <= n@, the variable @X i@ in @es@ stands for @C_i(x)/x^k@,
    -- and @X (i+n)@ for @C_i'(x)/x^(k-1)@, where @k@ is the order of @C_i(x)@.
    es = fmap cToCOverXn (c ++ c')
    cToCOverXn (X i, e) = (X i, substAndDivide order (order i) e)
    cToCOverXn _ = error "This doesn't happen."
    -- Use the values of @C_i(x)@ at @x=0@ as the initial vector for the search.
    initialGuess = vector (1 : fmap (\(X i, _) -> leadCoef i) es)
    v = case solveEquations defSolveArgs (sz : es) initialGuess of
      Nothing -> error "Solution not found."
      Just v_ -> v_

-- | Maps a key representing a type @a@ to a generator @m a@.
type Generators m = HashMap TypeRep (m Any)

data PrimRandom m = PrimRandom
  { int :: m Int
  , double :: m Double
  , char :: m Char
  }

defPrimRandom :: MonadRandom m => PrimRandom m
defPrimRandom = PrimRandom getRandom getRandom getRandom

primRandomR :: MonadRandom m => (Int, Int) -> (Double, Double) -> PrimRandom m
primRandomR intR doubleR = PrimRandom
  (getRandomR intR)
  (getRandomR doubleR)
  getRandom

makeGenerators
  :: forall m. MonadRandom m => H -> Index -> Oracle -> PrimRandom m
  -> Generators m
makeGenerators h ix oracle PrimRandom{..} = generators
  where
    f (a_, tyInfo) = case a_ of
      SomeData a -> fmap unsafeCoerce $
        case tyInfo of
          [] ->
            let dt = dataTypeOf a in
            case dataTypeRep dt of
              IntRep ->
                fromConstr . mkIntegralConstr dt <$> int
              FloatRep ->
                fromConstr . mkRealConstr dt <$> double
              CharRep ->
                fromConstr . mkCharConstr dt <$> char
              AlgRep _ -> error "Cannot generate for empty type."
              NoRep -> error "No representation."
          _ -> choose (fmap g tyInfo) `fTypeOf` a
    g :: Data a => (Constr, [TypeRep]) -> (Double, m a)
    g (constr, ts) = (w, gunfold generate return constr `runReaderT` gs)
      where
        gs = fmap (generators HashMap.!) ts
        w = product $ fmap ((oracle HashMap.!) . (snd ix HashMap.!)) ts
    generate :: GUnfold (ReaderT [m Any] m)
    generate rest = ReaderT $ \(g : gs) -> do
      x <- g
      f <- rest `runReaderT` gs
      (return . f . unsafeCoerce) x
    generators = fmap f h

(!@) :: (Functor m, Typeable a) => Generators m -> proxy a -> m a
g !@ a = fmap unsafeCoerce (g HashMap.! typeRep a)

fTypeOf :: f a -> a -> f a
fTypeOf = const

choose :: MonadRandom m => [(Double, m a)] -> m a
choose as = do
  x <- getRandomR (0, total)
  select x as
  where
    total = (sum . fmap fst) as
    select x ((w, a) : as)
      | x <= w = a
      | otherwise = select (x - w) as
    select _ _ = error "Exhausted choices."

makeGenerator :: (Data a, MonadRandom m) => a -> Int -> m a
makeGenerator a size = fmap unsafeCoerce (generators HashMap.! t)
  where
    h = collectTypes a
    ix = indexH h
    t = typeRep [a]
    oracle = makeOracle h ix t size
    generators = makeGenerators h ix oracle defPrimRandom

-- | > substAndDivide p n e
--
-- Substitute @(X i)@ with @(X 0 ^ p i * X i)@ for @i > 0@, and divide the
-- result (assumed to be divisible) by @(X 0 ^ n)@.
substAndDivide :: (Int -> Int) -> Int -> Exp -> Exp
substAndDivide p n e = X 0 ^ (m - n) * e'
  where
    (m, e') = subst p e

-- | Substitute as explained for @substAndDivide@ and factor by @(X 0)@,
-- returning the exponent of the factored out power of @(X 0)@.
subst :: (Int -> Int) -> Exp -> (Int, Exp)
subst _ (X 0) = (1, 1)
subst p x@(X i) = (p i, x)
subst _ x@(Constant _) = (0, x)
subst p (Prod xs) = (sum ns, prod' ys)
  where
    (ns, ys) = (unzip . fmap (subst p)) xs
subst p (Sum xs) = (m, sum' ys)
  where
    (m, ys) = mapAccumL (\m' x ->
      let (m'', y) = subst p x
      in (min m'' m', X 0 ^ (m'' - m) * y)) (maxBound :: Int) xs
