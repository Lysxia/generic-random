{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams, PartialTypeSignatures #-}
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
import GHC.Stack ( CallStack, showCallStack )
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
--
-- We map types to integers for more efficient and practical indexing.
-- The first component @n@ is the total number of types present.
--
-- Every type has an index @1 <= i <= n@; the variable @X i@ represents its
-- generating function @C_i(x)@, and @X (i + k*n)@ the GF of its @k@-th
-- "pointing" @C^(k)(x)@: @C^(k+1)(x) = x * C^(k)'(x)@ where @C^(k)'@ is the
-- derivative of @C^(k)@.
--
-- @X 0@ is the parameter @x@ of the generating functions.
--
-- The /leading coefficient/ of a power series is its first non-zero
-- coefficient.

data DataDef = DataDef
  { count :: Int -- ^ Number of registered types
  , points :: Int -- ^ Number of iterations of the pointing operation
  , index :: HashMap TypeRep Int -- ^ Map from types to integers
  , xedni :: HashMap Int SomeData -- ^ Inverse map from index to types
  , types :: HashMap I [(Integer, Constr, [I])]
  -- ^ Structure of types and their pointings (up to @points@)
  --
  -- The integer is a multiplicity which can be > 1 for pointings.
  , order :: HashMap Int Int
  -- ^ Orders of the generating functions @C_i(x)@: smallest size of
  -- objects of a given type.
  , lCoef :: HashMap I Integer
  -- ^ Leading coefficients: number of objects of smallest size.
  } deriving Show

-- | A pair @(i,k)@ represents the @k@-th "pointing" of the type at index @i@,
-- with generating function @C_i^(k)(x)@.
type I = (Int, Int)

-- | The type of the first argument of @Data.Data.gunfold@.
type GUnfold m = forall b r. Data b => m (b -> r) -> m r

emptyDataDef :: DataDef
emptyDataDef = DataDef
  { count = 0
  , points = 0
  , index = HashMap.empty
  , xedni = HashMap.empty
  , types = HashMap.empty
  , order = HashMap.empty
  , lCoef = HashMap.empty
  }

-- | Find all types that may be types of subterms of a value of type @a@.
--
-- This will loop if there are infinitely many such types.
collectTypes :: Data a => a -> DataDef
collectTypes a = collectTypesM a `execState` emptyDataDef

-- | A wrapper to construct indices of @C_i^(k)@.
c :: Int -> Int -> I
c = (,)

-- | Primitive datatypes have @C(x) = x@ (i.e., are considered as
-- having a single object of size 1).
primOrder :: Int
primOrder = 1

primLeadCoef :: Integer
primLeadCoef = 1

collectTypesM :: Data a => a -> State DataDef (Int, Int, Integer)
collectTypesM a = do
  let t = typeRep [a]
  DataDef{..} <- get
  case HashMap.lookup t index of
    Nothing -> do
      let i = count + 1
      modify $ \dd -> dd
        { count = i
        , index = HashMap.insert t i index
        , xedni = HashMap.insert i (SomeData a) xedni
        , order = HashMap.insert i (error "Unknown order") order
        , lCoef = HashMap.insert (c i 0) 0 lCoef }
      collectTypesM' a t i -- Updates order and lCoef
    Just i ->
      let
        order_i = order #! i
        lCoef_i = lCoef #! c i 0
      in return (i, order_i, lCoef_i)

-- | Traversal of the definition of a datatype.
collectTypesM'
  :: Data a => a -> TypeRep -> Int -> State DataDef (Int, Int, Integer)
collectTypesM' a t i = do
  let d = dataTypeOf a
  (types_i, order_i, lCoef_i) <-
    if isAlgType d then do
      let
        constrs = dataTypeConstrs d
        collect :: GUnfold (StateT ([Int], Int, Integer) (State DataDef))
        collect mkCon = do
          f <- mkCon
          let b = ofType f
          (j, order_, lead_) <- lift (collectTypesM b)
          modify $ \(js, order', lead') ->
            (j : js, order_ + order', lead_ * lead')
          return (f b)
      cjols <- forM constrs $ \constr -> do
        (js, order', lead') <-
          gunfold collect return constr `asTypeOf` return a
            `execStateT` ([], 1, 1)
        return ((1, constr, [ c j 0 | j <- js]), (order', lead'))
      let
        (types_i, ols) = unzip cjols
        (order_i, lCoef_i) = minSum ols
      return (types_i, order_i, lCoef_i)
    else
      return ([], primOrder, primLeadCoef)
  modify $ \dd@DataDef{..} -> dd
    { types = HashMap.insert (c i 0) types_i types
    , order = HashMap.insert i order_i order
    , lCoef = HashMap.insert (c i 0) lCoef_i lCoef
    }
  return (i, order_i, lCoef_i)

-- | If @(o, l)@ represents a power series of order @o@ and leading coefficient
-- @l@, and similarly for @(o', l')@, this finds the order and leading
-- coefficient of their sum.
minPlus :: (Ord int, Eq integer, Num integer)
  => (int, integer) -> (int, integer) -> (int, integer)
minPlus ol@(order, lCoef) ol'@(order', lCoef')
  | lCoef' == 0 = ol
  | order < order' = ol
  | order > order' = ol'
  | otherwise = (order, lCoef + lCoef')

minSum :: (Ord int, Bounded int, Eq integer, Num integer)
  => [(int, integer)] -> (int, integer)
minSum = foldl minPlus (maxBound, 0)

-- | Pointing operation.
--
-- Populates a @DataDef@ with one more level of pointings.
--
-- The "pointing" of a type @t@ is a derived type whose values are essentially
-- values of type @t@, with one of its constructors being "pointed".
-- Alternatively, we may turn every constructor into variants that indicate
-- the position of points.
--
-- @
--   -- Original type
--   data Tree = Node Tree Tree | Leaf
--   -- Pointing of Tree
--   data Tree'
--     = Tree' Tree -- Point at the root
--     | Node'0 Tree' Tree -- Point to the left
--     | Node'1 Tree Tree' -- Point to the right
--   -- Pointing of the pointing
--   -- Notice that the "points" introduced by both pointing operations
--   -- are considered different: exchanging their positions (when different)
--   -- produces a different tree.
--   data Tree''
--     = Tree'' Tree' -- Point 2 at the root, the inner Tree' places point 1
--     | Node'0' Tree' Tree -- Point 1 at the root, point 2 to the left
--     | Node'1' Tree Tree' -- Point 1 at the root, point 2 to the right
--     | Node'0'0 Tree'' Tree -- Points 1 and 2 to the left
--     | Node'0'1 Tree' Tree' -- Point 1 to the left, point 2 to the right
--     | Node'1'0 Tree' Tree' -- Point 1 to the right, point 2 to the left
--     | Node'0'1 Tree Tree'' -- Points 1 and 2 to the right
-- @
--
-- If we ignore points, some constructors are equivalent. Thus we may simply
-- calculate their multiplicity instead of duplicating them.
--
-- Given a constructor with @c@ arguments @C x_1 ... x_c@, and a sequence
-- @p_0 + p_1 + ... + p_c = k@ corresponding to a distribution of @k@ points
-- (@p_0@ are assigned to the constructor @C@ itself), the
-- multiplicity of the constructor with that distribution is the
-- multinomial coefficient @multinomial k [p_1, ..., p_c]@.

point :: DataDef -> DataDef
point dd@DataDef{..} = dd
  { points = points'
  , types = foldl g types [1 .. count]
  , lCoef = foldl f lCoef [1 .. count]
  } where
    points' = points + 1
    f lCoef i = HashMap.insert (c i points') (lCoef' i) lCoef
    lCoef' i = (lCoef #! c i points) * toInteger (order #! i)
    g types i = HashMap.insert (c i points') (types' i) types
    types' i = types #! c i 0 >>= h
    h (_, constr, js) = do
      ps <- partitions points' (length js)
      let
        mult = multinomial points' ps
        js' = zipWith (\(j, _) p -> (j, p)) js ps
      return (mult, constr, js')

partitions :: Int -> Int -> [[Int]]
partitions k 0 = [[]]
partitions k n = do
  p <- [0 .. k]
  (p :) <$> partitions (k - p) (n - 1)

-- | Multinomial coefficient.
multinomial :: Int -> [Int] -> Integer
multinomial n [] = 1
multinomial n (p : ps) = binomial n p * multinomial (n - p) ps

-- | Binomial coefficient.
binomial :: Int -> Int -> Integer
binomial = \n k -> pascal !! n !! k
  where
    pascal = [1] : fmap nextRow pascal
    nextRow r = zipWith (+) (0 : r) (r ++ [0])

{-

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
    lCoef i =
      case snd (fps HashMap.! i) of
        [] -> 0 :: Double
        a : _ -> fromInteger a
    -- For @0 < i <= n@, the variable @X i@ in @es@ stands for @C_i(x)/x^k@,
    -- and @X (i+n)@ for @C_i'(x)/x^(k-1)@, where @k@ is the order of @C_i(x)@.
    es = fmap cToCOverXn (c ++ c')
    cToCOverXn (X i, e) = (X i, substAndDivide order (order i) e)
    cToCOverXn _ = error "This doesn't happen."
    -- Use the values of @C_i(x)@ at @x=0@ as the initial vector for the search.
    initialGuess = vector (1 : fmap (\(X i, _) -> lCoef i) es)
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
-}

(.>) :: Functor f => f a -> (a -> b) -> f b
(.>) = flip fmap

(#!) :: (?loc :: CallStack, Eq k, _)
  => HashMap k v -> k -> v
h #! k = HashMap.lookupDefault e k h
  where e = error ("Data.HashMap.(!): key not found\n" ++ showCallStack ?loc)

-- | Type annotation. The produced value should not be evaluated.
ofType :: (?loc :: CallStack) => (b -> a) -> b
ofType _ = error
  ("ofType: this should not be evaluated.\n" ++ showCallStack ?loc)
