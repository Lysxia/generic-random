{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, ImplicitParams #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Data.Random.Generics.Boltzmann.Oracle where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.AEq ( (~==) )
import Data.Data
import Data.Hashable ( Hashable )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe ( fromJust )
import qualified Data.Vector.Storable as V
import GHC.Generics ( Generic )
import GHC.Prim ( Any )
import GHC.Stack ( CallStack, showCallStack )
import Unsafe.Coerce
import Data.Random.Generics.Boltzmann.Solver

data SomeData where
  SomeData :: Data a => a -> SomeData

-- | Dummy instance for debugging.
instance Show SomeData where
  show _ = "_"

-- | We build a dictionary which reifies type information in order to
-- create a Boltzmann generator.
--
-- We denote by @n@ (or 'count') the number of types in the dictionary.
--
-- Every type has an index @0 <= i < n@; the variable @X i@ represents its
-- generating function @C_i(x)@, and @X (i + k*n)@ the GF of its @k@-th
-- "pointing" @C_i[k](x)@; we have
--
-- @
--   C_i[0](x) = C_i(x)
--   C_i[k+1](x) = x * C_i[k]'(x)
-- @
--
-- where @C_i[k]'@ is the derivative of @C_i[k]@. See also 'point'.
--
-- @X (-1)@ is the parameter @x@ of the generating functions.
--
-- The /order/ (or /valuation/) of a power series is the index of the first
-- non-zero coefficient, called the /leading coefficient/.

data DataDef = DataDef
  { count :: Int -- ^ Number of registered types
  , points :: Int -- ^ Number of iterations of the pointing operator
  , index :: HashMap TypeRep Int -- ^ Map from types to indices
  , xedni :: HashMap Int SomeData -- ^ Inverse map from indices to types
  , types :: HashMap C [(Integer, Constr, [C])]
  -- ^ Structure of types and their pointings (up to 'points', initially 0)
  --
  -- Primitive types and empty types are mapped to an empty constructor list, and
  -- can be distinguished using 'Data.Data.dataTypeRep' on the 'SomeData'
  -- associated to it by 'xedni'.
  --
  -- The integer is a multiplicity which can be > 1 for pointings.
  , order :: HashMap Int Int
  -- ^ Orders of the generating functions @C_i[k](x)@: smallest size of
  -- objects of a given type.
  , lCoef :: HashMap C Integer
  -- ^ Leading coefficients: number of objects of smallest size.
  } deriving Show

-- | A pair @C i k@ represents the @k@-th "pointing" of the type at index @i@,
-- with generating function @C_i[k](x)@.
data C = C Int Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable C

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

-- | Primitive datatypes have @C(x) = x@: they are considered as
-- having a single object ('lCoef') of size 1 ('order')).
primOrder :: Int
primOrder = 1

primlCoef :: Integer
primlCoef = 1

primExp :: (Eq a, Num a) => Exp a
primExp = fromInteger primlCoef * X 0 ^ primOrder

-- | The type of the first argument of @Data.Data.gunfold@.
type GUnfold m = forall b r. Data b => m (b -> r) -> m r

collectTypesM :: Data a => a -> State DataDef (Int, Int, Integer)
collectTypesM a = do
  let t = typeRep [a]
  DataDef{..} <- get
  case HashMap.lookup t index of
    Nothing -> do
      let i = count
      modify $ \dd -> dd
        { count = i + 1
        , index = HashMap.insert t i index
        , xedni = HashMap.insert i (SomeData a) xedni
        , order = HashMap.insert i (error "Unknown order") order
        , lCoef = HashMap.insert (C i 0) 0 lCoef }
      collectTypesM' a i -- Updates order and lCoef
    Just i ->
      let
        order_i = order #! i
        lCoef_i = lCoef #! C i 0
      in return (i, order_i, lCoef_i)

-- | Traversal of the definition of a datatype.
collectTypesM'
  :: Data a => a -> Int -> State DataDef (Int, Int, Integer)
collectTypesM' a i = do
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
        return ((1, constr, [ C j 0 | j <- js]), (order', lead'))
      let
        (types_i, ols) = unzip cjols
        (order_i, lCoef_i) = minSum ols
      return (types_i, order_i, lCoef_i)
    else
      return ([], primOrder, primlCoef)
  modify $ \dd@DataDef{..} -> dd
    { types = HashMap.insert (C i 0) types_i types
    , order = HashMap.insert i order_i order
    , lCoef = HashMap.insert (C i 0) lCoef_i lCoef
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

-- | Pointing operator.
--
-- Populates a 'DataDef' with one more level of pointings.
-- ('collectTypes' produces a dictionary at level 0.)
--
-- The "pointing" of a type @t@ is a derived type whose values are essentially
-- values of type @t@, with one of their constructors being "pointed".
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
--   -- Notice that the "points" introduced by both applications of pointing
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
-- (@p_0@ are assigned to the constructor @C@ itself, and for @i > 0@, @p_i@
-- points are assigned within the @i@-th subterm), the multiplicity of the
-- constructor paired with that distribution is the multinomial coefficient
-- @multinomial k [p_1, ..., p_c]@.

point :: DataDef -> DataDef
point dd@DataDef{..} = dd
  { points = points'
  , types = foldl g types [0 .. count-1]
  , lCoef = foldl f lCoef [0 .. count-1]
  } where
    points' = points + 1
    f lCoef i = HashMap.insert (C i points') (lCoef' i) lCoef
    lCoef' i = (lCoef #! C i points) * toInteger (order #! i)
    g types i = HashMap.insert (C i points') (types' i) types
    types' i = types #! C i 0 >>= h
    h (_, constr, js) = do
      ps <- partitions points' (length js)
      let
        mult = multinomial points' ps
        js' = zipWith (\(C j _) p -> C j p) js ps
      return (mult, constr, js')

-- | An oracle gives the values of the generating functions at some @x@.
type Oracle = HashMap C Double

-- | Find the value of @x@ such that the average size of the generator is
-- equal to @size@, and produce the associated oracle. If the size is
-- @Nothing@, find the radius of convergence.
--
-- The search evaluates the generating functions for some values of @x@ in
-- order to run a binary search. The evaluator is implemented using Newton's
-- method, the convergence of which has been shown for relevant systems in
-- /Boltzmann Oracle for Combinatorial Systems/,
-- C. Pivoteau, B. Salvy, M. Soria.
makeOracle :: DataDef -> TypeRep -> Maybe Int -> Oracle
makeOracle dd@DataDef{..} t size' =
  seq v
  HashMap.fromList (zip cs (V.toList v))
  where
    cs = flip C <$> [0 .. points] <*> [0 .. count - 1]
    m = count * (points + 1)
    k = points - 1
    i = index #! t
    checkSize (Just size) (Just ys) = fromIntegral size >= size_
      where
        size_ = ys ! j' / ys ! j
        j = dd ? C i k
        j' = dd ? C i (k + 1)
    checkSize Nothing (Just _) = True
    checkSize _ Nothing = False
    -- Equations defining C_i(x) for all types with indices i
    es = fmap (toEquation dd) (HashMap.toList types)
    eval' x = solveEquations defSolveArgs es' (V.replicate m 0)
      where
        es' = fmap (\(y := fx) -> y := subst1 (-1) x fx) es
    v = fromJust (search eval' (checkSize size'))

-- | Equation defining the generating function @C_i[k](x)@ of a type/pointing.
toEquation
  :: (Eq a, Num a) => DataDef -> (C, [(Integer, constr, [C])]) -> Equation a
toEquation dd@DataDef{..} (c@(C i _), tyInfo) =
  X (dd ? c) := rhs tyInfo
  where
    rhs [] = case xedni #! i of
      SomeData a ->
        case (dataTypeRep . dataTypeOf) a of
          AlgRep _ -> Zero
          _ -> primExp
    rhs tyInfo = X (-1) * (sum . fmap toProd) tyInfo
    toProd (w, _, js) = fromInteger w * product [ X (dd ? j) | j <- js ]

-- | Assuming @p . f@ is satisfied only for positive values in some interval
-- @(0, r]@, find @f r@.
search :: (Double -> a) -> (a -> Bool) -> a
search f p = search' e0 (0 : [2 ^ n | n <- [0 .. 100 :: Int]])
  where
    search' y (x : xs@(x' : _))
      | p y' = search' y' xs
      | otherwise = search'' y x x'
      where y' = f x'
    search' _ _ = error "Solution not found. Uncontradictable predicate?"
    search'' y x x'
      | x ~== x' = y
      | p y_ = search'' y_ x_ x'
      | otherwise = search'' y x x_
      where
        x_ = (x + x') / 2
        y_ = f x_
    e0 = error "Solution not found. Unsatisfiable predicate?"

-- | Maps a key representing a type @a@ (or one of its pointings) to a
-- generator @m a@.
type Generators m = HashMap C (m Any)

-- | Generators of random primitive values and other useful actions to
-- inject in our generator.
--
-- This allows to remain generic over 'MonadRandom' instances and
-- 'Test.QuickCheck.Gen'.
data PrimRandom m = PrimRandom
  { incr :: m () -- Called for every constructor
  , getRandomR_ :: (Double, Double) -> m Double
  , int :: m Int
  , double :: m Double
  , char :: m Char
  }

-- | Build all involved generators at once.
makeGenerators
  :: forall m. Monad m
  => DataDef -> Oracle -> PrimRandom m -> Generators m
makeGenerators DataDef{..} oracle PrimRandom{..} =
  seq oracle
  generators
  where
    f (C i _) tyInfo = incr >> case xedni #! i of
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
          _ -> frequencyWith getRandomR_ (fmap g tyInfo) `fTypeOf` a
    g :: Data a => (Integer, Constr, [C]) -> (Double, m a)
    g (v, constr, js) =
      ( fromInteger v * w
      , gunfold generate return constr `runReaderT` gs)
      where
        gs = fmap (generators #!) js
        w = product $ fmap (oracle #!) js
    generate :: GUnfold (ReaderT [m Any] m)
    generate rest = ReaderT $ \(g : gs) -> do
      x <- g
      f <- rest `runReaderT` gs
      (return . f . unsafeCoerce) x
    generators = HashMap.mapWithKey f types

-- * Short operators

(?) :: DataDef -> C -> Int
dd ? C i k = i + k * count dd

ix :: C -> Int
ix (C i _) = i

(?!) :: DataDef -> Int -> C
dd ?! j = C i k
  where (k, i) = j `divMod` count dd

getGenerator :: (Functor m, Data a)
  => DataDef -> Generators m -> proxy a -> Int -> m a
getGenerator dd generators a k =
  fmap unsafeCoerce (generators #! C (index dd #! typeRep a) k)

-- * General helper functions

-- | Type annotation. The produced value should not be evaluated.
ofType :: (?loc :: CallStack) => (b -> a) -> b
ofType _ = error
  ("ofType: this should not be evaluated.\n" ++ showCallStack ?loc)

fTypeOf :: f a -> a -> f a
fTypeOf = const

frequencyWith :: Monad m
  => ((Double, Double) -> m Double) -> [(Double, m a)] -> m a
frequencyWith getRandomR as = do
  x <- getRandomR (0, total)
  select x as
  where
    total = (sum . fmap fst) as
    select x ((w, a) : as)
      | x <= w = a
      | otherwise = select (x - w) as
    select _ _ = error "Exhausted choices."

(#!) :: (?loc :: CallStack, Eq k, Hashable k)
  => HashMap k v -> k -> v
h #! k = HashMap.lookupDefault (e ?loc) k h
  where
    e loc = error ("HashMap.(!): key not found\n" ++ showCallStack loc)

(!) :: (?loc :: CallStack, V.Storable a)
  => V.Vector a -> Int -> a
v ! i | 0 <= i && i < V.length v = v V.! i
_ ! _ = error ("Vector.(!): index out of bounds\n" ++ showCallStack ?loc)

-- | @partitions k n@: lists of non-negative integers of length @n@ with sum
-- less than or equal to @k@.
partitions :: Int -> Int -> [[Int]]
partitions _ 0 = [[]]
partitions k n = do
  p <- [0 .. k]
  (p :) <$> partitions (k - p) (n - 1)

-- | Multinomial coefficient.
multinomial :: Int -> [Int] -> Integer
multinomial _ [] = 1
multinomial n (p : ps) = binomial n p * multinomial (n - p) ps

-- | Binomial coefficient.
binomial :: Int -> Int -> Integer
binomial = \n k -> pascal !! n !! k
  where
    pascal = [1] : fmap nextRow pascal
    nextRow r = zipWith (+) (0 : r) (r ++ [0])
