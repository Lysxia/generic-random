{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, ImplicitParams #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Data.Random.Generics.Internal.Oracle where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Data
import Data.Hashable ( Hashable )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe ( fromJust, isJust )
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import GHC.Generics ( Generic )
import GHC.Stack ( CallStack, showCallStack )
import Numeric.AD
import Data.Random.Generics.Internal.Types
import Data.Random.Generics.Internal.Solver

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
-- The /order/ (or /valuation/) of a power series is the index of the first
-- non-zero coefficient, called the /leading coefficient/.

data DataDef m = DataDef
  { count :: Int -- ^ Number of registered types
  , points :: Int -- ^ Number of iterations of the pointing operator
  , index :: HashMap TypeRep (Either Aliased Ix) -- ^ Map from types to indices
  , xedni :: HashMap Ix SomeData' -- ^ Inverse map from indices to types
  , xedni' :: HashMap Aliased (Ix, Alias m) -- ^ Inverse map to aliases
  , types :: HashMap C [(Integer, Constr, [C'])]
  -- ^ Structure of types and their pointings (up to 'points', initially 0)
  --
  -- Primitive types and empty types are mapped to an empty constructor list, and
  -- can be distinguished using 'Data.Data.dataTypeRep' on the 'SomeData'
  -- associated to it by 'xedni'.
  --
  -- The integer is a multiplicity which can be > 1 for pointings.
  , lTerm :: HashMap Ix (Nat, Integer)
  -- ^ Leading term @a * x ^ u@ of the generating functions @C_i[k](x)@ in the
  -- form (u, a).
  --
  -- [Order @u@] Smallest size of objects of a given type.
  -- [Leading coefficient @a@] number of objects of smallest size.
  , degree :: HashMap Ix Int
  -- ^ Degrees of the generating functions, when applicable: greatest size of
  -- objects of a given type.
  } deriving Show

-- | A pair @C i k@ represents the @k@-th "pointing" of the type at index @i@,
-- with generating function @C_i[k](x)@.
data C = C Ix Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable C

data AC = AC Aliased Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable AC

type C' = (Maybe Aliased, C)

newtype Aliased = Aliased Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable Aliased

type Ix = Int

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

instance Monoid Nat where
  mempty = Zero
  mappend (Succ n) = Succ . mappend n
  mappend Zero = id

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

infinity :: Nat
infinity = Succ infinity

dataDef :: [Alias m] -> DataDef m
dataDef as = DataDef
  { count = 0
  , points = 0
  , index = index
  , xedni = HashMap.empty
  , xedni' = xedni'
  , types = HashMap.empty
  , lTerm = HashMap.empty
  , degree = HashMap.empty
  } where
    xedni' = HashMap.fromList (fmap (\(i, a) -> (i, (-1, a))) as')
    index = HashMap.fromList (fmap (\(i, a) -> (ofType a, Left i)) as')
    as' = zip (fmap Aliased [0 ..]) as
    ofType (Alias f) = typeRep (f undefined)

-- | Find all types that may be types of subterms of a value of type @a@.
--
-- This will loop if there are infinitely many such types.
collectTypes :: Data a => [Alias m] -> proxy a -> DataDef m
collectTypes as a = collectTypesM a `execState` dataDef as

-- | Primitive datatypes have @C(x) = x@: they are considered as
-- having a single object (@lCoef@) of size 1 (@order@)).
primOrder :: Int
primOrder = 1

primOrder' :: Nat
primOrder' = Succ Zero

primlCoef :: Integer
primlCoef = 1

-- | The type of the first argument of @Data.Data.gunfold@.
type GUnfold m = forall b r. Data b => m (b -> r) -> m r

collectTypesM :: Data a => proxy a
  -> State (DataDef m) (Either Aliased Ix, ((Nat, Integer), Maybe Int))
collectTypesM a = chaseType a (const id)

chaseType :: Data a => proxy a
  -> ((Maybe (Alias m), Ix) -> DataDef m -> DataDef m)
  -> State (DataDef m) (Either Aliased Ix, ((Nat, Integer), Maybe Int))
chaseType a k = do
  let t = typeRep a
  DataDef{..} <- get
  let
    lookup i r =
      let
        lTerm_i = lTerm #! i
        degree_i = HashMap.lookup i degree
      in return (r, (lTerm_i, degree_i))
  case HashMap.lookup t index of
    Nothing -> do
      let i = count
      modify $ \dd -> k (Nothing, i) dd
        { count = i + 1
        , index = HashMap.insert t (Right i) index
        , xedni = HashMap.insert i (someData' a) xedni
        }
      traverseType a i -- Updates lTerm and degree
    Just (Right i) -> lookup i (Right i)
    Just (Left j) ->
      case xedni' #! j of
        (-1, Alias f) -> do
          (_, ld) <- chaseType (ofType f) $ \(alias, i) ->
            let
              alias' = case alias of
                Nothing -> Alias f
                Just (Alias g) -> Alias (composeCastM f g)
            in
            k (Just alias', i) . \dd -> dd
              { xedni' = HashMap.insert j (i, alias') xedni' }
          return (Left j, ld)
        (i, _) -> lookup i (Left j)
  where
    ofType :: (m a -> m b) -> m a
    ofType _ = undefined

-- | Traversal of the definition of a datatype.
traverseType
  :: Data a => proxy a -> Ix
  -> State (DataDef m) (Either Aliased Ix, ((Nat, Integer), Maybe Int))
traverseType a i = do
  let d = withProxy dataTypeOf a
  mfix $ \ ~(_, (lTerm_i0, _)) -> do
    modify $ \dd@DataDef{..} -> dd
      { lTerm = HashMap.insert i lTerm_i0 lTerm
      }
    (types_i, ld@(_, degree_i)) <- traverseType' a d
    modify $ \dd@DataDef{..} -> dd
      { types = HashMap.insert (C i 0) types_i types
      , degree = maybe id (HashMap.insert i) degree_i degree
      }
    return (Right i, ld)

traverseType'
  :: Data a => proxy a -> DataType
  -> State (DataDef m)
      ([(Integer, Constr, [(Maybe Aliased, C)])], ((Nat, Integer), Maybe Int))
traverseType' a d | isAlgType d = do
  let
    constrs = dataTypeConstrs d
    collect
      :: GUnfold (StateT
        ([Either Aliased Ix], (Nat, Integer), Maybe Int)
        (State (DataDef m)))
    collect mkCon = do
      f <- mkCon
      let ofType :: (b -> a) -> Proxy b
          ofType _ = Proxy
          b = ofType f
      (j, (lTerm_, degree_)) <- lift (collectTypesM b)
      modify $ \(js, lTerm', degree') ->
        (j : js, lMul lTerm_ lTerm', liftA2 (+) degree_ degree')
      return (withProxy f b)
  tlds <- forM constrs $ \constr -> do
    (js, lTerm', degree') <-
      gunfold collect return constr `proxyType` a
        `execStateT` ([], (Zero, 1), Just 1)
    dd <- get
    let
      c (Left j) = (Just j, C (fst (xedni' dd #! j)) 0)
      c (Right i) = (Nothing, C i 0)
    return ((1, constr, [ c j | j <- js]), lTerm', degree')
  let
    (types_i, ls, ds) = unzip3 tlds
    lTerm_i = first Succ (lSum ls)
    degree_i = maxDegree ds
  return (types_i, (lTerm_i, degree_i))
traverseType' _ _ =
  return ([], ((primOrder', primlCoef), Just primOrder))

-- | If @(u, a)@ represents a power series of leading term @a * x ^ u@, and
-- similarly for @(u', a')@, this finds the leading term of their sum.
lPlus :: (Nat, Integer) -> (Nat, Integer) -> (Nat, Integer)
lPlus ol@(order, lCoef) ol'@(order', lCoef')
  | order < order' = ol
  | order > order' = ol'
  | otherwise = (order, lCoef + lCoef')

-- | Sum of a list of series.
lSum :: [(Nat, Integer)] -> (Nat, Integer)
lSum [] = (infinity, 0)
lSum ls = foldl1 lPlus ls

-- | Leading term of a product of series.
lMul :: (Nat, Integer) -> (Nat, Integer) -> (Nat, Integer)
lMul (order, lCoef) (order', lCoef') = (order <> order', lCoef * lCoef')

lProd :: [(Nat, Integer)] -> (Nat, Integer)
lProd = foldl lMul (Zero, 1)

maxDegree :: [Maybe Int] -> Maybe Int
maxDegree = foldl (liftA2 max) (Just minBound)

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

point :: DataDef m -> DataDef m
point dd@DataDef{..} = dd
  { points = points'
  , types = foldl g types [0 .. count-1]
  } where
    points' = points + 1
    g types i = HashMap.insert (C i points') (types' i) types
    types' i = types #! C i 0 >>= h
    h (_, constr, js) = do
      ps <- partitions points' (length js)
      let
        mult = multinomial points' ps
        js' = zipWith (\(j', C i _) p -> (j', C i p)) js ps
      return (mult, constr, js')

-- | An oracle gives the values of the generating functions at some @x@.
type Oracle = HashMap C Double

-- | Find the value of @x@ such that the average size of the generator
-- for the @k-1@-th pointing is equal to @size@, and produce the associated
-- oracle. If the size is @Nothing@, find the radius of convergence.
--
-- The search evaluates the generating functions for some values of @x@ in
-- order to run a binary search. The evaluator is implemented using Newton's
-- method, the convergence of which has been shown for relevant systems in
-- /Boltzmann Oracle for Combinatorial Systems/,
-- C. Pivoteau, B. Salvy, M. Soria.
makeOracle :: DataDef m -> TypeRep -> Maybe Double -> Oracle
makeOracle dd0 t size' =
  seq v
  HashMap.fromList (zip cs (S.toList v))
  where
    dd@DataDef{..} = if isJust size' then point dd0 else dd0
    cs = flip C <$> [0 .. points] <*> [0 .. count - 1]
    m = count * (points + 1)
    k = points - 1
    i = case index #! t of
      Left j -> fst (xedni' #! j)
      Right i -> i
    checkSize (Just size) (Just ys) = size >= size_
      where
        size_ = ys S.! j' / ys S.! j
        j = dd ? C i k
        j' = dd ? C i (k + 1)
    checkSize Nothing (Just _) = True
    checkSize _ Nothing = False
    -- Equations defining C_i(x) for all types with indices i
    phis :: Num a => V.Vector (a -> V.Vector a -> a)
    phis = V.fromList [ phi dd c (types #! c) | c <- listCs dd ]
    eval' x = fixedPoint defSolveArgs phi' (S.replicate m 0)
      where
        phi' :: (Mode a, Scalar a ~ Double) => V.Vector a -> V.Vector a
        phi' y = fmap (\f -> f (auto x) y) phis
    v = fromJust (search eval' (checkSize size'))

-- | Generating function definition. This defines a @Phi_i[k]@ function
-- associated with the @k@-th pointing of the type at index @i@, such that:
--
-- > C_i[k](x)
-- >   = Phi_i[k](x, C_0[0](x), ..., C_(n-1)[0](x),
-- >              ..., C_0[k](x), ..., C_(n-1)[k](x))
--
-- Primitive datatypes have @C(x) = x@: they are considered as
-- having a single object ('lCoef') of size 1 ('order')).
phi :: Num a => DataDef m -> C -> [(Integer, constr, [C'])]
  -> a -> V.Vector a -> a
phi DataDef{..} (C i _) [] =
  case xedni #! i of
    SomeData a ->
      case (dataTypeRep . withProxy dataTypeOf) a of
        AlgRep _ -> \_ _ -> 0
        _ -> \x _ -> fromInteger primlCoef * x ^ primOrder
phi dd@DataDef{..} _ tyInfo = f
  where
    f x y = x * (sum . fmap (toProd y)) tyInfo
    toProd y (w, _, js) =
      fromInteger w * product [ y V.! (dd ? j) | (_, j) <- js ]

-- | Maps a key representing a type @a@ (or one of its pointings) to a
-- generator @m a@.
type Generators m = (HashMap AC (SomeData m), HashMap C (SomeData m))

-- | Basic component of random generators.
--
-- This makes the implementation abstract over both 'MonadRandom' instances and
-- 'Test.QuickCheck.Gen'.
data PrimRandom m = PrimRandom
  { incr :: m () -- Called for every constructor
  -- Generators in @[0, upperBound)@
  , doubleR :: Double -> m Double
  , integerR :: Integer -> m Integer
  , int :: m Int
  , double :: m Double
  , char :: m Char
  }

-- | Build all involved generators at once.
makeGenerators
  :: forall m. Monad m
  => DataDef m -> Oracle -> PrimRandom m -> Generators m
makeGenerators DataDef{..} oracle pr@PrimRandom{..} =
  seq oracle
  (generatorsL, generatorsR)
  where
    f (C i _) tyInfo = case xedni #! i of
      SomeData a -> SomeData $ incr >>
        case tyInfo of
          [] -> defGen pr
          _ -> frequencyWith doubleR (fmap g tyInfo) `proxyType` a
    g :: Data a => (Integer, Constr, [C']) -> (Double, m a)
    g (v, constr, js) =
      ( fromInteger v * w
      , gunfold generate return constr `runReaderT` gs)
      where
        gs = fmap (\(j', i) -> m j' i) js
        m = maybe (generatorsR #!) m'
        m' j (C _ k) = (generatorsL #! AC j k)
        w = product $ fmap ((oracle #!) . snd) js
    h (j, (i, Alias f)) k =
      (AC j k, applyCast f (generatorsR #! C i k))
    generatorsL = HashMap.fromList (liftA2 h (HashMap.toList xedni') [0 .. points])
    generatorsR = HashMap.mapWithKey f types

type SmallGenerators m =
  (HashMap Aliased (SomeData m), HashMap Ix (SomeData m))

-- | Generators of values of minimal sizes.
smallGenerators
  :: forall m. Monad m => DataDef m -> PrimRandom m -> SmallGenerators m
smallGenerators DataDef{..} pr@PrimRandom{..} = (generatorsL, generatorsR)
  where
    f i (SomeData a) = SomeData $ incr >>
      case types #! C i 0 of
        [] -> defGen pr
        tyInfo ->
          let gs = (tyInfo >>= g (fst (lTerm #! i))) in
          frequencyWith integerR gs `proxyType` a
    g :: Data a => Nat -> (Integer, Constr, [C']) -> [(Integer, m a)]
    g minSize (_, constr, js) =
      guard (minSize == Succ size) *>
      [(weight, gunfold generate return constr `runReaderT` gs)]
      where
        (size, weight) = lProd [ lTerm #! i | (_, C i _) <- js ]
        gs = fmap lookup js
        lookup (j', C i _) = maybe (generatorsR #! i) (generatorsL #!) j'
    h (j, (i, Alias f)) = (j, applyCast f (generatorsR #! i))
    generatorsL = (HashMap.fromList . fmap h . HashMap.toList) xedni'
    generatorsR = HashMap.mapWithKey f xedni

generate :: Applicative m => GUnfold (ReaderT [SomeData m] m)
generate rest = ReaderT $ \(g : gs) ->
  rest `runReaderT` gs <*> unSomeData g

defGen :: (Data a, Monad m) => PrimRandom m -> m a
defGen PrimRandom{..} = gen
  where
    gen =
      let dt = withProxy dataTypeOf gen in
      case dataTypeRep dt of
        IntRep -> fromConstr . mkIntegralConstr dt <$> int
        FloatRep -> fromConstr . mkRealConstr dt <$> double
        CharRep -> fromConstr . mkCharConstr dt <$> char
        AlgRep _ -> error "Cannot generate for empty type."
        NoRep -> error "No representation."

-- * Short operators

(?) :: DataDef m -> C -> Int
dd ? C i k = i + k * count dd

-- | > dd ? (listCs dd !! i) = i
listCs :: DataDef m -> [C]
listCs dd = liftA2 (flip C) [0 .. points dd] [0 .. count dd - 1]

ix :: C -> Int
ix (C i _) = i

-- | > dd ? (dd ?! i) = i
(?!) :: DataDef m -> Int -> C
dd ?! j = C i k
  where (k, i) = j `divMod` count dd

getGenerator :: (Functor m, Data a)
  => DataDef m -> Generators m -> proxy a -> Int -> m a
getGenerator dd (l, r) a k = unSomeData $
  case index dd #! typeRep a of
    Right i -> (r #! C i k)
    Left j -> (l #! AC j k)

getSmallGenerator :: (Functor m, Data a)
  => DataDef m -> SmallGenerators m -> proxy a -> m a
getSmallGenerator dd (l, r) a = unSomeData $
  case index dd #! typeRep a of
    Right i -> (r #! i)
    Left j -> (l #! j)

-- * General helper functions

frequencyWith
  :: (Show r, Ord r, Num r, Monad m) => (r -> m r) -> [(r, m a)] -> m a
frequencyWith _ [(_, a)] = a
frequencyWith randomR as = randomR total >>= select as
  where
    total = (sum . fmap fst) as
    select ((w, a) : as) x
      | x < w = a
      | otherwise = select as (x - w)
    select _ _ = (snd . head) as
    -- That should not happen in theory, but floating point might be funny.

(#!) :: (?loc :: CallStack, Eq k, Hashable k)
  => HashMap k v -> k -> v
h #! k = HashMap.lookupDefault (e ?loc) k h
  where
    e loc = error ("HashMap.(!): key not found\n" ++ showCallStack loc)

-- | @partitions k n@: lists of non-negative integers of length @n@ with sum
-- less than or equal to @k@.
partitions :: Int -> Int -> [[Int]]
partitions _ 0 = [[]]
partitions k n = do
  p <- [0 .. k]
  (p :) <$> partitions (k - p) (n - 1)

-- | Multinomial coefficient.
--
-- > multinomial n ps == factorial n `div` product [factorial p | p <- ps]
multinomial :: Int -> [Int] -> Integer
multinomial _ [] = 1
multinomial n (p : ps) = binomial n p * multinomial (n - p) ps

-- | Binomial coefficient.
--
-- > binomial n k == factorial n `div` (factorial k * factorial (n-k))
binomial :: Int -> Int -> Integer
binomial = \n k -> pascal !! n !! k
  where
    pascal = [1] : fmap nextRow pascal
    nextRow r = zipWith (+) (0 : r) (r ++ [0])
