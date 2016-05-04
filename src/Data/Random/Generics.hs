-- | Generic Boltzmann samplers.

module Data.Random.Generics (
  Size',
  -- * Main functions
  -- $sized
  generator,
  pointedGenerator,
  -- ** Fixed size
  -- $fixed
  simpleGenerator',
  pointedGenerator',
  -- * Generators with aliases
  -- $aliases
  generatorWith,
  pointedGeneratorWith,
  -- ** Fixed size
  simpleGeneratorWith',
  pointedGeneratorWith',
  -- * Auxiliary definitions
  -- ** Dictionaries
  asGen,
  asMonadRandom,
  PrimRandom,
  -- ** Alias
  alias,
  aliasR,
  alias',
  Alias,
  AliasR,
  -- ** Internal
  generator_
  ) where

import Data.Data
import Data.Foldable
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Random
import Test.QuickCheck
import Data.Random.Generics.Internal
import Data.Random.Generics.Internal.Oracle
import Data.Random.Generics.Internal.Types

-- | The size of a value is its number of constructors.
--
-- Here, however, the 'Size'' type is interpreted differently to make better
-- use of QuickCheck's size parameter, provided by the 'Test.QuickCheck.sized'
-- combinator, so that we generate non-trivial data even at very small size
-- values.
--
-- For infinite types, with objects of unbounded sizes @> minSize@, given a
-- parameter @delta :: 'Size''@, the produced values have an average size close
-- to @minSize + delta@.
--
-- For example, values of type @Either () [Bool]@ have at least two constructors,
-- so
--
-- @
--   'generator' 'asGen' delta :: 'Gen' (Either () [Bool])
-- @
--
-- will target sizes close to @2 + delta@;
-- the offset becomes less noticeable as @delta@ grows to infinity.
--
-- For finite types with sizes in @[minSize, maxSize]@, the target
-- expected size is obtained from a 'Size'' in @[0, 99]@ by an affine mapping.
type Size' = Int

rescale :: (Int, Maybe Int) -> Size' -> Double
rescale (minSize, Just maxSize) size' =
  fromIntegral minSize + fromIntegral (min 99 size' * (maxSize - minSize)) / 100
rescale (minSize, Nothing) size' = fromIntegral (minSize + size')

-- * Main functions

-- $sized
-- These functions and their @_With@ counterparts below can be partially
-- applied to a 'PrimRandom' dictionary: the numerical /oracles/ are computed
-- once and for all, so they can be reused for different sizes.

-- | @
--   'generator' 'asGen' :: Int -> 'Gen' a
--   'generator' 'asMonadRandom' :: 'MonadRandom' m => Int -> m a
-- @
--
-- Singular ceiled rejection sampler.
--
-- This works with recursive tree-like structures, as opposed to (lists of)
-- structures with bounded size. More precisely, the generating function of the
-- given type should have a finite radius of convergence, with a singularity of
-- a certain kind (see Duchon et al., reference in the README).
--
-- This has the advantage of using the same oracle for all sizes. Hence this is
-- the most convenient function to get generators with parametric size:
--
-- @
--   instance 'Arbitrary' MyT where
--     'arbitrary' = 'sized' ('generator' 'asGen')
-- @
generator :: (Data a, Monad m) => PrimRandom m -> Size' -> m a
generator = generatorWith []

-- | @
--   'pointedGenerator' 'asGen' :: Int -> 'Gen' a
--   'pointedGenerator' 'asMonadRandom' :: 'MonadRandom' m => Int -> m a
-- @
--
-- Generator of pointed values.
--
-- It usually has a flatter distribution of sizes than a simple Boltzmann
-- sampler, making it an efficient alternative to rejection sampling.
--
-- It also works on more types, particularly lists and finite types,
-- but relies on multiple oracles.
--
-- = Pointing
--
-- The /pointing/ of a type @t@ is a derived type whose values are essentially
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
-- @
--
-- Pointed values are easily mapped back to the original type by erasing the
-- point. Pointing makes larger values occur much more frequently, while
-- preserving the uniformness of the distribution conditionally to a fixed
-- size.

-- Oracles are computed only for sizes that are a power of two away from
-- the minimum size of the datatype @minSize + 2 ^ e@.
pointedGenerator :: (Data a, Monad m) => PrimRandom m -> Size' -> m a
pointedGenerator = pointedGeneratorWith []

-- ** Fixed size

-- $fixed
-- These functions do not benefit from the same precomputation pattern as the
-- above. 'simpleGenerator'' works with slightly more types than 'generator',
-- since it doesn't require the existence of a singularity.
--
-- The overhead of computing the "oracles" (if you plan to use these
-- functions with 'sized') has not been measured yet.

-- | Generator of pointed values.
pointedGenerator' :: (Data a, Monad m) => PrimRandom m -> Size' -> m a
pointedGenerator' = pointedGeneratorWith' []

-- | Ceiled rejection sampler with given average size.
simpleGenerator' :: (Data a, Monad m) => PrimRandom m -> Size' -> m a
simpleGenerator' = simpleGeneratorWith' []

-- * Generators with aliases

-- $aliases
-- Boltzmann samplers can normally be defined only for types @a@ such that:
--
-- - they are instances of 'Data';
-- - the set of types of subterms of values of type @a@ is finite;
-- - and all of these types have at least one finite value (i.e., values with
--   finitely many constructors).
--
-- Examples of misbehaving types are:
--
-- - @a -> b -- Not Data@
-- - @data E a = L a | R (E [a]) -- Contains a, [a], [[a]], [[[a]]], etc.@
-- - @data I = C I -- No finite value@
--
-- = Alias
--
-- The 'Alias' type works around these limitations ('AliasR' for rejection
-- samplers).
-- This existential wrapper around a user-defined function @f :: a -> m b@
-- makes @generic-random@ view occurences of the type @b@ as @a@ when
-- processing a recursive system of types, possibly stopping some infinite
-- unrolling of type definitions. When a value of type @b@ needs to be
-- generated, it generates an @a@ which is passed to @f@.
--
-- @
--   let
--     as = ['aliasR' $ \\() -> return (L []) :: 'Gen' (E [[Int]])]
--   in
--     'generatorWith' as 'asGen' :: 'Size' -> 'Gen' (E Int)
-- @
--
-- Another use case is to plug in user-defined generators where the default is
-- not satisfactory, for example, to get positive @Int@s:
--
-- @
--   let
--     as = ['alias' $ \\() -> 'choose' (0, 100) :: 'Gen' Int)]
--   in
--     'pointedGeneratorWith' as 'asGen' :: 'Size' -> 'Gen' [Int]
-- @

generatorWith
  :: (Data a, Monad m) => [AliasR m] -> PrimRandom m -> Size' -> m a
generatorWith aliases primRandom =
  generator_ primRandom aliases 0 Nothing . tolerance epsilon

pointedGeneratorWith
  :: (Data a, Monad m) => [Alias m] -> PrimRandom m -> Size' -> m a
pointedGeneratorWith aliases primRandom = generators
  where
    sg = makeGenerator primRandom aliases []
    range = (minSize sg, maxSizeM sg)
    generators =
      sparseSized
        (runSG sg 1 . Just . rescale range)
        (99 <$ maxSizeM sg)

sparseSized :: (Int -> a) -> Maybe Int -> Int -> a
sparseSized f maxSizeM =
  maybe a0 snd . \size' -> find ((>= size') . fst) as
  where
    as = [ (s, f s) | s <- ss ]
    ss = 0 : maybe id (takeWhile . (>)) maxSizeM [ 2 ^ e | e <- [ 0 :: Int ..] ]
    a0 = f (fromJust maxSizeM)

-- ** Fixed size

pointedGeneratorWith'
  :: (Data a, Monad m) => [Alias m] -> PrimRandom m -> Size' -> m a
pointedGeneratorWith' aliases primRandom size' =
  runSG sg 1 (Just (rescale (rangeSG sg) size'))
  where
    sg = makeGenerator primRandom aliases []

simpleGeneratorWith'
  :: (Data a, Monad m) => [AliasR m] -> PrimRandom m -> Size' -> m a
simpleGeneratorWith' aliases primRandom size' =
  generator_ primRandom aliases 0 (Just size') (tolerance epsilon size')

-- * Auxiliary definitions

-- | Boltzmann sampler, singular or with target average size, and rejecting
-- outside the tolerance interval.
--
-- Used to implement 'generator' and 'simpleGenerator''.
generator_ :: (Data a, Monad m)
  => PrimRandom m -> [AliasR m] -> Int -> Maybe Size'
  -> (Size', Size') -> m a
generator_ primRandom aliases = \k size' ->
  let size = fmap (rescale range) size' in
  runSG sg k size . rescaleInterval
  where
    sg = ceiledRejectionSampler primRandom aliases []
    range = rangeSG sg
    rescaleInterval (a', b') = (a, b)
      where
        a = floor (rescale range a')
        b | Just maxSize <- snd range, b' == 100 = maxSize
          | otherwise = ceiling (rescale range b')

-- ** Dictionaries

-- $primrandom
-- @'PrimRandom' m@ is a record of basic components to build our generators
-- with, allowing the implementation to remain abstract over both the
-- 'Test.QuickCheck.Gen' type and 'MonadRandom' instances. The concrete records
-- 'asGen' and 'asMonadRandom' provide their respective specializations.

-- | Dictionary for QuickCheck's 'Gen'.
asGen :: PrimRandom Gen
asGen = PrimRandom
  (return ())
  (\x -> choose (0, x))
  (\x -> choose (0, x-1))
  arbitrary
  arbitrary
  arbitrary

-- | Dictionary for 'MonadRandom' instances.
asMonadRandom :: MonadRandom m => PrimRandom m
asMonadRandom = PrimRandom
  (return ())
  (\x -> getRandomR (0, x))
  (\x -> getRandomR (0, x-1))
  getRandom
  getRandom
  getRandom

-- ** Aliases

-- | Main constructor for 'Alias'.
alias :: (Monad m, Data a, Data b) => (a -> m b) -> Alias m
alias = Alias . (=<<)

-- | Main constructor for 'AliasR'.
aliasR :: (Monad m, Data a, Data b) => (a -> m b) -> AliasR m
aliasR = Alias . (=<<) . fmap (lift . lift . lift)

-- | The true and more general form of 'Alias'.
alias' :: (Data a, Data b) => (m a -> m b) -> Alias m
alias' = Alias
