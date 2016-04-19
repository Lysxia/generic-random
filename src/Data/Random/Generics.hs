-- | Generic Boltzmann samplers.
module Data.Random.Generics where

import Data.Bifunctor
import Data.Data
import Data.Foldable
import Data.Maybe
import Control.Monad.Random
import Test.QuickCheck
import Data.Random.Generics.Internal
import Data.Random.Generics.Boltzmann.Oracle

-- * Main functions

-- | Singular ceiled rejection sampler.
--
-- It works with recursive tree-like structures, as opposed to (lists of)
-- structures with bounded size. It has the advantage of using the same oracle
-- for all sizes, which will be computed before passing the size argument.
-- Hence this is the most convenient function to get generators with parametric
-- size:
--
-- @
--   instance Arbitrary MyT where
--     arbitrary = sized (generator asGen)
-- @
generator :: (Data a, Monad m) => PrimRandom m -> Size -> m a
generator primRandom = generator_ primRandom 0 Nothing . tolerance epsilon

-- | Generator of pointed values.
--
-- It usually has a flatter distribution of sizes than a simple Boltzmann
-- sampler, making it an efficient alternative to rejection sampling.

-- Oracles are computed only for sizes that are a power of two away from
-- the minimum size of the datatype @minSize + 2 ^ e@.
pointedGenerator :: (Data a, Monad m) => PrimRandom m -> Size -> m a
pointedGenerator primRandom = \size ->
  snd . fromMaybe (last generators) . find ((>= size) . fst) $ generators
  where
    ((minSize, maxSize'), makeGenerator') = makeGenerator primRandom []
    generators =
      [ (size, makeGenerator' 1 (Just (minSize + size))) | size <- sizes ]
    sizes = fmap fromInteger (takeWhile ltMaxSize pow2s) ++ [ maxSize_ ]
      where
        maxSize = fromMaybe maxBound maxSize'
        ltMaxSize = (< toInteger maxSize_)
        maxSize_ = maxSize - minSize - 1
    pow2s = [ 2 ^ e | e <- [0 :: Int ..] ]

-- * Fixed size

-- | Generator of pointed values.
pointedGenerator' :: (Data a, Monad m) => PrimRandom m -> Size -> m a
pointedGenerator' primRandom size =
  snd (makeGenerator primRandom []) 1 (Just size)

-- | Ceiled rejection sampler with given average size.
simpleGenerator' :: (Data a, Monad m) => PrimRandom m -> Size -> m a
simpleGenerator' primRandom size =
  generator_ primRandom 0 (Just size) (tolerance epsilon size)

-- * Auxiliary definitions

-- | Boltzmann sampler, singular or with target average size, and rejecting
-- outside the tolerance interval.
--
-- The target size and the tolerance interval are shifted and clamped to the actual
-- size range of the datatype.
generator_ :: (Data a, Monad m)
  => PrimRandom m -> Int -> Maybe Size -> (Size, Size) -> m a
generator_ primRandom = \k size ->
  arbitraryG' k (fmap clamp' size) . bimap clamp clamp
  where
    ((minSize, maxSize'), arbitraryG') = ceiledRejectionSampler primRandom []
    clamp x = maybe id min maxSize' (minSize + x)
    clamp' x = maybe id (min . subtract 1) maxSize' (minSize + x + 1)

-- | Dictionary for QuickCheck's 'Gen'.
asGen :: PrimRandom Gen
asGen = PrimRandom
  (return ())
  choose
  arbitrary
  arbitrary
  arbitrary

-- | Dictionary for 'MonadRandom' instances.
asMonadRandom :: MonadRandom m => PrimRandom m
asMonadRandom = PrimRandom
  (return ())
  getRandomR
  getRandom
  getRandom
  getRandom
