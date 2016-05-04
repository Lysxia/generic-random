{-# LANGUAGE RecordWildCards #-}
module Data.Random.Generics.Internal where

import Control.Arrow ( (&&&) )
import Control.Applicative
import Data.Data
import qualified Data.HashMap.Lazy as HashMap
import Data.Random.Generics.Internal.Oracle
import Data.Random.Generics.Internal.Types

-- | Sized generator.
data SG r = SG
  { minSize :: Size
  , maxSizeM :: Maybe Size
  , runSG :: Int -> Maybe Double -> r
  }

rangeSG :: SG r -> (Size, Maybe Size)
rangeSG = minSize &&& maxSizeM

-- * Helper functions

makeGenerator :: (Data a, MonadRandomLike m)
  => [Alias m] -> proxy a -> SG (m a)
makeGenerator aliases a =
  SG minSize maxSizeM makeGenerator'
  where
    dd = collectTypes aliases a
    t = typeRep a
    i = case index dd #! t of
      Left j -> fst (xedni' dd #! j)
      Right i -> i
    minSize = natToInt $ fst (lTerm dd #! i)
    maxSizeM = HashMap.lookup i (degree dd)
    makeGenerator' k sizeM = getGenerator dd' generators a k
      where
        dd' = dds !! k
        oracle = makeOracle dd' t sizeM
        generators = makeGenerators dd' oracle
    dds = iterate point dd

applySG :: SG r -> Int -> Maybe Double -> r
applySG SG{..} k sizeM
  | Just minSize == maxSizeM = runSG k (fmap fromIntegral maxSizeM)
  | Just size <- sizeM, size <= fromIntegral minSize =
      error "Target size too small."
  | Just True <- liftA2 ((<=) . fromIntegral) maxSizeM sizeM =
      error "Target size too large."
  | Nothing <- sizeM, Just _ <- maxSizeM =
      error "Cannot make singular sampler for finite type."
  | otherwise = runSG k sizeM

ceiledRejectionSampler :: (Data a, MonadRandomLike m)
  => [AliasR m] -> proxy a
  -> SG ((Size, Size) -> m a)
ceiledRejectionSampler aliases a = sg
  { runSG = (fmap . fmap) (flip runRejectT) (runSG sg) }
  where
    sg = makeGenerator aliases a

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
--   'generator' delta :: 'Gen' (Either () [Bool])
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

epsilon :: Double
epsilon = 0.1

-- | > (size * (1 - epsilon), size * (1 + epsilon))
tolerance :: Double -> Int -> (Size, Size)
tolerance epsilon size = (size - delta, size + delta)
  where
    delta = ceiling (fromIntegral size * epsilon)

-- * Auxiliary definitions

-- | Boltzmann sampler, singular or with target average size, and rejecting
-- outside the tolerance interval.
--
-- Used to implement 'generator' and 'simpleGenerator''.
generator_ :: (Data a, MonadRandomLike m)
  => [AliasR m] -> Int -> Maybe Size'
  -> (Size', Size') -> m a
generator_ aliases = \k size' ->
  let size = fmap (rescale range) size' in
  runSG sg k size . rescaleInterval
  where
    sg = ceiledRejectionSampler aliases []
    range = rangeSG sg
    rescaleInterval (a', b') = (a, b)
      where
        a = floor (rescale range a')
        b | Just maxSize <- snd range, b' == 100 = maxSize
          | otherwise = ceiling (rescale range b')
