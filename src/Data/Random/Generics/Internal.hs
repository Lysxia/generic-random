{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Data.Random.Generics.Internal where

import Control.Arrow ( (&&&) )
import Control.Applicative
import Data.Data
import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Lazy as HashMap
import Data.Random.Generics.Internal.Oracle
import Data.Random.Generics.Internal.Types

-- | Sized generator.
data SG r = SG
  { minSize :: Size
  , maxSizeM :: Maybe Size
  , runSG :: Int -> Maybe Double -> r
  } deriving Functor

type Points = Int

rangeSG :: SG r -> (Size, Maybe Size)
rangeSG = minSize &&& maxSizeM

applySG :: SG r -> Points -> Maybe Double -> r
applySG SG{..} k sizeM
  | Just minSize == maxSizeM = runSG k (fmap fromIntegral maxSizeM)
  | Just size <- sizeM, size <= fromIntegral minSize =
      error "Target size too small."
  | Just True <- liftA2 ((<=) . fromIntegral) maxSizeM sizeM =
      error "Target size too large."
  | Nothing <- sizeM, Just _ <- maxSizeM =
      error "Cannot make singular sampler for finite type."
  | otherwise = runSG k sizeM

-- * Helper functions

make :: (Data a, MonadRandomLike m)
  => [Alias m] -> proxy a -> SG (m a)
make aliases a =
  SG minSize maxSizeM make'
  where
    dd = collectTypes aliases a
    t = typeRep a
    i = case index dd #! t of
      Left j -> fst (xedni' dd #! j)
      Right i -> i
    minSize = natToInt $ fst (lTerm dd #! i)
    maxSizeM = HashMap.lookup i (degree dd)
    make' k sizeM = getGenerator dd' generators a k
      where
        dd' = dds !! k
        oracle = makeOracle dd' t sizeM
        generators = makeGenerators dd' oracle
    dds = iterate point dd

makeR :: (Data a, MonadRandomLike m)
  => [AliasR m] -> proxy a
  -> SG ((Size, Size) -> m a)
makeR aliases a = fmap (flip runRejectT) (make aliases a)

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

rescale :: SG r -> Size' -> Double
rescale (SG minSize (Just maxSize) _) size' =
  fromIntegral minSize + fromIntegral (min 99 size' * (maxSize - minSize)) / 100
rescale (SG minSize Nothing _) size' = fromIntegral (minSize + size')

apply :: SG r -> Points -> Maybe Size' -> r
apply sg k size' = runSG sg k (fmap (rescale sg) size')

applyR :: SG ((Size, Size) -> r) -> Points -> Maybe Size' -> (Size', Size') -> r
applyR sg k size' = apply sg k size' . rescaleInterval sg

rescaleInterval :: SG r -> (Size', Size') -> (Size, Size)
rescaleInterval sg (a', b') = (a, b)
  where
    a = (clamp . floor .rescale sg) a'
    b = (clamp . ceiling . rescale sg) b'
    clamp x
      | Just maxSize <- maxSizeM sg, x >= 100 = maxSize
      | otherwise = x

-- | > 'epsilon' = 0.1
--
-- Default approximation ratio.
epsilon :: Double
epsilon = 0.1

-- | > (size * (1 - epsilon), size * (1 + epsilon))
tolerance :: Double -> Int -> (Int, Int)
tolerance epsilon size = (size - delta, size + delta)
  where
    delta = ceiling (fromIntegral size * epsilon)

-- * Auxiliary definitions

memo
  :: (t -> [t2] -> SG r)
  -> (SG r -> t1 -> Maybe Int -> a)
  -> t -> t1 -> Int -> a
memo make apply aliases k = generators
  where
    sg = make aliases []
    generators = sparseSized (apply sg k . Just) (99 <$ maxSizeM sg)

-- Oracles are computed only for sizes that are a power of two away from
-- the minimum size of the datatype @minSize + 2 ^ e@.
sparseSized :: (Int -> a) -> Maybe Int -> Int -> a
sparseSized f maxSizeM =
  maybe a0 snd . \size' -> find ((>= size') . fst) as
  where
    as = [ (s, f s) | s <- ss ]
    ss = 0 : maybe id (takeWhile . (>)) maxSizeM [ 2 ^ e | e <- [ 0 :: Int ..] ]
    a0 = f (fromJust maxSizeM)
