module Test.Stats where

import Data.List
import Data.Maybe

import Test.Tree
import Control.Monad

mean :: Foldable v => v Int -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- | Number of samples to estimate a probability distribution on a finite set
-- of size @n@ to precision @epsilon@ (infinity-norm between distributions)
-- with probability at least @(1 - delta)@.
sampleSize
  :: Int  -- ^ Domain size
  -> Double  -- ^ Target distance (infinity-norm)
  -> Double  -- ^ Target error probability
  -> Int
sampleSize n epsilon delta =
  ceiling (log (2 * fromIntegral n / delta) / (2 * epsilon ^ 2))

-- | Number of trees with @n@ internal nodes.
catalan :: [Integer]
catalan = fmap catalan' [0 ..]
  where
    catalan' 0 = 1
    catalan' i =
      let prefix = take i catalan
      in sum $ zipWith (*) prefix (reverse prefix)

-- | Average size of a binary tree given the probability (@> 1/2@) of choosing
-- a leaf.
avgSize :: Fractional a => a -> a
avgSize p = 1 / (2 * p - 1)

-- | Inverse of 'avgSize'.
invAvgSize :: Fractional a => a -> a
invAvgSize s = (1 / s + 1) / 2

-- | Distribution of sizes (actually, @(size - 1) / 2@), given the probability
-- of choosing a leaf.
distribution :: Fractional a => a -> [a]
distribution p = zipWith f [0 ..] catalan
  where
    f i c = fromInteger c * p * (p * (1 - p)) ^ i

expected :: Fractional a => Maybe a -> (Int, Int) -> Double -> Double -> (Int, [(Int, a)])
expected avgSize' (minSize_, maxSize_) epsilon delta = (k, d)
  where
    p = maybe (1/2) invAvgSize avgSize'
    minSize = (minSize_ + 1) `div` 2
    maxSize = maxSize_ `div` 2
    n = maxSize - minSize + 1
    k = sampleSize n epsilon delta
    d_ = (take n . drop minSize . distribution) p
    d = zip [minSize ..] (fmap (/ sum d_) d_)

runExperiment
  :: (Fractional a, Ord a, Applicative m)
  => (Int, [(Int, a)]) -> m Int -> m ([(Int, a)], [(Int, a)], a)
runExperiment (k, d) gen = cmp' . collect <$> replicateM k gen
  where
    collect :: Fractional a => [Int] -> [(Int, a)]
    collect = fmap c . group . sort
    c xs@(x : _) = (x, fromIntegral (length xs) / fromIntegral k)
    c _ = undefined
    cmp' z = (d, z, cmp d z)
    cmp :: (Ord a, Num a) => [(Int, a)] -> [(Int, a)] -> a
    cmp xs ys = maximum (zipWith_ (\x y -> abs (x - y)) xs ys)
    zipWith_ :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
    zipWith_ f xxs@((x, m) : xs) yys@((y, n) : ys)
      | x == y = f m n : zipWith_ f xs ys
      | x < y = m : zipWith_ f xs yys
      | otherwise = n : zipWith_ f xxs ys
    zipWith_ f [] ys = fmap snd ys
    zipWith_ f xs [] = fmap snd xs
