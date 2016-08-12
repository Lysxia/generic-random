-- | General helper functions

module Generic.Random.Internal.Common where

frequencyWith
  :: (Ord r, Num r, Monad m) => (r -> m r) -> [(r, m a)] -> m a
frequencyWith _ [(_, a)] = a
frequencyWith randomR as = randomR total >>= select as
  where
    total = (sum . fmap fst) as
    select ((w, a) : as) x
      | x < w = a
      | otherwise = select as (x - w)
    select _ _ = (snd . head) as
    -- That should not happen in theory, but floating point might be funny.

-- | @partitions k n@: lists of non-negative integers of length @n@ with sum
-- less than or equal to @k@.
partitions :: Int -> Int -> [[Int]]
partitions _ 0 = [[]]
partitions k n = do
  p <- [0 .. k]
  (p :) <$> partitions (k - p) (n - 1)

-- | Binomial coefficient.
--
-- > binomial n k == factorial n `div` (factorial k * factorial (n-k))
binomial :: Int -> Int -> Integer
binomial = \n k -> pascal !! n !! k
  where
    pascal = [1] : fmap nextRow pascal
    nextRow r = zipWith (+) (0 : r) (r ++ [0])

-- | Multinomial coefficient.
--
-- > multinomial n ps == factorial n `div` product [factorial p | p <- ps]
multinomial :: Int -> [Int] -> Integer
multinomial _ [] = 1
multinomial n (p : ps) = binomial n p * multinomial (n - p) ps
