{-# LANGUAGE DeriveDataTypeable #-}
import Control.Monad
import Data.Data
import Data.Foldable
import Data.List
import Test.QuickCheck
import Data.Random.Generics

data T = N T T | L
  deriving (Eq, Ord, Show, Data)

-- size
s :: T -> Int
s (N l r) = 1 + s l + s r
s L = 0

pointRejectT' :: Int -> Gen T
pointRejectT' size = generator_ [] 1 (Just size) (tolerance epsilon size)

main =
  for_ [ 4 ^ e | e <- [2 .. 4] ] $ \n ->
    for_
      [ ("reject ", generator)
      , ("rejectSimple ", simpleGenerator')
      , ("point ", pointedGenerator')
      , ("pointReject ", pointRejectT')
      ] $ \(name, g) ->
      stats (name ++ show n) s (g n)

stats :: String -> (a -> Int) -> Gen a -> IO ()
stats s f g = do
  putStrLn s
  xs <- replicateM 1000 (fmap f (generate g))
  putStrLn $ "Mean: " ++ show (mean xs)
  pp (histogram xs)
  putStrLn ""

histogram xs' = (bounds, bins)
  where
    (xs, ys) = splitAt (95 * length xs' `div` 100) (sort xs')
    xMin = minimum xs
    xMax = maximum xs
    bounds
      | xMax - xMin < 20 = [xMin .. xMax]
      | otherwise = [xMin, xMin + (xMax - xMin) `div` 10 .. xMax]
    bins = f bounds xs
    f (_ : b1 : bs) xs =
      let (a, ys) = span (< b1) xs
      in length a : f (b1 : bs) ys
    f _ xs = [length xs + length ys]

pp :: ([Int], [Int]) -> IO ()
pp (vs, bs) = do
  putStrLn $ vs >>= \v -> three v ++ " - "
  putStrLn $ bs >>= \b -> " | " ++ three b

three x = replicate (3 - length s) ' ' ++ s
  where
    s = show x

mean :: Foldable v => v Int -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
