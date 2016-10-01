{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Bool
import Data.Data
import Data.Functor
import GHC.Generics
import Control.DeepSeq
import Criterion.Main
import Test.Feat
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Exception ( evaluate )
import Generic.Random.Data
import Generic.Random.Internal.Data
import Generic.Random.Internal.Types

data T = N T T | L
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData T

deriveEnumerable ''T

size :: Num a => T -> a
size L = 1
size (N l r) = 1 + size l + size r

gen1 :: Int -> Gen T
gen1 n = runRejectT (tolerance epsilon (n + 1)) gen'
  where
    gen' = incr >> lift arbitrary >>= bool (return L) (liftA2 N gen' gen')

gen2 :: Int -> Gen T
gen2 n = g
  where
    (minSize, maxSize) = tolerance epsilon (n + 1)
    g = gen' 0 (\m t -> if m < minSize then g else return t)
    gen' n k | n >= maxSize = g
    gen' n k =
      arbitrary >>= bool
        (k (n+1) L)
        (gen' (n+1) $ \m l -> gen' m $ \m r -> k m (N l r))

genFeat :: Int -> Gen T
genFeat = uniform

main = newQCGen >>= \g -> defaultMain $ liftA2 (\n f -> f n g)
  [4 ^ e | e <- [1 .. 6]]

  -- Singular rejection sampling
  [ bg "handwritten1" gen1
  , bg "handwritten2" gen2

  , bg "feat" genFeat

  -- Pointed generator
  , bg "P" generatorP'

  -- Pointed generator with rejection sampling
  , bg "PR" generatorPR'

  , bg "SR" generatorSR

  -- Sized rejection sampling
  , bg "R" generatorR'

  -- Sized rejection sampling, not memoizing oracle
  , bg' "R-recomp" generatorR'

  -- Pointed generator, not memoizing oracle
  , bg' "P-recomp" generatorP'
  ]

bg, bg' :: String -> (Int -> Gen T) -> Int -> QCGen -> Benchmark
bg name gen n g =
  bench (name ++ "_" ++ show n) $ nf f g
  where
    go 0 = return (0 :: Int)
    go k = liftA2 (\t s -> size t + s) gg (go (k-1))
    gg = gen n
    f g = unGen (go 100) g 0

bg' name gen n g =
  bench (name ++ "_" ++ show n) $ nf f (n, g)
  where
    go n 0 = return (0 :: Int)
    go n k = liftA2 (\t s -> size t + s) (gen n) (go n (k-1))
    f (n, g) = unGen (go n 100) g 0

avgSize :: [T] -> Double
avgSize ts = sum (fmap size ts) / fromIntegral (length ts)
