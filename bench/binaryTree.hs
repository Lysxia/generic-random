{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Exception ( evaluate )
import Data.Random.Generics
import Data.Random.Generics.Internal
import Data.Random.Generics.Internal.Types

data T = N T T | L
  deriving (Eq, Ord, Show, Data, Generic)

instance NFData T

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

main = getGs >>= \gs -> defaultMain $ liftA2 (\n f -> f n gs)
  [4 ^ e | e <- [1 .. 5]]

  -- Singular rejection sampling
  [ bg "handwritten1" gen1
  , bg "handwritten2" gen2
  , bg "SR" generatorSR

  -- Sized rejection sampling
  , bg "R" generatorR'

  -- Sized rejection sampling, not memoizing oracle
  , bg' "R-recomp" generatorR'

  -- Pointed generator
  , bg "P" generatorP'

  -- Pointed generator with rejection sampling
  , bg "PR" generatorPR'

  -- Pointed generator, not memoizing oracle
  , bg' "P-recomp" generatorP'
  ]

bg, bg' :: String -> (Int -> Gen T) -> Int -> [QCGen] -> Benchmark
bg name gen n gs =
  bench (name ++ "_" ++ show n) $
    nf (fmap (\g -> unGen gg g 0)) gs
  where
    gg = gen n

bg' name gen n gs =
  bench (name ++ "_" ++ show n) $
    nf (fmap (\(n, g) -> unGen (gen n) g 0)) (fmap ((,) n) gs)

getGs :: IO [QCGen]
getGs = replicateM 100 newQCGen
