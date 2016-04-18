{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Criterion.Main
import Data.Data
import Data.Random.Generics
import Data.Random.Generics.Internal
import Test.QuickCheck

data T = N T T | L
  deriving (Eq, Ord, Show, Data)

-- size
s :: T -> Integer
s (N l r) = 1 + s l + s r
s L = 1

-- Rejection is more elementary and works fine too.
rejectT :: Int -> Gen T
rejectT n = arbitraryGenerator n

-- Pointing makes the generator more precise.
pointT :: Int -> Gen T
pointT n = arbitraryApproxGenerator [] 1 (Just n) (Just (tolerance epsilon n))

benchPoint, benchReject :: Int -> Benchmark
benchPoint n = bench ("point " ++ show n) $ whnfGen (pointT n)
benchReject n = bench ("reject " ++ show n) $ whnfGen (rejectT n)

main = defaultMain $
  [5 .. 10] >>= \e ->
    [ benchReject (2 ^ e)
    , benchPoint (2 ^ e)
    ]

whnfGen :: Gen a -> Benchmarkable
whnfGen = whnfIO . generate
