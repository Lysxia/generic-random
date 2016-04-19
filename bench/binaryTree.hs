{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Data
import GHC.Generics
import Data.Random.Generics
import Data.Random.Generics.Internal
import Test.QuickCheck

data T = N T T | L
  deriving (Eq, Ord, Show, Data, Generic)

instance NFData T

-- size
s :: T -> Integer
s (N l r) = 1 + s l + s r
s L = 1

rejectT :: Int -> Gen T
rejectT = generator asGen

rejectSimpleT :: Int -> Gen T
rejectSimpleT = simpleGenerator' asGen

-- Pointing makes the generator more precise.
pointT :: Int -> Gen T
pointT = pointedGenerator asGen

pointRejectT :: Int -> Gen T
pointRejectT size =
  generator_ asGen 1 (Just size) (tolerance epsilon size)

main = defaultMain $
  [5 .. 10] >>= \e ->
    let n = 2 ^ e in
    [ bench ("reject " ++ show n) $
        nfGen (rejectT n)
    , bench ("reject-simple " ++ show n) $
        nfGen (rejectSimpleT n)
    , bench ("point " ++ show n) $
        nfGen (pointT n)
    , bench ("point-reject " ++ show n) $
        nfGen (pointRejectT n)
    ]

nfGen :: NFData a => Gen a -> Benchmarkable
nfGen = nfIO . generate
