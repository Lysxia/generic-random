{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Data
import GHC.Generics
import Test.QuickCheck
import Control.Exception ( evaluate )
import Data.Random.Generics

data T = N T T | L
  deriving (Eq, Ord, Show, Data, Generic)

instance NFData T

pointRejectT' size = generator_ [] 1 (Just size) (tolerance epsilon size)

main = defaultMain $
  [4 ^ e | e <- [1 .. 5]] >>= \n ->
    -- Singular rejection sampling
    [ bench ("reject " ++ show n) $
        nfGen (generator n)

    -- Sized rejection sampling
    , bench ("reject-simple " ++ show n) $
        nfGen (simpleGenerator' n)

    -- Sized rejection sampling, not memoizing oracle
    , bench ("reject-simple-recomp " ++ show n) $
        nfIO (evaluate n >>= generateT . simpleGenerator')

    -- Pointed generator (default)
    , bench ("point " ++ show n) $
        nfGen (pointedGenerator n)

    -- Pointed generator with rejection sampling
    , bench ("point-reject " ++ show n) $
        nfGen (pointRejectT' n)

    -- Pointed generator, memoizing oracle
    , bench ("point-norecomp " ++ show n) $
        nfGen (pointedGenerator' n)

    -- Pointed generator, not memoizing oracle
    , bench ("point-recomp " ++ show n) $
        nfIO (evaluate n >>= generateT . pointedGenerator')
    ]

nfGen :: Gen T -> Benchmarkable
nfGen = nfIO . generateT

generateT :: Gen T -> IO T
generateT = generate
