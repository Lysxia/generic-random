module Main where

import Control.DeepSeq
import Criterion.Main
import Test.QuickCheck
import Control.Exception ( evaluate )
import Common

main = defaultMain $
  [4 ^ e | e <- [1 .. 5]] >>= \n ->
    -- Singular rejection sampling
    [ bench ("reject " ++ show n) $
        nfGen (rejectT n)

    -- Sized rejection sampling
    , bench ("reject-simple " ++ show n) $
        nfGen (rejectSimpleT' n)

    -- Sized rejection sampling, not memoizing oracle
    , bench ("reject-simple-recomp " ++ show n) $
        nfIO (evaluate n >>= generate . rejectSimpleT')

    -- Pointed generator (default)
    , bench ("point " ++ show n) $
        nfGen (pointT n)

    -- Pointed generator with rejection sampling
    , bench ("point-reject " ++ show n) $
        nfGen (pointRejectT' n)

    -- Pointed generator, memoizing oracle
    , bench ("point-norecomp " ++ show n) $
        nfGen (pointT' n)

    -- Pointed generator, not memoizing oracle
    , bench ("point-recomp " ++ show n) $
        nfIO (evaluate n >>= generate . pointT')
    ]

nfGen :: NFData a => Gen a -> Benchmarkable
nfGen = nfIO . generate
