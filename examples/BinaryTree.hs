{-# LANGUAGE DeriveDataTypeable #-}
module BinaryTree where

import Data.Data
import Data.Random.Generics

data T = N T T | L
  deriving (Eq, Ord, Show, Data)

-- size
s :: T -> Integer
s (N l r) = 1 + s l + s r
s L = 1

mBoltz :: IO T
mBoltz = makeGenerator randomPrimRandom undefined 0 10

-- Pointing makes the generator more precise.
mPoint :: IO T
mPoint = makeGenerator randomPrimRandom undefined 1 10

-- Rejection is more elementary and works fine too.
mReject :: IO T
mReject = randomGenerator 10
