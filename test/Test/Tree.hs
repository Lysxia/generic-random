{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.Tree where

import Data.Data ( Data )
import GHC.Generics ( Generic )
import Test.QuickCheck

import Generic.Random.Generic

data T = L | N T T
  deriving (Eq, Ord, Show, Data, Generic)

size :: T -> Int
size (N l r) = 1 + size l + size r
size L = 0

instance Arbitrary T where
  arbitrary = genericArbitrary (weights (9 % 8 % ()))
