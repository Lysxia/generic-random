{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics ( Generic )
import Test.QuickCheck
import Generic.Random.Generic

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitrary' @'Z

main = sample (arbitrary :: Gen (Tree ()))
