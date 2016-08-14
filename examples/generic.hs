{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Generics ( Generic, Rep )
import Test.QuickCheck
import Generic.Random.Generic

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitrary' @'Z

data Bush a = Tip a | Fork (Bush a) (Bush a)
  deriving (Show, Generic)

instance (Arbitrary a, BaseCases' 'Z a) => Arbitrary (Bush a) where
  arbitrary = genericArbitraryFrequency' @('S 'Z) [1, 2]

main = do
  sample (arbitrary :: Gen (Tree ()))
  putStrLn ""
  sample (arbitrary :: Gen (Bush ()))
