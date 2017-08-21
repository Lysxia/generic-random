{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

import GHC.Generics (Generic)
import Test.QuickCheck
import Generic.Random.Generic

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Generic)

type instance Found (Tree a) z = GFound (Tree a) z
instance GBaseCaseSearch (Tree a) z e => BaseCaseSearch (Tree a) z e

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitrary' uniform

data Bush a = Tip a | Fork (Bush a) (Bush a)
  deriving (Show, Generic)

type instance Found (Bush a) z = GFound (Bush a) z
instance GBaseCaseSearch (Bush a) z e => BaseCaseSearch (Bush a) z e

instance (Arbitrary a, BaseCase (Bush a)) => Arbitrary (Bush a) where
  arbitrary = genericArbitrary' (weights (1 % (2 % ())))

main :: IO ()
main = do
  sample (arbitrary :: Gen (Tree ()))
  putStrLn ""
  sample (arbitrary :: Gen (Bush ()))
