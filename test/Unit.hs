{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

import GHC.Generics
import Test.QuickCheck

import Generic.Random.Generic

newtype T a = W a deriving (Generic, Show)

instance (Arbitrary a, BaseCase (T a)) => Arbitrary (T a) where
  arbitrary = genericArbitrary' uniform

type instance Found (T a) z = GFound (T a) z
instance GBaseCaseSearch (T a) z e => BaseCaseSearch (T a) z e

f :: Gen (T (T Int))
f = arbitrary

main :: IO ()
main = sample' f >>= force
  where
    force [] = return ()
    force (x : xs) = x `seq` force xs
