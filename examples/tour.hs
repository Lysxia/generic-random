-- Just another toy example

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import GHC.Generics
import Test.QuickCheck

import Generic.Random

data MyType
  = OneThing Int
  | TwoThings Double String
  | ThreeThings (Maybe Integer) [()] (Bool -> Word)
  deriving (Show, Generic)

custom :: GenList '[Maybe Integer]
custom = (Just <$> arbitrary) :@ Nil

instance Arbitrary MyType where
  arbitrary :: Gen MyType
  arbitrary = genericArbitraryG custom (1 % 4 % 4 % ())
  -- arbitrary = frequency
  --   [ (1, OneThing <$> arbitrary)
  --   , (4, TwoThings <$> arbitrary <*> arbitrary)
  --   , (4, ThreeThings <$> (Just <$> arbitrary) <*> arbitrary <*> arbitrary)
  --   ]

main = do
  -- Print some examples
  sample (arbitrary @MyType)

  -- Check the property that ThreeThings contains three things.
  quickCheck $ \case
    ThreeThings Nothing _ _ -> False
    _ -> True

-- Ew. Sorry.
instance Show a => Show (Bool -> a) where
  show f = "<True -> " ++ show (f True) ++ ",False -> " ++ show (f False) ++ ">"
