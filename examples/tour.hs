-- Just another toy example

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad
import GHC.Generics
import Test.QuickCheck

import Generic.Random

data MyType
  = OneThing Int
  | TwoThings Double String
  | ThreeThings (Maybe Integer) [()] (Bool -> Word)
  deriving (Show, Generic)

custom :: Gen (Maybe Integer) :+ ()
custom = (Just <$> arbitrary) :+ ()

instance Arbitrary MyType where
  arbitrary :: Gen MyType
  arbitrary = genericArbitraryG custom (1 % 4 % 4 % ())
  -- arbitrary = frequency
  --   [ (1, OneThing <$> arbitrary)
  --   , (4, TwoThings <$> arbitrary <*> arbitrary)
  --   , (4, ThreeThings <$> (Just <$> arbitrary) <*> arbitrary <*> arbitrary)
  --   ]

main :: IO ()
#ifndef BENCHMODE
main = do
  -- Print some examples
  sample (arbitrary @MyType)

  -- Check the property that ThreeThings contains three things.
  quickCheck $ \case
    ThreeThings Nothing _ _ -> False
    _ -> True
#else
-- Quick and dirty benchmark
main = do
  xs <- generate (replicateM 1000000 (arbitrary @MyType))
  go xs
 where
   go [] = print ()
   go (x : xs) = x `seq` go xs
#endif

-- Ew. Sorry.
instance Show a => Show (Bool -> a) where
  show f = "<True -> " ++ show (f True) ++ ",False -> " ++ show (f False) ++ ">"
