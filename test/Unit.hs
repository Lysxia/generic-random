{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (replicateM)
import Control.DeepSeq (NFData, force)
import GHC.Generics (Generic)
import System.Timeout (timeout)

import Test.QuickCheck

import Generic.Random

newtype T a = W a deriving (Generic, Show)

instance (Arbitrary a, BaseCase (T a)) => Arbitrary (T a) where
  arbitrary = genericArbitrary' uniform

instance NFData a => NFData (T a)

f :: Gen (T (T Int))
f = arbitrary


data NTree = Leaf | Node [NTree] deriving (Generic, Show)

instance Arbitrary NTree where
  arbitrary = genericArbitraryU'

instance NFData NTree

eval :: NFData a => String -> Gen a -> IO ()
eval name g = do
  x <- timeout (10 ^ 6) $ do
    xs <- replicateM 100 (generate g)
    return $! force xs
  case x of
    Just _ -> return ()
    Nothing -> fail $ name ++ ": did not finish on time"

main :: IO ()
main = do
  eval "T" (arbitrary :: Gen (T (T Int)))
  eval "NTree" (arbitrary :: Gen NTree)
