{-# LANGUAGE
    CPP,
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    LambdaCase,
    TypeFamilies,
    UndecidableInstances #-}

import Control.Monad (replicateM)
import Control.DeepSeq (NFData, force)
import GHC.Generics (Generic)
import System.Timeout (timeout)

import Test.QuickCheck

import Generic.Random

-- Binary trees
data B = BL | BN B B
  deriving (Eq, Ord, Show, Generic)

size :: B -> Int
size (BN l r) = 1 + size l + size r
size BL = 0

instance Arbitrary B where
  arbitrary = genericArbitrary ((9 :: W "BL") % (3 :: W "BN") % ())

instance NFData B


-- Messing with base cases
newtype T a = W a deriving (Generic, Show)

instance (Arbitrary a, BaseCase (T a)) => Arbitrary (T a) where
  arbitrary = genericArbitrary' uniform

instance NFData a => NFData (T a)


-- Rose tree for testing the custom list generator that's inserted by default.
data NTree = Leaf | Node [NTree] deriving (Generic, Show)

instance Arbitrary NTree where
  arbitrary = genericArbitraryU'

instance NFData NTree

eval :: NFData a => String -> Gen a -> IO ()
eval name g = do
  x <- timeout (10 ^ (6 :: Int)) $ do
    xs <- generate (replicateM 100 g)
    return $! force xs
  case x of
    Just _ -> return ()
    Nothing -> fail $ name ++ ": did not finish on time"

#if __GLASGOW_HASKELL__ >= 800
-- Tests for ConstrGen

data Tree2 = Leaf2 Int | Node2 Tree2 Tree2 deriving (Generic, Show)

instance Arbitrary Tree2 where
  arbitrary = genericArbitraryUG (ConstrGen (Leaf2 <$> arbitrary) :: ConstrGen "Node2" 1 Tree2)

isLeftBiased :: Tree2 -> Bool
isLeftBiased (Leaf2 _) = True
isLeftBiased (Node2 t (Leaf2 _)) = isLeftBiased t
isLeftBiased _ = False
#endif

main :: IO ()
main = do
  eval "B" (arbitrary :: Gen B)
  eval "T" (arbitrary :: Gen (T (T Int)))
  eval "NTree" (arbitrary :: Gen NTree)
#if __GLASGOW_HASKELL__ >= 800
  quickCheck . whenFail (putStrLn "Tree2") $ isLeftBiased
#endif
