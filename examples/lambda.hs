{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Data.Data
import Data.Random.Generics

data Term = Lambda Int Term | App Term Term | Var Int
  deriving (Show, Data)

instance Arbitrary Term where
  arbitrary = sized $ generatorPWith [positiveInts]

positiveInts :: Alias Gen
positiveInts =
  alias $ \() -> fmap getPositive arbitrary :: Gen Int

main = sample (arbitrary :: Gen Term)
