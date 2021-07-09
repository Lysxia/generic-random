{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    DerivingVia,
    TypeOperators,
    TemplateHaskell
  #-}

import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(arbitrary), Gen)

import Test.Inspection (inspect, (==-))

import Generic.Random

data T = A | B | C Int [Bool]
  deriving Generic
  deriving Arbitrary via (GenericArbitrary '[1,2,3] T)

arbT :: Gen T
arbT = genericArbitrary (1 % 2 % 3 % ())

arbT' :: Gen T
arbT' = arbitrary

data T1 = A1 | B1 | C1 Int [Bool]
  deriving Generic
  deriving Arbitrary via (GenericArbitrary '[1,2,3] `AndShrinking` T1)

main :: IO ()
main = pure ()

inspect $ 'arbT ==- 'arbT'
