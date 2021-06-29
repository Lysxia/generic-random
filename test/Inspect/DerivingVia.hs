{-# OPTIONS_GHC -dsuppress-all #-}
{-# LANGUAGE
    DeriveGeneric,
    DerivingVia,
    TemplateHaskell
  #-}


import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(arbitrary), Gen)

import Test.Inspection (inspect, (===))

import Generic.Random

data T = A | B | C Int [Bool]
  deriving Generic
  deriving Arbitrary via (GenericArbitrary '[1,2,3] T)

arbT :: Gen T
arbT = genericArbitrary (1 % 2 % 3 % ())

arbT' :: Gen T
arbT' = arbitrary

main :: IO ()
main = pure ()

inspect $ 'arbT === 'arbT'
