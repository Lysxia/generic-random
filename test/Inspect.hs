{-# OPTIONS_GHC -dsuppress-all #-}
{-# LANGUAGE
    DeriveGeneric,
    TemplateHaskell
  #-}


import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(arbitrary), Gen, choose)

import Test.Inspection (inspect, (===))

import Generic.Random

arbMaybe :: Arbitrary a => Gen (Maybe a)
arbMaybe = genericArbitraryU

arbMaybe' :: Arbitrary a => Gen (Maybe a)
arbMaybe' = do
  i <- choose (0, 1 :: Int)
  if i < 1 then
    pure Nothing
  else
    Just <$> arbitrary

data T = A | B | C Int [Bool]
  deriving Generic

arbT :: Gen T
arbT = genericArbitrary (1 % 2 % 3 % ())

arbT' :: Gen T
arbT' = do
  i <- choose (0, 5 :: Int)
  if i < 1 then
    pure A
  else
    if i - 1 < 2 then
      pure B
    else
      C <$> arbitrary <*> arbitrary

main :: IO ()
main = pure ()

inspect $ 'arbMaybe === 'arbMaybe'
inspect $ 'arbT === 'arbT'
