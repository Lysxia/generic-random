{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Common where

import Control.DeepSeq
import Data.Data
import GHC.Generics
import Test.QuickCheck
import Data.Random.Generics
import Data.Random.Generics.Internal

data T = N T T | L
  deriving (Eq, Ord, Show, Data, Generic)

instance NFData T

-- size
s :: T -> Int
s (N l r) = 1 + s l + s r
s L = 0

rejectT, rejectSimpleT', pointT, pointRejectT', pointT' :: Int -> Gen T
rejectT = generator asGen
rejectSimpleT' = simpleGenerator' asGen
pointT = pointedGenerator asGen
pointRejectT' size = generator_ asGen [] 1 (Just size) (tolerance epsilon size)
pointT' = pointedGenerator' asGen
