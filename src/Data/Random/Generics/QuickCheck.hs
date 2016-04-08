-- | 'Test.QuickCheck' interface
module Data.Random.Generics.QuickCheck where

import Data.Data
import Test.QuickCheck
import Data.Random.Generics.Internal
import Data.Random.Generics.Boltzmann.Oracle

-- * Main functions

-- | > arbitraryGenerator size :: Gen a
--
-- Make a random generator of type @a@ with average size @size@.
arbitraryGenerator :: Data a => Int -> Gen a
arbitraryGenerator size = a
  where a = arbitraryGenerator' a size

-- | 'arbitraryGenerator' with the target type as an argument.
arbitraryGenerator' :: Data a => proxy a -> Int -> Gen a
arbitraryGenerator' a size = arbitraryApproxGenerator' a size epsilon

arbitraryApproxGenerator' :: Data a => proxy a -> Int -> Double -> Gen a
arbitraryApproxGenerator' = ceiledRejectionSampler arbitraryPrimRandom

-- * Auxiliary definitions

arbitraryPrimRandom :: PrimRandom Gen
arbitraryPrimRandom = PrimRandom
  (return ())
  choose
  arbitrary
  arbitrary
  arbitrary
