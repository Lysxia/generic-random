-- | Simple 'GHC.Generics'-based 'arbitrary' generators.
--
-- Here is an example. Define your type.
--
-- > data Tree a = Leaf a | Node (Tree a) (Tree a)
-- >   deriving Generic
--
-- Pick an arbitrary implementation.
--
-- > instance Arbitrary a => Arbitrary (Tree a) where
-- >   arbitrary = genericArbitraryFrequency [9, 8]
--
-- @arbitrary :: 'Gen' (Tree a)@ picks a @Leaf@ with probability 9\/17, or a
-- @Node@ with probability 8\/17, and recursively fills their fields with
-- @arbitrary@.

module Generic.Random.Generic
  (
    -- * Arbitrary implementations
    genericArbitrary
  , genericArbitraryFrequency
  , genericArbitraryFrequency'
  , genericArbitrary'

    -- * Type-level natural numbers
    -- $nat
  , Z (..)
  , S (..)

    -- * Generic class for finite values
  , BaseCases'
  , BaseCases
  ) where

import Generic.Random.Internal.Generic
