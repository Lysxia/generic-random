-- | Simple 'GHC.Generics'-based 'arbitrary' generators.
--
-- Here is an example. Define your type.
--
-- > data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- Derive 'GHC.Generics.Generic'.
--
-- >   deriving 'Generic'  -- Turn on the DeriveGeneric extension
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
  ( genericArbitrary
  , genericArbitraryFrequency
  , genericArbitraryFrequency'
  , genericArbitrary'
  , Nat (..)
  ) where

import Generic.Random.Internal.Generic
