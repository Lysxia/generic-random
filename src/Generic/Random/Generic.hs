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
-- >   arbitrary = genericArbitrary (weights (9 % 8 % ()))
--
-- @arbitrary :: 'Gen' (Tree a)@ picks a @Leaf@ with probability 9\/17, or a
-- @Node@ with probability 8\/17, and recursively fills their fields with
-- @arbitrary@.

module Generic.Random.Generic
  (
    -- * Arbitrary implementations
    genericArbitrary
  , genericArbitrary'

    -- * Specifying finite distributions
  , Weights
  , W
  , weights
  , (%)
  , uniform

    -- * Type-level natural numbers
    -- $nat
  , Z (..)
  , S (..)

    -- * Generic classes for finite values
  , BaseCases'
  , BaseCases
  , ListBaseCases
  ) where

import Generic.Random.Internal.Generic
