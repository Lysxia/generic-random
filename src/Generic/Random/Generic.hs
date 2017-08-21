-- | Simple 'GHC.Generics'-based 'arbitrary' generators.
--
-- Here is an example. Define your type.
--
-- @
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving Generic
-- @
--
-- Pick an 'arbitrary' implementation.
--
-- @
-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary = 'genericArbitrary' ('weights' (9 '%' 8 '%' ()))
-- @
--
-- @arbitrary :: 'Gen' (Tree a)@ picks a @Leaf@ with probability 9\/17, or a
-- @Node@ with probability 8\/17, and recursively fills their fields with
-- @arbitrary@.
--
-- == Distribution of constructors
--
-- The distribution of constructors can be specified using 'weights' applied to
-- a special list of /weights/ in the same order as the data type definition.
-- This assigns to each constructor a probability proportional to its weight;
-- in other words, @p_C = weight_C / sumOfWeights@.
--
-- The list of weights is built up with the @('%')@ operator as a cons, and using
-- the unit @()@ as the empty list, in the order corresponding to the data type
-- definition. The uniform distribution can be obtained with 'uniform'.
--
-- === Example
--
-- For @Tree@, 'genericArbitrary' produces code equivalent to the following:
--
-- @
-- 'genericArbitrary' :: Arbitrary a => 'Weights' (Tree a) -> Gen (Tree a)
-- 'genericArbitrary' ('weights' (x '%' y '%' ())) =
--   frequency
--     [ (x, Leaf \<$\> arbitrary)
--     , (y, Node \<$\> arbitrary \<*\> arbitrary)
--     ]
-- @
--
-- === Uniform distribution
--
-- You can specify the uniform distribution (all weights equal) with 'uniform'.
-- 'genericArbitraryU' is available as a shorthand for
-- @'genericArbitrary' 'uniform'@.
--
-- Note that for many types, a uniform distribution tends to produce big
-- values. For instance for @Tree a@, generated values are finite but the
-- __average__ number of @Leaf@ and @Node@ constructors is __infinite__.
--
-- === Checked weights
--
-- /GHC 8.0.1 and above only./
--
-- The weights actually have type @'W' \"ConstructorName\"@ (just a newtype
-- around 'Int'), so that you can annotate a weight with its corresponding
-- constructor, and it will be checked that you got the order right.
--
-- This will type-check.
--
-- @
-- 'weighted' ((x :: 'W' \"Leaf\") '%' (y :: 'W' \"Node\") '%' ()) :: 'Weights' (Tree a)
-- 'weighted' (x '%' (y :: 'W' \"Node\") '%' ()) :: 'Weights' (Tree a)
-- @
--
-- This will not: the first requires an order of constructors different from
-- the definition of the @Tree@ type; the second doesn't have the right number
-- of weights.
--
-- @
-- 'weighted' ((x :: 'W' \"Node\") '%' y '%' ()) :: 'Weights' (Tree a)
-- 'weighted' (x '%' y '%' z '%' ()) :: 'Weights' (Tree a)
-- @
--
-- == Ensuring termination
--
-- As was just mentioned, one must be careful with recursive types
-- to avoid producing extremely large values.
--
-- The alternative 'genericArbitrary'' implements a simple strategy to keep
-- values at reasonable sizes: the size parameter of 'Gen' is divided among the
-- fields of the chosen constructor. When it reaches zero, the generator
-- selects a finite term whenever it can find any of the given type.  This
-- generally ensures that the number of constructors remains close to the
-- initial size parameter passed to 'Gen'.
--
-- A natural number @n@ determines the maximum /depth/ of terms that can be
-- used to end recursion.
-- It is encoded using @'Z' :: 'Z'@ and @'S' :: n -> 'S' n@.
--
-- @
-- 'genericArbitrary'' n ('weights' (...))
-- @
--
-- With @n = 'Z'@, the generator looks for a simple nullary constructor.  If none
-- exist at the current type, as is the case for our @Tree@ type, it carries on
-- as in 'genericArbitrary'.
--
-- @
-- 'genericArbitrary'' 'Z' :: Arbitrary a => 'Weights' (Tree a) -> Gen (Tree a)
-- 'genericArbitrary'' 'Z' ('weights' (x '%' y '%' ())) =
--   frequency
--     [ (x, Leaf \<$\> arbitrary)
--     , (y, scale (\`div\` 2) $ Node \<$\> arbitrary \<*\> arbitrary)
--     -- 2 because Node is 2-ary.
--     ]
-- @
--
-- Here is another example with nullary constructors:
--
-- @
-- data Tree' = Leaf1 | Leaf2 | Node3 Tree' Tree' Tree'
--   deriving Generic
--
-- instance Arbitrary Tree' where
--   arbitrary = 'genericArbitrary'' 'Z' ('weights' (1 '%' 2 '%' 3 '%' ()))
-- @
--
-- Here, @'genericArbitrary'' 'Z'@ is equivalent to:
--
-- @
-- 'genericArbitrary'' 'Z' :: 'Weights' Tree' -> Gen Tree'
-- 'genericArbitrary'' 'Z' ('weights' (x '%' y '%' z '%' ())) =
--   sized $ \n ->
--     if n == 0 then
--       -- If the size parameter is zero, the non-nullary alternative is discarded.
--       frequency $
--         [ (x, return Leaf1)
--         , (y, return Leaf2)
--         ]
--     else
--       frequency $
--         [ (x, return Leaf1)
--         , (y, return Leaf2)
--         , (z, resize (n \`div\` 3) node)  -- 3 because Node3 is 3-ary
--         ]
--   where
--     node = Node3 \<$\> arbitrary \<*\> arbitrary \<*\> arbitrary
-- @
--
-- To increase the chances of termination when no nullary constructor is directly
-- available, such as in @Tree@, we can pass a larger depth @n@. The effectiveness
-- of this parameter depends on the concrete type the generator is used for.
--
-- For instance, if we want to generate a value of type @Tree ()@, there is a
-- value of depth 1 (represented by @'S' 'Z'@) that we can use to end
-- recursion: @Leaf ()@.
--
-- @
-- 'genericArbitrary'' ('S' 'Z') :: 'Weights' (Tree ()) -> Gen (Tree ())
-- 'genericArbitrary'' ('S' 'Z') ('weights' (x '%' y '%' ())) =
--   sized $ \n ->
--     if n == 0 then
--       return (Leaf ())
--     else
--       frequency
--         [ (x, Leaf \<$\> arbitrary)
--         , (y, scale (\`div\` 2) $ Node \<$\> arbitrary \<*\> arbitrary)
--         ]
-- @
--
-- Because the argument of @Tree@ must be inspected in order to discover
-- values of type @Tree ()@, we incur some extra constraints if we want
-- polymorphism.
--
-- @UndecidableInstances@ is also required.
--
-- @
-- instance (Arbitrary a, Generic a, 'ListBaseCases' 'Z' (Rep a))
--   => Arbitrary (Tree a) where
--   arbitrary = 'genericArbitrary'' ('S' 'Z') ('weights' (1 '%' 2 '%' ()))
-- @
--
-- A synonym is provided for brevity.
--
-- @
-- instance (Arbitrary a, 'BaseCases'' Z a) => Arbitrary (Tree a) where
--   arbitrary = 'genericArbitrary'' ('S' 'Z') ('weights' (1 '%' 2 '%' ()))
-- @


module Generic.Random.Generic
  (
    -- * Arbitrary implementations
    genericArbitrary
  , genericArbitraryU
  , genericArbitrary'
  , genericArbitraryU'
  , genericArbitraryRec

    -- * Specifying finite distributions
  , Weights
  , W
  , weights
  , (%)
  , uniform

    -- * Base cases for recursive types
  , withBaseCase
  , BaseCase
  , baseCase
  , Found, GFound
  , BaseCaseSearch()
  , GBaseCaseSearch()
  , gBaseCaseSearch
  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
