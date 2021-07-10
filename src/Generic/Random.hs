-- | "GHC.Generics"-based 'Test.QuickCheck.arbitrary' generators.
--
-- = Basic usage
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- data Foo = A | B | C  -- some generic data type
--   deriving 'GHC.Generics.Generic'
-- @
--
-- Derive instances of 'Test.QuickCheck.Arbitrary'.
--
-- @
-- instance Arbitrary Foo where
--   arbitrary = 'genericArbitrary' 'uniform'  -- Give a distribution of constructors.
--   shrink = 'Test.QuickCheck.genericShrink'  -- Generic shrinking is provided by the QuickCheck library.
-- @
--
-- Or derive standalone generators (the fields must still be instances of
-- 'Test.QuickCheck.Arbitrary', or use custom generators).
--
-- @
-- genFoo :: Gen Foo
-- genFoo = 'genericArbitrary' 'uniform'
-- @
--
-- === Using @DerivingVia@
--
-- @
-- {-\# LANGUAGE DerivingVia, TypeOperators \#-}
--
-- data Foo = A | B | C
--   deriving 'GHC.Generics.Generic'
--   deriving Arbitrary via ('GenericArbitraryU' `'AndShrinking'` Foo)
-- @
--
-- For more information:
--
-- - "Generic.Random.Tutorial"
-- - http://blog.poisson.chat/posts/2018-01-05-generic-random-tour.html

{-# LANGUAGE ExplicitNamespaces #-}

module Generic.Random
  (
    -- * Arbitrary implementations

    -- | The suffixes for the variants have the following meanings:
    --
    -- - @U@: pick constructors with uniform distribution (equivalent to
    --   passing 'uniform' to the non-@U@ variant).
    -- - @Single@: restricted to types with a single constructor.
    -- - @G@: with custom generators.
    -- - @Rec@: decrease the size at every recursive call (ensuring termination
    --   for (most) recursive types).
    -- - @'@: automatic discovery of "base cases" when size reaches 0.
    genericArbitrary
  , genericArbitraryU
  , genericArbitrarySingle
  , genericArbitraryRec
  , genericArbitrary'
  , genericArbitraryU'

    -- ** With custom generators

    -- |
    -- === Note about incoherence
    --
    -- The custom generator feature relies on incoherent instances, which can
    -- lead to surprising behaviors for parameterized types.
    --
    -- ==== __Example__
    --
    -- For example, here is a pair type and a custom generator of @Int@ (always
    -- generating 0).
    --
    -- @
    -- data Pair a b = Pair a b
    --   deriving (Generic, Show)
    --
    -- customGen :: Gen Int
    -- customGen = pure 0
    -- @
    --
    -- The following two ways of defining a generator of @Pair Int Int@ are
    -- __not__ equivalent.
    --
    -- The first way is to use 'genericArbitrarySingleG' to define a
    -- @Gen (Pair a b)@ parameterized by types @a@ and @b@, and then
    -- specialize it to @Gen (Pair Int Int)@.
    --
    -- In this case, the @customGen@ will be ignored.
    --
    -- @
    -- genPair :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
    -- genPair = 'genericArbitrarySingleG' customGen
    --
    -- genPair' :: Gen (Pair Int Int)
    -- genPair' = genPair
    -- -- Will generate nonzero pairs
    -- @
    --
    -- The second way is to define @Gen (Pair Int Int)@ directly using
    -- 'genericArbitrarySingleG' (as if we inlined @genPair@ in @genPair'@
    -- above.
    --
    -- Then the @customGen@ will actually be used.
    --
    -- @
    -- genPair2 :: Gen (Pair Int Int)
    -- genPair2 = 'genericArbitrarySingleG' customGen
    -- -- Will only generate (Pair 0 0)
    -- @
    --
    -- In other words, the decision of whether to use a custom generator
    -- is done by comparing the type of the custom generator with the type of
    -- the field only in the context where 'genericArbitrarySingleG' is being
    -- used (or any other variant with a @G@ suffix).
    --
    -- In the first case above, those fields have types @a@ and @b@, which are
    -- not equal to @Int@ (or rather, there is no available evidence that they
    -- are equal to @Int@, even if they could be instantiated as @Int@ later).
    -- In the second case, they both actually have type @Int@.

  , genericArbitraryG
  , genericArbitraryUG
  , genericArbitrarySingleG
  , genericArbitraryRecG

    -- * Specifying finite distributions
  , Weights
  , W
  , (%)
  , uniform

    -- * Custom generators

    -- | Custom generators can be specified in a list constructed with @(':+')@,
    -- and passed to functions such as 'genericArbitraryG' to override how certain
    -- fields are generated.
    --
    -- Example:
    --
    -- @
    -- customGens :: Gen String ':+' Gen Int
    -- customGens =
    --   (filter (/= '\NUL') '<$>' arbitrary) ':+'
    --   (getNonNegative '<$>' arbitrary)
    -- @
    --
    -- There are also different types of generators, other than 'Test.QuickCheck.Gen', providing
    -- more ways to select the fields the generator than by simply comparing types:
    --
    -- - @'Test.QuickCheck.Gen' a@: override fields of type @a@;
    -- - @'Gen1' f@: override fields of type @f x@ for some @x@, requiring a generator for @x@;
    -- - @'Gen1_' f@: override fields of type @f x@ for some @x@, __not__ requiring a generator for @x@;
    -- - @'FieldGen' s a@: override record fields named @s@, which must have type @a@;
    -- - @'ConstrGen' c i a@: override the field at index @i@ of constructor @c@,
    --   which must have type @a@ (0-indexed);
    --
    -- Multiple generators may match a given field: the first, leftmost
    -- generator in the list will be chosen.
  , (:+) (..)
  , FieldGen (..)
  , fieldGen
  , ConstrGen (..)
  , constrGen
  , Gen1 (..)
  , Gen1_ (..)

    -- * Helpful combinators
  , listOf'
  , listOf1'
  , vectorOf'

    -- * Base cases for recursive types
  , withBaseCase
  , BaseCase (..)

    -- * Full options
  , Options ()
  , genericArbitraryWith

    -- ** Setters
  , SetOptions
  , type (<+)
  , setOpts

    -- ** Size modifiers
  , Sizing (..)
  , SetSized
  , SetUnsized
  , setSized
  , setUnsized

    -- ** Custom generators
  , SetGens
  , setGenerators

    -- ** Coherence options
  , Coherence (..)
  , Incoherent (..)

    -- ** Common options
  , SizedOpts
  , sizedOpts
  , SizedOptsDef
  , sizedOptsDef
  , UnsizedOpts
  , unsizedOpts

    -- *** Advanced options
    -- | See 'Coherence'
  , CohUnsizedOpts
  , cohUnsizedOpts
  , CohSizedOpts
  , cohSizedOpts

    -- * Generic classes
  , GArbitrary
  , GUniformWeight

  -- * Newtypes for DerivingVia

  -- | These newtypes correspond to the variants of 'genericArbitrary' above.

  , GenericArbitrary (..)
  , GenericArbitraryU (..)
  , GenericArbitrarySingle (..)
  , GenericArbitraryRec (..)
  , GenericArbitraryG (..)
  , GenericArbitraryUG (..)
  , GenericArbitrarySingleG (..)
  , GenericArbitraryRecG (..)
  , GenericArbitraryWith (..)
  , AndShrinking (..)

  -- ** Helpers typeclasses
  , TypeLevelGenList (..)
  , TypeLevelOpts (..)
  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
import Generic.Random.DerivingVia
