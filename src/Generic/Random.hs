-- | "GHC.Generics"-based 'Test.QuickCheck.arbitrary' generators.
--
-- = Basic usage
--
-- @
-- data Foo = A | B | C  -- some generic data type
--   deriving 'GHC.Generics.Generic'
-- @
--
-- Derive instances of 'Test.QuickCheck.Arbitrary'.
--
-- @
-- instance Arbitrary Foo where
--   arbitrary = 'genericArbitrary' 'uniform'  -- give a distribution of constructors
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
-- For more information:
--
-- - "Generic.Random.Tutorial"
-- - http://blog.poisson.chat/posts/2018-01-05-generic-random-tour.html

{-# LANGUAGE CPP #-}

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
  , (:+) (..)
#if __GLASGOW_HASKELL__ >= 800
  , FieldGen (..)
  , fieldGen
  , ConstrGen (..)
  , constrGen
#endif
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

    -- ** Size modifiers
  , Sizing (..)
  , setSized
  , setUnsized

    -- ** Custom generators
  , SetGens
  , setGenerators

    -- ** Common options
  , SizedOpts
  , sizedOpts
  , SizedOptsDef
  , sizedOptsDef
  , UnsizedOpts
  , unsizedOpts

    -- * Generic classes
  , GArbitrary
  , GUniformWeight

  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
