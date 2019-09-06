-- | "GHC.Generics"-based 'Test.QuickCheck.arbitrary' generators.
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
  , genericArbitraryG
  , genericArbitraryUG
  , genericArbitrarySingleG
  , genericArbitraryRecG
  , genericArbitraryWith

    -- * Base cases for recursive types
  , withBaseCase
  , BaseCase (..)

    -- * Specifying finite distributions
  , Weights
  , W
  , (%)
  , uniform

    -- * Full options
  , Options ()
  , SizedOpts
  , sizedOpts
  , SizedOptsDef
  , sizedOptsDef
  , UnsizedOpts
  , unsizedOpts
  , Sizing (..)
  , setSized
  , setUnsized
  , (:+) (..)
#if __GLASGOW_HASKELL__ >= 800
  , FieldGen (..)
  , fieldGen
#endif
  , Gen1 (..)
  , Gen1_ (..)
  , setGenerators

    -- * Public classes
  , GArbitrary
  , GUniformWeight

    -- * Helpful combinators
  , listOf'
  , listOf1'
  , vectorOf'
  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
