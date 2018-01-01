-- | Simple "GHC.Generics"-based 'arbitrary' generators.
--
-- For more information:
--
-- - "Generic.Random.Tutorial"
-- - https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/

{-# LANGUAGE CPP #-}

module Generic.Random
  (
    -- * Arbitrary implementations
    genericArbitrary
  , genericArbitraryU
  , genericArbitrarySingle
  , genericArbitrary'
  , genericArbitraryU'
  , genericArbitraryRec
  , genericArbitraryG
  , genericArbitraryUG
  , genericArbitrarySingleG
  , genericArbitraryRecG
  , genericArbitraryWith

    -- * Specifying finite distributions
  , Weights
  , W
  , (%)
  , uniform

    -- * Base cases for recursive types
  , withBaseCase
  , BaseCase (..)

    -- * Full options
  , Options ()
  , SizedOpts
  , sizedOpts
  , UnsizedOpts
  , unsizedOpts
  , Sizing (..)
  , setSized
  , setUnsized
  , GenList (..)
#if __GLASGOW_HASKELL__ >= 800
  , Field (..)
  , field
#endif
  , setGenerators
  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
