-- | Simple "GHC.Generics"-based 'arbitrary' generators.
--
-- For more information:
--
-- - "Generic.Random.Tutorial"
-- - http://blog.poisson.chat/posts/2018-01-05-generic-random-tour.html
-- - https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/

{-# LANGUAGE CPP #-}

module Generic.Random
  (
    -- * Arbitrary implementations
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

    -- * Public classes
  , GArbitrary
  , GUniformWeight
  ) where

import Generic.Random.Internal.BaseCase
import Generic.Random.Internal.Generic
