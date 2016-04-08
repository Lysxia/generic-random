-- | 'Control.Monad.Random' interface
module Data.Random.Generics.Random where

import Control.Monad.Random
import Data.Data
import Data.Random.Generics.Internal
import Data.Random.Generics.Boltzmann.Oracle

-- * Main functions

-- | > randomGenerator size :: m a
--
-- Make a random generator of type @a@ with average size @size@.
randomGenerator :: (Data a, MonadRandom m) => Int -> m a
randomGenerator size = a
  where a = randomGenerator' a size -- @a@ is used just for its type

-- | 'randomGenerator' with the target type as an argument.
randomGenerator' :: (Data a, MonadRandom m) => proxy a -> Int -> m a
randomGenerator' a size = randomApproxGenerator a 0 size (Just epsilon)

-- | > randomApproxGenerator k n eps :: m a
--
-- Boltzmann generator for the @k@-th pointing of type @a@ with average size
-- @n@ and tolerance @eps@ (or no filtering if @eps = Nothing@).
randomApproxGenerator
  :: (Data a, MonadRandom m) => proxy a -> Int -> Int -> Maybe Double -> m a
randomApproxGenerator = ceiledRejectionSampler randomPrimRandom

-- * Auxiliary definitions

randomPrimRandom :: MonadRandom m => PrimRandom m
randomPrimRandom = PrimRandom
  (return ())
  getRandomR
  getRandom
  getRandom
  getRandom
