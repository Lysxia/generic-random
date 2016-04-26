{-# LANGUAGE RecordWildCards #-}
module Data.Random.Generics.Internal where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Data
import qualified Data.HashMap.Lazy as HashMap
import Data.Random.Generics.Boltzmann.Oracle

-- * Helper functions

type Size = Int

makeGenerator :: (Data a, Monad m)
  => PrimRandom m -> proxy a -> ((Size, Maybe Size), Int -> Maybe Size -> m a)
makeGenerator primRandom a = ((minSize, maxSize'), makeGenerator')
  where
    dd = collectTypes [] (undefined `asProxyTypeOf` a)
    -- We need the next pointing to capture the average size in an equation.
    t = typeRep a
    i = case index dd #! t of
      Left j -> fst (xedni' dd #! j)
      Right i -> i
    minSize = order dd #! i
    maxSize' = HashMap.lookup i (degree dd)
    makeGenerator' _ (Just size)
      | size <= minSize = error "Target size too small."
      | Just maxSize <- maxSize', size >= maxSize =
        error "Target size too large."
    makeGenerator' _ Nothing | Just _ <- maxSize' =
      error "Cannot make singular sampler: this type is finite."
    makeGenerator' k size' = getGenerator dd' generators a k
      where
        dd' = iterate point dd !! k
        oracle = makeOracle dd' t size'
        generators = makeGenerators dd' oracle primRandom

ceiledRejectionSampler
  :: (Data a, Monad m) => PrimRandom m
  -> proxy a -> ((Size, Maybe Size), Int -> Maybe Size -> (Size, Size) -> m a)
ceiledRejectionSampler primRandom a =
  (range, (fmap . fmap) (flip runRejectT) makeGenerator')
  where
    (range, makeGenerator') = makeGenerator primRandom' a
    primRandom' = ceilPrimRandom primRandom

epsilon :: Double
epsilon = 0.1

-- | > (size * (1 - epsilon), size * (1 + epsilon))
tolerance :: Double -> Size -> (Size, Size)
tolerance epsilon size = (size - delta, size + delta)
  where
    delta = floor (fromIntegral size * epsilon)

type RejectT m = ReaderT Size (StateT Size (MaybeT m))

-- | Set upper bound
ceilPrimRandom :: Monad m => PrimRandom m -> PrimRandom (RejectT m)
ceilPrimRandom pr = ((lpr . lpr . lpr) pr)
  { incr = do
      maxSize <- ask
      size <- get
      guard (size < maxSize)
      put (size + 1)
  } where
    lpr x = liftPrimRandom x

-- | Set lower bound
runRejectT :: Monad m => (Size, Size) -> RejectT m a -> m a
runRejectT (minSize, maxSize) m = fix $ \go -> do
  x' <- runMaybeT (m `runReaderT` maxSize `runStateT` 0)
  case x' of
    Just (x, size) | size >= minSize -> return x
    _ -> go

liftPrimRandom
  :: (MonadTrans t, Monad m) => PrimRandom m -> PrimRandom (t m)
liftPrimRandom PrimRandom{..} = PrimRandom
  (lift incr)
  (fmap lift getRandomR_)
  (lift int)
  (lift double)
  (lift char)

asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
