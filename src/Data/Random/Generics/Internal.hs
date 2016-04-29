{-# LANGUAGE RecordWildCards #-}
module Data.Random.Generics.Internal where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Data
import qualified Data.HashMap.Lazy as HashMap
import Data.Random.Generics.Internal.Oracle
import Data.Random.Generics.Internal.Types

-- | Size as the number of constructors.
type Size = Int

-- * Helper functions

makeGenerator :: (Data a, Monad m)
  => PrimRandom m -> [Alias m] -> proxy a
  -> ((Size, Maybe Size), Int -> Maybe Double -> m a)
makeGenerator primRandom aliases a = ((minSize, maxSize), makeGenerator')
  where
    dd = collectTypes aliases a
    -- We need the next pointing to capture the average size in an equation.
    t = typeRep a
    i = case index dd #! t of
      Left j -> fst (xedni' dd #! j)
      Right i -> i
    minSize = natToInt $ fst (lTerm dd #! i)
    maxSize = HashMap.lookup i (degree dd)
    makeGenerator' _ (Just size)
      | size <= fromIntegral minSize = smallGenerator
      | Just maxSize <- maxSize, size >= fromIntegral maxSize =
        error "Target size too large."
    makeGenerator' _ Nothing | Just _ <- maxSize =
      error "Cannot make singular sampler: this type is finite."
    makeGenerator' k size' = getGenerator dd' generators a k
      where
        dd' = dds !! k
        oracle = makeOracle dd' t size'
        generators = makeGenerators dd' oracle primRandom
    smallGenerator = getSmallGenerator dd (smallGenerators dd primRandom) a
    dds = iterate point dd

type AliasR m = Alias (RejectT m)

ceiledRejectionSampler
  :: (Data a, Monad m)
  => PrimRandom m -> [AliasR m] -> proxy a
  -> ((Size, Maybe Size), Int -> Maybe Double -> (Size, Size) -> m a)
ceiledRejectionSampler primRandom aliases a =
  (range, (fmap . fmap) (flip runRejectT) makeGenerator')
  where
    (range, makeGenerator') = makeGenerator primRandom' aliases a
    primRandom' = ceilPrimRandom primRandom

epsilon :: Double
epsilon = 0.1

-- | > (size * (1 - epsilon), size * (1 + epsilon))
tolerance :: Double -> Int -> (Size, Size)
tolerance epsilon size = (size - delta, size + delta)
  where
    delta = ceiling (fromIntegral size * epsilon)

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
  (fmap lift doubleR)
  (fmap lift integerR)
  (lift int)
  (lift double)
  (lift char)

asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
