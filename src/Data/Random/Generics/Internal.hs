{-# LANGUAGE RecordWildCards #-}
module Data.Random.Generics.Internal where

import Control.Arrow ( (&&&) )
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Data
import qualified Data.HashMap.Lazy as HashMap
import Data.Random.Generics.Internal.Oracle
import Data.Random.Generics.Internal.Types

-- | Size as the number of constructors.
type Size = Int

-- | Sized generator.
data SG r = SG
  { minSize :: Size
  , maxSizeM :: Maybe Size
  , runSG :: Int -> Maybe Double -> r
  }

rangeSG :: SG r -> (Size, Maybe Size)
rangeSG = minSize &&& maxSizeM

-- * Helper functions

makeGenerator :: (Data a, Monad m)
  => PrimRandom m -> [Alias m] -> proxy a -> SG (m a)
makeGenerator primRandom aliases a =
  SG minSize maxSizeM makeGenerator'
  where
    dd = collectTypes aliases a
    t = typeRep a
    i = case index dd #! t of
      Left j -> fst (xedni' dd #! j)
      Right i -> i
    minSize = natToInt $ fst (lTerm dd #! i)
    maxSizeM = HashMap.lookup i (degree dd)
    makeGenerator' k sizeM = getGenerator dd' generators a k
      where
        dd' = dds !! k
        oracle = makeOracle dd' t sizeM
        generators = makeGenerators dd' oracle primRandom
    -- smallGenerator = getSmallGenerator dd (smallGenerators dd primRandom) a
    dds = iterate point dd

applySG :: SG r -> Int -> Maybe Double -> r
applySG SG{..} k sizeM
  | Just minSize == maxSizeM = runSG k (fmap fromIntegral maxSizeM)
  | Just size <- sizeM, size <= fromIntegral minSize =
      error "Target size too small."
  | Just True <- liftA2 ((<=) . fromIntegral) maxSizeM sizeM =
      error "Target size too large."
  | Nothing <- sizeM, Just _ <- maxSizeM =
      error "Cannot make singular sampler for finite type."
  | otherwise = runSG k sizeM

type AliasR m = Alias (RejectT m)

ceiledRejectionSampler :: (Data a, Monad m)
  => PrimRandom m -> [AliasR m] -> proxy a
  -> SG ((Size, Size) -> m a)
ceiledRejectionSampler primRandom aliases a = sg
  { runSG = (fmap . fmap) (flip runRejectT) (runSG sg) }
  where
    sg = makeGenerator primRandom' aliases a
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
