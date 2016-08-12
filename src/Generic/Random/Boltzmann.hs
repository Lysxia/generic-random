-- | Applicative interface to define recursive structures and derive Boltzmann
-- samplers.
--
-- Given the recursive structure of the types, and how to combine generators,
-- the library takes care of computing the oracles and setting the right
-- distributions.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, ImplicitParams #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Generic.Random.Boltzmann where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Coerce
import Data.Function
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Numeric.AD as AD
import Generic.Random.Internal.Common
import Generic.Random.Internal.Solver
import Generic.Random.Internal.Types

class Embed f m where
  emap :: (m a -> m b) -> f a -> f b
  -- | A natural transformation between @f@ and @m@?
  embed :: m a -> f a

-- | 'Applicative' defines a product, 'Alternative' defines an addition,
-- with scalar multiplication we get a module.
--
-- This typeclass allows to directly tweak weights in the oracle by
-- chosen factors.
class (Alternative f, Num (Scalar f)) => Module f where
  type Scalar f :: *

  -- | Scalar embedding.
  scalar :: Scalar f -> f ()
  scalar x = x <.> pure ()

  -- | Scalar multiplication.
  (<.>) :: Scalar f -> f a -> f a
  x <.> f = scalar x *> f

infixr 3 <.>

type Endo a = a -> a

data System f a c = System
  { dim :: Int
  , sys' :: f () -> Vector (f a) -> (Vector (f a), c)
  } deriving (Functor)

sys :: System f a c -> f () -> Vector (f a) -> Vector (f a)
sys = (fmap . fmap . fmap) fst sys'

newtype ConstModule r a = ConstModule { unConstModule :: r }

instance Functor (ConstModule r) where
  fmap _ (ConstModule r) = ConstModule r

instance Num r => Embed (ConstModule r) m where
  emap _ (ConstModule r) = ConstModule r
  embed _ = ConstModule 1

instance Num r => Applicative (ConstModule r) where
  pure _ = ConstModule 1
  ConstModule x <*> ConstModule y = ConstModule (x * y)

instance Num r => Alternative (ConstModule r) where
  empty = ConstModule 0
  ConstModule x <|> ConstModule y = ConstModule (x + y)

instance Num r => Module (ConstModule r) where
  type Scalar (ConstModule r) = r
  scalar = ConstModule
  x <.> ConstModule r = ConstModule (x * r)

solve
  :: forall b
  . (forall a. Num a => System (ConstModule a) b ())
  -> Double -> Maybe (Vector Double)
solve s x = fixedPoint defSolveArgs phi' (V.replicate (dim (s @Int)) 0)
  where
    phi' :: forall a. (AD.Mode a, AD.Scalar a ~ Double) => Endo (Vector a)
    phi' = coerce (sys s (scalar (AD.auto x)) :: Endo (Vector (ConstModule a b)))

newtype Weighted m a = Weighted [(Double, m a)]

weighted :: Double -> m a -> Weighted m a
weighted x a = Weighted [(x, a)]

runWeighted :: MonadRandomLike m => Weighted m a -> (Double, m a)
runWeighted (Weighted [a]) = a
runWeighted (Weighted as) = (sum (fmap fst as), frequencyWith doubleR as)

instance Functor m => Functor (Weighted m) where
  fmap f (Weighted as) = Weighted ((fmap . fmap . fmap) f as)

instance MonadRandomLike m => Embed (Weighted m) m where
  emap f = Weighted . (: []) . fmap f . runWeighted
  embed m = Weighted [(1, m)]

instance MonadRandomLike m => Applicative (Weighted m) where
  pure a = Weighted [(1, pure a)]
  f' <*> a' = Weighted [(u * v, f <*> a)]
    where
      (u, f) = runWeighted f'
      (v, a) = runWeighted a'

instance MonadRandomLike m => Alternative (Weighted m) where
  empty = Weighted []
  Weighted as <|> Weighted bs = Weighted (as ++ bs)

instance MonadRandomLike m => Module (Weighted m) where
  type Scalar (Weighted m) = Double
  scalar x = Weighted [(x, pure ())]
  x <.> Weighted as = Weighted (fmap (first (x *)) as)

sfix
  :: MonadRandomLike m
  => System (Weighted m) b c -> Double -> Vector Double -> (Vector (m b), c)
sfix s x oracle =
  fix $ (first . fmap) (snd . runWeighted) . sys' s (scalar x) . V.zipWith weighted oracle . fst

data Pointiful f a = Pointiful [f a] | Zero (f a)

instance Functor f => Functor (Pointiful f) where
  fmap f (Pointiful v) = Pointiful ((fmap . fmap) f v)
  fmap f (Zero x) = Zero (fmap f x)

instance Embed f m => Embed (Pointiful f) m where
  emap f (Pointiful v) = Pointiful ((fmap . emap) f v)
  emap f (Zero x) = Zero (emap f x)
  embed = Zero . embed

instance Module f => Applicative (Pointiful f) where
  pure a = Zero (pure a)
  Zero f <*> Zero x = Zero (f <*> x)
  Zero f <*> Pointiful xs = Pointiful (fmap (f <*>) xs)
  Pointiful fs <*> Zero x = Pointiful (fmap (<*> x) fs)
  Pointiful fs <*> Pointiful xs = Pointiful (convolute fs xs)
    where
      convolute fs xs = zipWith3 sumOfProducts [0 ..] (inits' fs) (inits' xs)
      inits' = tail . inits
      sumOfProducts k f x = asum (zipWith3 (times k) [0 ..] f (reverse x))
      times k k1 f x = fromInteger (binomial k k1) <.> f <*> x

unPointiful :: Alternative f => Pointiful f a -> [f a]
unPointiful (Pointiful as) = as
unPointiful (Zero a) = a : repeat empty

point :: Module f => Int -> System (Pointiful f) b c -> System f b c
point k s = System ((k + 1) * dim s) $ \x ->
  first flatten . sys' s (Pointiful (repeat x)) . resize
  where
    flatten = join . fmap (V.fromList . take (k + 1) . unPointiful)
    resize = V.fromList . fmap Pointiful . chunksOf (k + 1) . V.toList
