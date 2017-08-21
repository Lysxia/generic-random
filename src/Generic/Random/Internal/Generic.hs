{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Random.Internal.Generic where

import Control.Applicative
import Data.Proxy
#if __GLASGOW_HASKELL__ >= 800
import GHC.Generics hiding (S)
#else
import GHC.Generics hiding (S, Arity)
#endif
import GHC.TypeLits
import Test.QuickCheck

-- * Random generators

-- | Pick a constructor with a given distribution, and fill its fields
-- recursively.
genericArbitrary
  :: forall a
  .  (Generic a, GA Unsized (Rep a))
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitrary (Weights w n) = fmap to (ga (Proxy :: Proxy Unsized) w n)

-- | Shorthand for @'genericArbitrary' 'uniform'@.
genericArbitraryU
  :: forall a
  .  (Generic a, GA Unsized (Rep a), UniformWeight (Weights_ (Rep a)))
  => Gen a
genericArbitraryU = genericArbitrary uniform

-- | Decrease size at every recursive call, but don't do anything different
-- at size 0.
genericArbitraryRec
  :: forall a
  . (Generic a, GA Sized (Rep a))
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRec (Weights w n) =
  fmap to (ga (Proxy :: Proxy Sized) w n :: Gen (Rep a p))

-- * Internal

type family Weights_ (f :: * -> *) :: * where
  Weights_ (f :+: g) = Weights_ f :| Weights_ g
  Weights_ (M1 D _c f) = Weights_ f
#if __GLASGOW_HASKELL__ >= 800
  Weights_ (M1 C ('MetaCons c _i _j) _f) = L c
#else
  Weights_ (M1 C _c _f) = ()
#endif

data a :| b = N a Int b
data L (c :: Symbol) = L

-- | Trees of weights assigned to constructors of type @a@,
-- rescaled to obtain a probability distribution.
--
-- Two ways of constructing them.
--
-- @
-- 'weights' (x1 '%' x2 '%' ... '%' xn '%' ()) :: 'Weights' a
-- 'uniform' :: 'Weights' a
-- @
--
-- Using @weights@, there must be exactly as many weights as
-- there are constructors.
--
-- 'uniform' is equivalent to @'weights' (1 '%' ... '%' 1 '%' ())@
-- (automatically fills out the right number of 1s).
data Weights a = Weights (Weights_ (Rep a)) Int

-- | Type of a single weight, tagged with the name of the associated
-- constructor for additional compile-time checking.
--
-- @
-- 'weights' ((9 :: 'W' \"Leaf\") '%' (8 :: 'W' \"Node\") '%' ())
-- @
newtype W (c :: Symbol) = W Int deriving Num

-- | A smart constructor to specify a custom distribution.
weights :: (Weights_ (Rep a), Int, ()) -> Weights a
weights (w, n, ()) = Weights w n

-- | Uniform distribution.
uniform :: UniformWeight (Weights_ (Rep a)) => Weights a
uniform =
  let (w, n) = uniformWeight
  in Weights w n

type family First a :: Symbol where
  First (a :| _b) = First a
  First (L c) = c

class WeightBuilder a where
  type Prec a r

  -- | A binary constructor for building up trees of weights.
  (%) :: W (First a) -> Prec a r -> (a, Int, r)

infixr 1 %

instance WeightBuilder a => WeightBuilder (a :| b) where
  type Prec (a :| b) r = Prec a (b, Int, r)
  m % prec =
    let (a, n, (b, p, r)) = m % prec
    in (N a n b, n + p, r)

instance WeightBuilder (L c) where
  type Prec (L c) r = r
  W m % prec = (L, m, prec)

instance WeightBuilder () where
  type Prec () r = r
  W m % prec = ((), m, prec)

class UniformWeight a where
  uniformWeight :: (a, Int)

instance (UniformWeight a, UniformWeight b) => UniformWeight (a :| b) where
  uniformWeight =
    let
      (a, m) = uniformWeight
      (b, n) = uniformWeight
    in
      (N a m b, m + n)

instance UniformWeight (L c) where
  uniformWeight = (L, 1)

instance UniformWeight () where
  uniformWeight = ((), 1)

data Sized
data Unsized

-- | Generic Arbitrary
class GA sized f where
  ga :: proxy sized -> Weights_ f -> Int -> Gen (f p)

instance GA sized f => GA sized (M1 D c f) where
  ga z w n = fmap M1 (ga z w n)

instance (GASum sized f, GASum sized g) => GA sized (f :+: g) where
  ga = gaSum'

instance GAProduct sized f => GA sized (M1 C c f) where
  ga z _ _ = fmap M1 (gaProduct z)


gArbitrarySingle
  :: forall sized f p c0 proxy
  .  (GA sized f, Weights_ f ~ L c0)
  => proxy sized -> Gen (f p)
gArbitrarySingle z = ga z L 0

gaSum' :: GASum sized f => proxy sized -> Weights_ f -> Int -> Gen (f p)
gaSum' z w n = do
  i <- choose (0, n-1)
  gaSum z i w

class GASum sized f where
  gaSum :: proxy sized -> Int -> Weights_ f -> Gen (f p)

instance (GASum sized f, GASum sized g) => GASum sized (f :+: g) where
  gaSum z i (N a n b)
    | i < n = fmap L1 (gaSum z i a)
    | otherwise = fmap R1 (gaSum z (i - n) b)

instance GAProduct sized f => GASum sized (M1 i c f) where
  gaSum z _ _ = fmap M1 (gaProduct z)


class GAProduct sized f where
  gaProduct :: proxy sized -> Gen (f p)

instance GAProduct' f => GAProduct Unsized f where
  gaProduct _ = gaProduct'

instance (GAProduct' f, KnownNat (Arity f)) => GAProduct Sized f where
  gaProduct _ = sized $ \n -> resize (n `div` arity) gaProduct'
    where
      arity = fromInteger (natVal (Proxy :: Proxy (Arity f)))

instance {-# OVERLAPPING #-} GAProduct Sized U1 where
  gaProduct _ = pure U1


class GAProduct' f where
  gaProduct' :: Gen (f p)

instance GAProduct' U1 where
  gaProduct' = pure U1

instance Arbitrary c => GAProduct' (K1 i c) where
  gaProduct' = fmap K1 arbitrary

instance (GAProduct' f, GAProduct' g) => GAProduct' (f :*: g) where
  gaProduct' = liftA2 (:*:) gaProduct' gaProduct'

instance GAProduct' f => GAProduct' (M1 i c f) where
  gaProduct' = fmap M1 gaProduct'


type family Arity f :: Nat where
  Arity (f :*: g) = Arity f + Arity g
  Arity (M1 _i _c _f) = 1


newtype Weighted a = Weighted (Maybe (Int -> Gen a, Int))
  deriving Functor

instance Applicative Weighted where
  pure a = Weighted (Just ((pure . pure) a, 1))
  Weighted f <*> Weighted a = Weighted $ liftA2 g f a
    where
      g (f, m) (a, n) =
        ( \i ->
            let (j, k) = i `divMod` m
            in f j <*> a k
        , m * n )

instance Alternative Weighted where
  empty = Weighted Nothing
  a <|> Weighted Nothing = a
  Weighted Nothing <|> b = b
  Weighted (Just (a, m)) <|> Weighted (Just (b, n)) = Weighted . Just $
    ( \i ->
        if i < m then
          a i
        else
          b (i - m)
    , m + n )

liftGen :: Gen a -> Weighted a
liftGen g = Weighted (Just (\_ -> g, 1))

