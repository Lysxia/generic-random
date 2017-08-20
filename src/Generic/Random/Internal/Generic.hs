{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
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

-- | Decrease size to ensure termination for
-- recursive types, looking for base cases once the size reaches 0.
genericArbitrary'
  :: forall a
  . (Generic a, GA Sized (Rep a), BaseCase a)
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitrary' w = genericArbitraryRec w `withBaseCase` baseCase

-- | Shorthand for @\n -> 'genericArbitrary'' n 'uniform'@.
genericArbitraryU'
  :: forall a
  . (Generic a, GA Sized (Rep a), BaseCase a, UniformWeight (Weights_ (Rep a)))
  => Gen a
genericArbitraryU' = genericArbitrary' uniform

-- | Decrease size at every recursive call, but don't do anything different
-- at size 0.
genericArbitraryRec
  :: forall a
  . (Generic a, GA Sized (Rep a))
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRec (Weights w n) =
  fmap to (ga (Proxy :: Proxy Sized) w n :: Gen (Rep a p))

withBaseCase
  :: Gen a  -- ^ Default generator
  -> Gen a  -- ^ Base case
  -> Gen a
withBaseCase def bc = sized $ \sz ->
  if sz > 0 then def else bc

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

instance GAProduct f => GA Unsized (M1 C c f) where
  ga _ _ _ = fmap M1 gaProduct

instance (GAProduct f, KnownNat (Arity f)) => GA Sized (M1 C c f) where
  ga _ _ _ = sized $ \n -> resize (n `div` arity) gaProduct
    where
      arity = fromInteger (natVal (Proxy :: Proxy (Arity f)))

instance (GASum sized f, GASum sized g) => GA sized (f :+: g) where
  ga = gaSum'

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

instance GAProduct f => GASum sized (M1 i c f) where
  gaSum _ _ _ = gaProduct


class GAProduct f where
  gaProduct :: Gen (f p)

instance GAProduct U1 where
  gaProduct = pure U1

instance Arbitrary c => GAProduct (K1 i c) where
  gaProduct = fmap K1 arbitrary

instance GAProduct f => GAProduct (M1 i c f) where
  gaProduct = fmap M1 gaProduct

instance (GAProduct f, GAProduct g) => GAProduct (f :*: g) where
  gaProduct = liftA2 (:*:) gaProduct gaProduct

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

class BaseCaseSearch (n :: Nat) a where
  type Found n a :: Bool
  type Found n a = GFound n (Rep a)

  baseCaseSearch :: b ~ Found n a => proxy n -> If b Gen Proxy a

  default baseCaseSearch
    :: (Generic a, GBaseCaseSearch' n a b, b ~ GFound n (Rep a))
    => proxy n -> If b Gen Proxy a
  baseCaseSearch = genericBCS

instance BaseCaseSearch n Int where
  type Found n Int = 'True
  baseCaseSearch = const arbitrary

type family If (b :: Bool) (c :: k) (d :: k) :: k
type instance If 'True c d = c
type instance If 'False c d = d

type family GFound (n :: Nat) (f :: k -> *) :: Bool
type instance GFound n (M1 i c f) = GFound n f
type instance GFound n (f :+: g) = GFound n f || GFound n g
type instance GFound n (f :*: g) = GFound n f && GFound n g
type instance GFound n (K1 i c) = GNotFoundIfZ (n == 0) n c
type instance GFound n U1 = 'True

type (==) m n = IsEQ (CmpNat m n)

type family IsEQ (e :: Ordering) :: Bool
type instance IsEQ 'EQ = 'True
type instance IsEQ 'GT = 'False
type instance IsEQ 'LT = 'False

type family GNotFoundIfZ (b :: Bool) (n :: Nat) c :: Bool
type instance GNotFoundIfZ 'True n c = 'False
type instance GNotFoundIfZ 'False n c = Found n c

type family (||) (b :: Bool) (c :: Bool) :: Bool
type instance 'True  || c = 'True
type instance 'False || c = c

type family (&&) (b :: Bool) (c :: Bool) :: Bool
type instance 'True  && c = c
type instance 'False && c = 'False

class GBCS n f where
  gbcs :: proxy n -> Weighted (f p)

instance GBCS n f => GBCS n (M1 i c f) where
  gbcs = (fmap . fmap) M1 gbcs

instance (GBCS n f, GBCS n g) => GBCS n (f :+: g) where
  gbcs = liftA2 (\f g -> fmap L1 f <|> fmap R1 g) gbcs gbcs

instance (GBCS n f, GBCS n g) => GBCS n (f :*: g) where
  gbcs = (liftA2 . liftA2) (:*:) gbcs gbcs

instance {-# OVERLAPPABLE #-}
  (BaseCaseSearch (n - 1) c, Found (n - 1) c ~ 'True)
  => GBCS n (K1 i c) where
  gbcs _ = liftGen (fmap K1 (baseCaseSearch (Proxy :: Proxy (n - 1))))

instance GBCS 0 (K1 i c) where
  gbcs = const empty

instance GBCS n U1 where
  gbcs = const (pure U1)

class GBaseCaseSearch' n a b where
  gbcs' :: proxy b -> proxy2 n -> If b Gen Proxy a

instance GBaseCaseSearch' n a 'False where
  gbcs' = const (const Proxy)

instance (Generic a, GBCS n (Rep a)) => GBaseCaseSearch' n a 'True where
  gbcs' = const (fmap (\(Weighted (Just (g, n))) -> choose (0, n-1) >>= fmap to . g) gbcs)

class GBaseCaseSearch' n a (GFound n (Rep a)) => GBaseCaseSearch n a
instance GBaseCaseSearch' n a (GFound n (Rep a)) => GBaseCaseSearch n a

genericBCS
  :: forall a n proxy
  . (Generic a, GBaseCaseSearch n a)
  => proxy n -> (If (GFound n (Rep a)) Gen Proxy a)
genericBCS = gbcs' (Proxy :: Proxy (GFound n (Rep a)))

class Found n a ~ b => BaseCaseSearching_ n a b where
  baseCaseSearching_ :: proxy b -> proxy2 n -> (Gen a)

instance (Found n a ~ 'True, BaseCaseSearch n a) => BaseCaseSearching_ n a 'True where
  baseCaseSearching_ _ = baseCaseSearch

instance (Found n a ~ 'False, BaseCaseSearching (n + 1) a) => BaseCaseSearching_ n a 'False where
  baseCaseSearching_ _ _ = baseCaseSearching (Proxy :: Proxy (n + 1))

class BaseCaseSearching_ n a (Found n a) => BaseCaseSearching n a
instance (BaseCaseSearch n a, BaseCaseSearching_ n a (Found n a)) => BaseCaseSearching n a

class BaseCaseSearching 0 a => BaseCase a
instance BaseCaseSearching 0 a => BaseCase a

baseCaseSearching :: forall n a proxy. BaseCaseSearching n a => proxy n -> (Gen a)
baseCaseSearching = baseCaseSearching_ (Proxy :: Proxy (Found n a))

baseCase :: forall a. BaseCase a => Gen a
baseCase = baseCaseSearching (Proxy :: Proxy 0)
