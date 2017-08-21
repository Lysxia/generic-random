{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Random.Internal.BaseCase where

import Control.Applicative
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Test.QuickCheck

import Generic.Random.Internal.Generic

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

withBaseCase
  :: Gen a  -- ^ Default generator
  -> Gen a  -- ^ Base case
  -> Gen a
withBaseCase def bc = sized $ \sz ->
  if sz > 0 then def else bc

type family Found a (z :: Nat) :: Maybe Nat
type GFound a z = GFound' (Rep a) z

class BaseCaseSearch (a :: *) (z :: Nat) (e :: *) where
  baseCaseSearch :: b ~ Found a z => proxy '(z, e) -> IfM b Gen Proxy a

  default baseCaseSearch
    :: (b ~ GFound' (Rep a) z, GBaseCaseSearch a z e)
    => proxy '(z, e) -> IfM b Gen Proxy a
  baseCaseSearch = gBaseCaseSearch

type instance Found Int z = 'Just 0
instance BaseCaseSearch Int z e where
  baseCaseSearch _ = arbitrary

type instance Found () z = 'Just 0
instance BaseCaseSearch () z e where
  baseCaseSearch _ = arbitrary

type instance Found Bool z = 'Just 0
instance BaseCaseSearch Bool z e where
  baseCaseSearch _ = arbitrary

type instance Found [a] z = 'Just 0
instance BaseCaseSearch [a] z e where
  baseCaseSearch _ = return []

type instance Found (Either a b) z = Found' a z ||? Found' b z
instance GBaseCaseSearch (Either a b) z e => BaseCaseSearch (Either a b) z e where
  baseCaseSearch = gBaseCaseSearch


instance {-# INCOHERENT #-}
  ( TypeError
      (     'Text "Could not find instance for ("
      ':<>: 'ShowType (BaseCaseSearch a z e)
      ':<>: 'Text ")"
      ':$$: 'Text "If the type ("
      ':<>: 'ShowType a
      ':<>: 'Text ") is a type variable,"
      ':$$: 'Text "    you may be missing a (BaseCase ("
      ':<>: 'ShowType (BaseCase e)
      ':<>: 'Text ") constraint"
      ':$$: 'Text "Otherwise, if it starts with a type constructor,"
      ':$$: 'Text "    you may need to add an instance of BaseCaseSearch for it"
    )
  ) => BaseCaseSearch a z e where
  baseCaseSearch = error "Type error"

class Found a z ~ b => BaseCaseSearching_ a z b where
  baseCaseSearching_ :: proxy b -> proxy2 '(z, a) -> (Gen a)

instance (Found a z ~ 'Just m, BaseCaseSearch a z a) => BaseCaseSearching_ a z ('Just m) where
  baseCaseSearching_ _ = baseCaseSearch

instance (Found a z ~ 'Nothing, BaseCaseSearching a (z + 1)) => BaseCaseSearching_ a z 'Nothing where
  baseCaseSearching_ _ _ = baseCaseSearching (Proxy :: Proxy '(z + 1, a))

class BaseCaseSearching_ a z (Found a z) => BaseCaseSearching a z
instance (BaseCaseSearch a z a, BaseCaseSearching_ a z (Found a z)) => BaseCaseSearching a z

class BaseCaseSearching a 0 => BaseCase a
instance BaseCaseSearching a 0 => BaseCase a

baseCaseSearching :: forall z a proxy. BaseCaseSearching a z => proxy '(z, a) -> (Gen a)
baseCaseSearching = baseCaseSearching_ (Proxy :: Proxy (Found a z))

baseCase :: forall a. BaseCase a => Gen a
baseCase = baseCaseSearching (Proxy :: Proxy '(0, a))


type family IfM (b :: Maybe t) (c :: k) (d :: k) :: k
type instance IfM ('Just t) c d = c
type instance IfM 'Nothing c d = d

type Found' a z = NotFoundIfZ (z == 0) a z

type family NotFoundIfZ (b :: Bool) (c :: *) (z :: Nat) :: Maybe Nat
type instance NotFoundIfZ 'True c z = 'Nothing
type instance NotFoundIfZ 'False c z = Found c (z - 1)

type family GFound' (f :: k -> *) (z :: Nat) :: Maybe Nat
type instance GFound' (M1 i c f) z = GFound' f z
type instance GFound' (f :+: g) z = GFound' f z ||? GFound' g z
type instance GFound' (f :*: g) z = GFound' f z &&? GFound' g z
type instance GFound' (K1 i a) z = Found' a z
type instance GFound' U1 z = 'Just 0

type (==) m n = IsEQ (CmpNat m n)

type family IsEQ (e :: Ordering) :: Bool
type instance IsEQ 'EQ = 'True
type instance IsEQ 'GT = 'False
type instance IsEQ 'LT = 'False

type family (||?) (b :: Maybe Nat) (c :: Maybe Nat) :: Maybe Nat
type instance 'Just m ||? 'Just n = 'Just (Min m n)
type instance m ||? 'Nothing = m
type instance 'Nothing ||? n = n

type family (&&?) (b :: Maybe Nat) (c :: Maybe Nat) :: Maybe Nat
type instance 'Just m &&? 'Just n = 'Just (Max m n)
type instance m &&? 'Nothing = 'Nothing
type instance 'Nothing &&? n = 'Nothing

type Max m n = MaxOf (CmpNat m n) m n

type family MaxOf (e :: Ordering) (m :: k) (n :: k) :: k
type instance MaxOf 'GT m n = m
type instance MaxOf 'EQ m n = m
type instance MaxOf 'LT m n = n

type Min m n = MinOf (CmpNat m n) m n

type family MinOf (e :: Ordering) (m :: k) (n :: k) :: k
type instance MinOf 'GT m n = n
type instance MinOf 'EQ m n = n
type instance MinOf 'LT m n = m

type family IsJust (b :: Maybe t) :: Bool
type instance IsJust ('Just t) = 'True
type instance IsJust 'Nothing = 'False

class Alternative (IfM (GFound' f z) Weighted Proxy)
  => GBCS (f :: k -> *) (z :: Nat) (e :: *) where
  gbcs :: b ~ GFound' f z => proxy '(z, e) -> IfM b Weighted Proxy (f p)

instance GBCS f z e => GBCS (M1 i c f) z e where
  gbcs = (fmap . fmap) M1 gbcs

instance GBCSSum f g z e (GFound' f z) (GFound' g z) => GBCS (f :+: g) z e where
  gbcs = gbcsSum (Proxy :: Proxy '(GFound' f z, GFound' g z))

class Alternative (IfM (b ||? c) Weighted Proxy) => GBCSSum f g z e b c where
  gbcsSum :: proxy0 '(b, c) -> proxy '(z, e) -> IfM (b ||? c) Weighted Proxy ((f :+: g) p)

instance GBCSSum f g z e 'Nothing 'Nothing where
  gbcsSum _ = const Proxy

instance (GBCS f z e, GFound' f z ~ 'Just m) => GBCSSum f g z e ('Just m) 'Nothing where
  gbcsSum _ = (fmap . fmap) L1 gbcs

instance (GBCS g z e, GFound' g z ~ 'Just n) => GBCSSum f g z e 'Nothing ('Just n) where
  gbcsSum _ = (fmap . fmap) R1 gbcs

instance GBCSSumCompare f g z e m n (CmpNat m n)
  => GBCSSum f g z e ('Just m) ('Just n) where
  gbcsSum = gbcsSumCompare (Proxy :: Proxy (CmpNat m n))

class GBCSSumCompare f g z e m n o where
  gbcsSumCompare
    :: proxy1 o -> proxy0 '( 'Just m, 'Just n) -> proxy '(z, e)
    -> IfM ('Just (MinOf o m n)) Weighted Proxy ((f :+: g) p)

instance GBCSProduct f g z e (GFound' f z) (GFound' g z) => GBCS (f :*: g) z e where
  gbcs = gbcsProduct (Proxy :: Proxy '(GFound' f z, GFound' g z))

class Alternative (IfM (b &&? c) Weighted Proxy) => GBCSProduct f g z e b c where
  gbcsProduct :: proxy0 '(b, c) -> proxy '(z, e) -> IfM (b &&? c) Weighted Proxy ((f :*: g) p)

instance {-# OVERLAPPABLE #-} (b &&? c ~ 'Nothing) => GBCSProduct f g z e b c where
  gbcsProduct _ = const Proxy

instance (GBCS f z e, GBCS g z e, GFound' f z ~ 'Just m, GFound' g z ~ 'Just n)
  => GBCSProduct f g z e ('Just m) ('Just n) where
  gbcsProduct _ = (liftA2 . liftA2) (:*:) gbcs gbcs

class IsMaybe b where
  ifMmap :: proxy b -> (c a -> c' a') -> (d a -> d' a') -> IfM b c d a -> IfM b c' d' a'

instance IsMaybe ('Just t) where
  ifMmap _ f _ a = f a

instance IsMaybe 'Nothing where
  ifMmap _ _ g a = g a

instance {-# OVERLAPPABLE #-}
  ( BaseCaseSearch c (z - 1) e
  , b ~ GFound' (K1 i c) z
  , (z == 0) ~ 'False
  , Alternative (IfM b Weighted Proxy)
  , IsMaybe b
  ) => GBCS (K1 i c) z e where
  gbcs _ =
    fmap K1
      (ifMmap (Proxy :: Proxy b)
        liftGen
        (id :: Proxy c -> Proxy c)
        (baseCaseSearch (Proxy :: Proxy '(z - 1, e))))

instance GBCS (K1 i c) 0 e where
  gbcs = const empty

instance GBCS U1 z e where
  gbcs = const (pure U1)

class GBaseCaseSearch' a z e b where
  gbcs' :: proxy b -> proxy2 '(z, e) -> IfM b Gen Proxy a

instance GBaseCaseSearch' a z e 'Nothing where
  gbcs' = const (const Proxy)

instance (Generic a, GBCS (Rep a) z e, GFound' (Rep a) z ~ 'Just m)
  => GBaseCaseSearch' a z e ('Just m) where
  gbcs' = const (fmap (\(Weighted (Just (g, n))) -> choose (0, n-1) >>= fmap to . g) gbcs)

class (Generic a, GBCS (Rep a) z e, GBaseCaseSearch' a z e (GFound' (Rep a) z))
  => GBaseCaseSearch a z e
instance (Generic a, GBCS (Rep a) z e, GBaseCaseSearch' a z e (GFound' (Rep a) z))
  => GBaseCaseSearch a z e

gBaseCaseSearch
  :: forall z a e proxy
  . GBaseCaseSearch a z e
  => proxy '(z, e) -> (IfM (GFound' (Rep a) z) Gen Proxy a)
gBaseCaseSearch = gbcs' (Proxy :: Proxy (GFound' (Rep a) z))
