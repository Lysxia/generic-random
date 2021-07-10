{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Generic.Random.DerivingVia
  ( GenericArbitrary (..),
    GenericArbitraryU (..),
    GenericArbitrarySingle (..),
    GenericArbitraryRec (..),
    GenericArbitraryG (..),
    GenericArbitraryUG (..),
    GenericArbitrarySingleG (..),
    GenericArbitraryRecG (..),
    GenericArbitraryWith (..),
    AndShrinking (..),
    TypeLevelGenList (..),
    TypeLevelOpts (..),
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic(..))
import GHC.TypeLits (KnownNat, natVal)
import Generic.Random.Internal.Generic
import Test.QuickCheck (Arbitrary (..), Gen, genericShrink)
import Test.QuickCheck.Arbitrary (RecursivelyShrink, GSubterms)

-- * Newtypes for DerivingVia

-- | Pick a constructor with a given distribution, and fill its fields
-- with recursive calls to 'Test.QuickCheck.arbitrary'.
--
-- === Example
--
-- > data X = ...
-- >   deriving Arbitrary via (GenericArbitrary '[2, 3, 5] X)
--
-- Picks the first constructor with probability @2/10@,
-- the second with probability @3/10@, the third with probability @5/10@.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitrary'.
--
-- @since 1.5.0.0
newtype GenericArbitrary weights a = GenericArbitrary {unGenericArbitrary :: a} deriving (Eq, Show)

instance
  ( GArbitrary UnsizedOpts a,
    TypeLevelWeights' weights a
  ) =>
  Arbitrary (GenericArbitrary weights a)
  where
  arbitrary = GenericArbitrary <$> genericArbitrary (typeLevelWeights @weights)

-- | Pick every constructor with equal probability.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryU'.
--
-- @since 1.5.0.0
newtype GenericArbitraryU a = GenericArbitraryU {unGenericArbitraryU :: a} deriving (Eq, Show)

instance
  ( GArbitrary UnsizedOpts a,
    GUniformWeight a
  ) =>
  Arbitrary (GenericArbitraryU a)
  where
  arbitrary = GenericArbitraryU <$> genericArbitraryU

-- | @arbitrary@ for types with one constructor.
-- Equivalent to 'GenericArbitraryU', with a stricter type.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitrarySingle'.
--
-- @since 1.5.0.0
newtype GenericArbitrarySingle a = GenericArbitrarySingle {unGenericArbitrarySingle :: a} deriving (Eq, Show)

instance
  ( GArbitrary UnsizedOpts a,
    Weights_ (Rep a) ~ L c0
  ) =>
  Arbitrary (GenericArbitrarySingle a)
  where
  arbitrary = GenericArbitrarySingle <$> genericArbitrarySingle

-- | Decrease size at every recursive call, but don't do anything different
-- at size 0.
--
-- > data X = ...
-- >   deriving Arbitrary via (GenericArbitraryRec '[2, 3, 5] X)
--
-- N.B.: This replaces the generator for fields of type @[t]@ with
-- @'listOf'' arbitrary@ instead of @'Test.QuickCheck.listOf' arbitrary@ (i.e., @arbitrary@ for
-- lists).
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryRec'.
--
-- @since 1.5.0.0
newtype GenericArbitraryRec weights a = GenericArbitraryRec {unGenericArbitraryRec :: a} deriving (Eq, Show)

instance
  ( GArbitrary SizedOptsDef a,
    TypeLevelWeights' weights a
  ) =>
  Arbitrary (GenericArbitraryRec weights a)
  where
  arbitrary = GenericArbitraryRec <$> genericArbitraryRec (typeLevelWeights @weights)

-- | 'GenericArbitrary' with explicit generators.
--
-- === Example
--
-- > data X = ...
-- >   deriving Arbitrary via (GenericArbitraryG CustomGens '[2, 3, 5] X)
--
-- where, for example, custom generators to override 'String' and 'Int' fields
-- might look as follows:
--
-- @
-- type CustomGens = CustomString ':+' CustomInt
-- @
--
-- === Note on multiple matches
--
-- Multiple generators may match a given field: the first will be chosen.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryG'.
--
-- @since 1.5.0.0
newtype GenericArbitraryG genList weights a = GenericArbitraryG {unGenericArbitraryG :: a} deriving (Eq, Show)

instance
  ( GArbitrary (SetGens genList UnsizedOpts) a,
    GUniformWeight a,
    TypeLevelWeights' weights a,
    TypeLevelGenList genList',
    genList ~ TypeLevelGenList' genList'
  ) =>
  Arbitrary (GenericArbitraryG genList' weights a)
  where
  arbitrary = GenericArbitraryG <$> genericArbitraryG (toGenList $ Proxy @genList') (typeLevelWeights @weights)

-- | 'GenericArbitraryU' with explicit generators.
-- See also 'GenericArbitraryG'.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryUG'.
--
-- @since 1.5.0.0
newtype GenericArbitraryUG genList a = GenericArbitraryUG {unGenericArbitraryUG :: a} deriving (Eq, Show)

instance
  ( GArbitrary (SetGens genList UnsizedOpts) a,
    GUniformWeight a,
    TypeLevelGenList genList',
    genList ~ TypeLevelGenList' genList'
  ) =>
  Arbitrary (GenericArbitraryUG genList' a)
  where
  arbitrary = GenericArbitraryUG <$> genericArbitraryUG (toGenList $ Proxy @genList')

-- | 'genericArbitrarySingle' with explicit generators.
-- See also 'GenericArbitraryG'.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitrarySingleG'.
--
-- @since 1.5.0.0
newtype GenericArbitrarySingleG genList a = GenericArbitrarySingleG {unGenericArbitrarySingleG :: a} deriving (Eq, Show)

instance
  ( GArbitrary (SetGens genList UnsizedOpts) a,
    Weights_ (Rep a) ~ L c0,
    TypeLevelGenList genList',
    genList ~ TypeLevelGenList' genList'
  ) =>
  Arbitrary (GenericArbitrarySingleG genList' a)
  where
  arbitrary = GenericArbitrarySingleG <$> genericArbitrarySingleG (toGenList $ Proxy @genList')

-- | 'genericArbitraryRec' with explicit generators.
-- See also 'genericArbitraryG'.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryRecG'.
--
-- @since 1.5.0.0
newtype GenericArbitraryRecG genList weights a = GenericArbitraryRecG {unGenericArbitraryRecG :: a} deriving (Eq, Show)

instance
  ( GArbitrary (SetGens genList SizedOpts) a,
    TypeLevelWeights' weights a,
    TypeLevelGenList genList',
    genList ~ TypeLevelGenList' genList'
  ) =>
  Arbitrary (GenericArbitraryRecG genList' weights a)
  where
  arbitrary = GenericArbitraryRecG <$> genericArbitraryRecG (toGenList $ Proxy @genList') (typeLevelWeights @weights)

-- | General generic generator with custom options.
--
-- This newtype does no shrinking. To add generic shrinking, use 'AndShrinking'.
--
-- Uses 'genericArbitraryWith'.
--
-- @since 1.5.0.0
newtype GenericArbitraryWith opts weights a = GenericArbitraryWith {unGenericArbitraryWith :: a} deriving (Eq, Show)

instance
  ( GArbitrary opts a,
    TypeLevelWeights' weights a,
    TypeLevelOpts opts',
    opts ~ TypeLevelOpts' opts'
  ) =>
  Arbitrary (GenericArbitraryWith opts' weights a)
  where
  arbitrary = GenericArbitraryWith <$> genericArbitraryWith (toOpts $ Proxy @opts') (typeLevelWeights @weights)

-- | Add generic shrinking to a newtype wrapper for 'Arbitrary', using 'genericShrink'.
--
-- @
-- data X = ...
--   deriving Arbitrary via ('GenericArbitrary' '[1,2,3] `'AndShrinking'` X)
-- @
--
-- Equivalent to:
--
-- @
-- instance Arbitrary X where
--   arbitrary = 'genericArbitrary' (1 % 2 % 3 % ())
--   shrink = 'Test.QuickCheck.genericShrink'
-- @
--
-- @since 1.5.0.0
newtype AndShrinking f a = AndShrinking a deriving (Eq, Show)

instance
  ( Arbitrary (f a), Coercible (f a) a, Generic a, RecursivelyShrink (Rep a), GSubterms (Rep a) a
  ) => Arbitrary (AndShrinking f a) where
  arbitrary = coerce (arbitrary :: Gen (f a))
  shrink = coerce (genericShrink :: a -> [a])

-- * Internal

-- |
-- @since 1.5.0.0
type TypeLevelWeights' weights a = TypeLevelWeights weights (Weights_ (Rep a))

typeLevelWeights ::
  forall weights a.
  TypeLevelWeights weights (Weights_ (Rep a)) =>
  Weights a
typeLevelWeights =
  let (w, n) = typeLevelWeightsBuilder @weights
   in Weights w n

-- |
-- @since 1.5.0.0
class TypeLevelWeights weights a where
  typeLevelWeightsBuilder :: (a, Int)

instance
  ( KnownNat weight,
    TypeLevelWeights weights a
  ) =>
  TypeLevelWeights (weight ': weights) (L x :| a)
  where
  typeLevelWeightsBuilder =
    let (a, m) = (L, fromIntegral $ natVal $ Proxy @weight)
        (b, n) = typeLevelWeightsBuilder @weights @a
     in (N a m b, m + n)

instance
  ( KnownNat weight
  ) =>
  TypeLevelWeights (weight ': '[]) (L x)
  where
  typeLevelWeightsBuilder = (L, fromIntegral $ natVal $ Proxy @weight)

instance
  TypeLevelWeights (w ': ws) (t :| (u :| v)) =>
  TypeLevelWeights (w ': ws) ((t :| u) :| v)
  where
  typeLevelWeightsBuilder =
    let (N t nt (N u nu v), m) = typeLevelWeightsBuilder @(w ': ws) @(t :| (u :| v))
     in (N (N t nt u) (nt + nu) v, m)

instance TypeLevelWeights '[] () where
  typeLevelWeightsBuilder = ((), 1)

-- |
-- @since 1.5.0.0
class TypeLevelGenList a where
  type TypeLevelGenList' a :: Type
  toGenList :: Proxy a -> TypeLevelGenList' a

instance Arbitrary a => TypeLevelGenList (Gen a) where
  type TypeLevelGenList' (Gen a) = Gen a
  toGenList _ = arbitrary

instance (TypeLevelGenList a, TypeLevelGenList b) => TypeLevelGenList (a :+ b) where
  type TypeLevelGenList' (a :+ b) = TypeLevelGenList' a :+ TypeLevelGenList' b
  toGenList _ = toGenList (Proxy @a) :+ toGenList (Proxy @b)

-- |
-- @since 1.5.0.0
class TypeLevelOpts a where
  type TypeLevelOpts' a :: Type
  toOpts :: Proxy a -> TypeLevelOpts' a
