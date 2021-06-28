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
    TypeLevelGenList (..),
    TypeLevelOpts (..),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics hiding (S, prec)
import GHC.TypeLits (KnownNat, natVal)
import Generic.Random.Internal.Generic
import Test.QuickCheck (Arbitrary (..), Gen)

-- * Random generators (to be  used with DerivingVia)

-- | Pick a constructor with a given distribution, and fill its fields
-- with recursive calls to 'arbitrary'.
--
-- === Example
--
-- > deriving Arbitrary '[2, 3, 5] via (GenericArbitrary X)
--
-- Picks the first constructor with probability @2/10@,
-- the second with probability @3/10@, the third with probability @5/10@.
--
-- Use 'genericArbitrary'.
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
-- Use 'genericArbitraryU'.
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

-- | 'arbitrary' for types with one constructor.
-- Equivalent to 'GenericArbitraryU', with a stricter type.
--
-- Use 'genericArbitrarySingle'.
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
-- > deriving Arbitrary '[2, 3, 5] via (GenericArbitraryRec X)
--
-- N.B.: This replaces the generator for fields of type @[t]@ with
-- @'listOf'' arbitrary@ instead of @'Test.QuickCheck.listOf' arbitrary@ (i.e., @arbitrary@ for
-- lists).
--
-- Use 'genericArbitraryRec'.
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
-- > deriving Arbitrary '[2, 3, 5] via (GenericArbitraryG CustomGens X)
--
-- where, the generators for 'String' and 'Int' fields are overridden as
-- follows, for example:
--
-- @
-- type CustomGens :: CustomString ':+' CustomInt
-- @
--
-- === Note on multiple matches
--
-- Multiple generators may match a given field: the first will be chosen.
--
-- Use 'genericArbitraryG'.
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
-- Use 'genericArbitraryUG'.
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
-- Use 'genericArbitrarySingleG'.
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
-- Use 'genericArbitraryRecG'.
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
-- Use 'genericArbitraryWith'.
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

-- * Internal

type TypeLevelWeights' weights a = TypeLevelWeights weights (Weights_ (Rep a))

typeLevelWeights ::
  forall weights a.
  TypeLevelWeights weights (Weights_ (Rep a)) =>
  Weights a
typeLevelWeights =
  let (w, n) = typeLevelWeightsBuilder @weights
   in Weights w n

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
  TypeLevelWeights (w ': ws) (t :| (u :| v)) =>
  TypeLevelWeights (w ': ws) ((t :| u) :| v)
  where
  typeLevelWeightsBuilder =
    let (N t nt (N u nu v), m) = typeLevelWeightsBuilder @(w ': ws) @(t :| (u :| v))
     in (N (N t nt u) (nt + nu) v, m)

instance TypeLevelWeights '[] () where
  typeLevelWeightsBuilder = ((), 1)

class TypeLevelGenList a where
  type TypeLevelGenList' a :: Type
  toGenList :: Proxy a -> TypeLevelGenList' a

instance Arbitrary a => TypeLevelGenList (Gen a) where
  type TypeLevelGenList' (Gen a) = Gen a
  toGenList _ = arbitrary

instance (TypeLevelGenList a, TypeLevelGenList b) => TypeLevelGenList (a :+ b) where
  type TypeLevelGenList' (a :+ b) = TypeLevelGenList' a :+ TypeLevelGenList' b
  toGenList _ = toGenList (Proxy @a) :+ toGenList (Proxy @b)

class TypeLevelOpts a where
  type TypeLevelOpts' a :: Type
  toOpts :: Proxy a -> TypeLevelOpts' a
