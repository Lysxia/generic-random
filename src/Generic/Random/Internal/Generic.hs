{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
#endif

module Generic.Random.Internal.Generic where

import Control.Applicative
import Data.Coerce
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind
#endif
import Data.Proxy
#if __GLASGOW_HASKELL__ >= 800
import GHC.Generics hiding (S)
#else
import GHC.Generics hiding (S, Arity)
#endif
import GHC.TypeLits
import Test.QuickCheck

#if __GLASGOW_HASKELL__ < 800
type Type = *
#endif

-- * Random generators

-- | Pick a constructor with a given distribution, and fill its fields
-- with recursive calls to 'arbitrary'.
--
-- === Example
--
-- > genericArbitrary (2 % 3 % 5 % ()) :: Gen a
--
-- Picks the first constructor with probability @2/10@,
-- the second with probability @3/10@, the third with probability @5/10@.
genericArbitrary
  :: (Generic a, GA UnsizedOpts (Rep a))
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitrary = genericArbitraryWith unsizedOpts

-- | Pick every constructor with equal probability.
-- Equivalent to @'genericArbitrary' 'uniform'@.
--
-- > genericArbitraryU :: Gen a
genericArbitraryU
  :: (Generic a, GA UnsizedOpts (Rep a), UniformWeight_ (Rep a))
  => Gen a
genericArbitraryU = genericArbitrary uniform

-- | 'arbitrary' for types with one constructor.
-- Equivalent to 'genericArbitraryU', with a stricter type.
--
-- > genericArbitrarySingle :: Gen a
genericArbitrarySingle
  :: (Generic a, GA UnsizedOpts (Rep a), Weights_ (Rep a) ~ L c0)
  => Gen a
genericArbitrarySingle = genericArbitraryU

-- | Decrease size at every recursive call, but don't do anything different
-- at size 0.
--
-- > genericArbitraryRec (7 % 11 % 13 % ()) :: Gen a
genericArbitraryRec
  :: (Generic a, GA SizedOpts (Rep a))
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRec = genericArbitraryWith sizedOpts

-- | 'genericArbitrary' with explicit generators.
genericArbitraryG
  :: (Generic a, GA (SetGens g UnsizedOpts) (Rep a))
  => GenList g
  -> Weights a
  -> Gen a
genericArbitraryG gs = genericArbitraryWith opts
  where
    opts = setGenerators gs unsizedOpts

-- | 'genericArbitraryU' with explicit generators.
genericArbitraryUG
  :: (Generic a, GA (SetGens g UnsizedOpts) (Rep a), UniformWeight_ (Rep a))
  => GenList g
  -> Gen a
genericArbitraryUG gs = genericArbitraryG gs uniform

-- | 'genericArbitrarySingle' with explicit generators.
genericArbitrarySingleG
  :: (Generic a, GA (SetGens g UnsizedOpts) (Rep a), Weights_ (Rep a) ~ L c0)
  => GenList g
  -> Gen a
genericArbitrarySingleG = genericArbitraryUG

-- | 'genericArbitraryRec' with explicit generators.
genericArbitraryRecG
  :: (Generic a, GA (SetGens g SizedOpts) (Rep a))
  => GenList g
  -> Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRecG gs = genericArbitraryWith opts
  where
    opts = setGenerators gs sizedOpts

-- | General generic generator with custom options.
genericArbitraryWith
  :: (Generic a, GA opts (Rep a))
  => opts -> Weights a -> Gen a
genericArbitraryWith opts (Weights w n) =
  fmap to (ga opts w n)

-- * Internal

type family Weights_ (f :: * -> *) :: * where
  Weights_ (f :+: g) = Weights_ f :| Weights_ g
  Weights_ (M1 D _c f) = Weights_ f
#if __GLASGOW_HASKELL__ >= 800
  Weights_ (M1 C ('MetaCons c _i _j) _f) = L c
#else
  Weights_ (M1 C _c _f) = L ""
#endif

data a :| b = N a Int b
data L (c :: Symbol) = L

-- | Trees of weights assigned to constructors of type @a@,
-- rescaled to obtain a probability distribution.
--
-- Two ways of constructing them.
--
-- @
-- (x1 '%' x2 '%' ... '%' xn '%' ()) :: 'Weights' a
-- 'uniform' :: 'Weights' a
-- @
--
-- Using @('%')@, there must be exactly as many weights as
-- there are constructors.
--
-- 'uniform' is equivalent to @(1 '%' ... '%' 1 '%' ())@
-- (automatically fills out the right number of 1s).
data Weights a = Weights (Weights_ (Rep a)) Int

-- | Type of a single weight, tagged with the name of the associated
-- constructor for additional compile-time checking.
--
-- @
-- ((9 :: 'W' \"Leaf\") '%' (8 :: 'W' \"Node\") '%' ())
-- @
newtype W (c :: Symbol) = W Int deriving Num

{-# DEPRECATED weights "Can be omitted" #-}
-- | A smart constructor to specify a custom distribution.
weights :: (Weights_ (Rep a), Int, ()) -> Weights a
weights (w, n, ()) = Weights w n

-- | Uniform distribution.
uniform :: UniformWeight_ (Rep a) => Weights a
uniform =
  let (w, n) = uniformWeight
  in Weights w n

type family First a :: Symbol where
  First (a :| _b) = First a
  First (L c) = c

type family First' w where
  First' (Weights a) = First (Weights_ (Rep a))
  First' (a, Int, r) = First a

type family Prec' w where
  Prec' (Weights a) = Prec (Weights_ (Rep a)) ()
  Prec' (a, Int, r) = Prec a r

class WeightBuilder' w where

  -- | A binary constructor for building up trees of weights.
  (%) :: W (First' w) -> Prec' w -> w

instance WeightBuilder (Weights_ (Rep a)) => WeightBuilder' (Weights a) where
  w % prec = weights (w %. prec)

instance WeightBuilder a => WeightBuilder' (a, Int, r) where
  (%) = (%.)

class WeightBuilder a where
  type Prec a r

  (%.) :: W (First a) -> Prec a r -> (a, Int, r)

infixr 1 %

instance WeightBuilder a => WeightBuilder (a :| b) where
  type Prec (a :| b) r = Prec a (b, Int, r)
  m %. prec =
    let (a, n, (b, p, r)) = m % prec
    in (N a n b, n + p, r)

instance WeightBuilder (L c) where
  type Prec (L c) r = r
  W m %. prec = (L, m, prec)

instance WeightBuilder () where
  type Prec () r = r
  W m %. prec = ((), m, prec)

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

class UniformWeight (Weights_ f) => UniformWeight_ f
instance UniformWeight (Weights_ f) => UniformWeight_ f


-- | Type-level options for 'GA'.
data Options (s :: Sizing) (g :: [Type]) = Options
  { _generators :: GenList g
  }

unsizedOpts :: UnsizedOpts
unsizedOpts = Options Nil

sizedOpts :: SizedOpts
sizedOpts = Options Nil


-- | Whether to decrease the size parameter before generating fields.
data Sizing = Sized | Unsized

type UnsizedOpts = (Options 'Unsized '[] :: Type)
type SizedOpts = (Options 'Sized '[] :: Type)

type family SizingOf opts :: Sizing
type instance SizingOf (Options s _) = s

proxySizing :: opts -> Proxy (SizingOf opts)
proxySizing _ = Proxy

setSized :: Options s g -> Options 'Sized g
setSized = coerce

setUnsized :: Options s g -> Options 'Unsized g
setUnsized = coerce


data GenList (g :: [Type]) where
  Nil :: GenList '[]
  (:@) :: Gen a -> GenList g -> GenList (a ': g)

infixr 3 :@

type family GeneratorsOf opts :: [Type]
type instance GeneratorsOf (Options _ g) = g

class HasGenerators opts where
  generators :: opts -> GenList (GeneratorsOf opts)

instance HasGenerators (Options s g) where
  generators = _generators

setGenerators :: GenList g -> Options s g0 -> Options s g
setGenerators gens (Options _) = Options gens


type family SetGens (g :: [Type]) opts
type instance SetGens g (Options s _g) = Options s g

-- | Generic Arbitrary
class GA opts f where
  ga :: opts -> Weights_ f -> Int -> Gen (f p)

instance GA opts f => GA opts (M1 D c f) where
  ga z w n = fmap M1 (ga z w n)

instance (GASum opts f, GASum opts g) => GA opts (f :+: g) where
  ga = gaSum'

instance GAProduct (SizingOf opts) opts f => GA opts (M1 C c f) where
  ga z _ _ = fmap M1 (gaProduct (proxySizing z) z)

#if __GLASGOW_HASKELL__ >= 800
instance {-# INCOHERENT #-}
  TypeError
    (     'Text "Unrecognized Rep: "
    ':<>: 'ShowType f
    ':$$: 'Text "Possible cause: missing Generic instance"
    )
  => GA opts f where
  ga = error "Type error"
#endif

gaSum' :: GASum opts f => opts -> Weights_ f -> Int -> Gen (f p)
gaSum' z w n = do
  i <- choose (0, n-1)
  gaSum z i w

class GASum opts f where
  gaSum :: opts -> Int -> Weights_ f -> Gen (f p)

instance (GASum opts f, GASum opts g) => GASum opts (f :+: g) where
  gaSum z i (N a n b)
    | i < n = fmap L1 (gaSum z i a)
    | otherwise = fmap R1 (gaSum z (i - n) b)

instance GAProduct (SizingOf opts) opts f => GASum opts (M1 i c f) where
  gaSum z _ _ = fmap M1 (gaProduct (proxySizing z) z)


class GAProduct (s :: Sizing) opts f where
  gaProduct :: proxys s -> opts -> Gen (f p)

instance GAProduct' opts f => GAProduct 'Unsized opts f where
  gaProduct _ = gaProduct'

instance (GAProduct' opts f, KnownNat (Arity f)) => GAProduct 'Sized opts f where
  gaProduct _ opts = sized $ \n -> resize (n `div` arity) (gaProduct' opts)
    where
      arity = fromInteger (natVal (Proxy :: Proxy (Arity f)))

instance {-# OVERLAPPING #-} GAProduct 'Sized opts U1 where
  gaProduct _ _ = pure U1


class GAProduct' opts f where
  gaProduct' :: opts -> Gen (f p)

instance GAProduct' opts U1 where
  gaProduct' _ = pure U1

instance (HasGenerators opts, ArbitraryOr (GeneratorsOf opts) c)
  => GAProduct' opts (K1 i c) where
  gaProduct' opts = fmap K1 (arbitraryOr (generators opts))

instance (GAProduct' opts f, GAProduct' opts g) => GAProduct' opts (f :*: g) where
  gaProduct' = (liftA2 . liftA2) (:*:) gaProduct' gaProduct'

instance GAProduct' opts f => GAProduct' opts (M1 i c f) where
  gaProduct' = (fmap . fmap) M1 gaProduct'


type family Arity f :: Nat where
  Arity (f :*: g) = Arity f + Arity g
  Arity (M1 _i _c _f) = 1


class ArbitraryOr (g :: [Type]) a where
  arbitraryOr :: GenList g -> Gen a

instance {-# INCOHERENT #-} ArbitraryOr (a ': g) a where
  arbitraryOr (gen :@ _) = gen

instance {-# OVERLAPPABLE #-} ArbitraryOr g a => ArbitraryOr (b ': g) a where
  arbitraryOr (_ :@ gens) = arbitraryOr gens

instance Arbitrary a => ArbitraryOr '[] a where
  arbitraryOr Nil = arbitrary


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

