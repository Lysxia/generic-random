{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
#endif

-- | Core implementation.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Random.Internal.Generic where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif
import Control.Applicative (Alternative(..), liftA2)
import Data.Coerce (coerce)
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#endif
import Data.Proxy (Proxy(..))
#if __GLASGOW_HASKELL__ >= 800
import GHC.Generics hiding (S, prec)
#else
import GHC.Generics hiding (S, Arity, prec)
#endif
import GHC.TypeLits (KnownNat, Nat, Symbol, type (+), natVal)
import Test.QuickCheck (Arbitrary(..), Gen, choose, scale, sized, vectorOf)

#if __GLASGOW_HASKELL__ < 800
#define Type *
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
  :: (GArbitrary UnsizedOpts a)
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitrary = genericArbitraryWith unsizedOpts

-- | Pick every constructor with equal probability.
-- Equivalent to @'genericArbitrary' 'uniform'@.
--
-- > genericArbitraryU :: Gen a
genericArbitraryU
  :: (GArbitrary UnsizedOpts a, GUniformWeight a)
  => Gen a
genericArbitraryU = genericArbitrary uniform

-- | 'arbitrary' for types with one constructor.
-- Equivalent to 'genericArbitraryU', with a stricter type.
--
-- > genericArbitrarySingle :: Gen a
genericArbitrarySingle
  :: (GArbitrary UnsizedOpts a, Weights_ (Rep a) ~ L c0)
  => Gen a
genericArbitrarySingle = genericArbitraryU

-- | Decrease size at every recursive call, but don't do anything different
-- at size 0.
--
-- > genericArbitraryRec (7 % 11 % 13 % ()) :: Gen a
--
-- N.B.: This replaces the generator for fields of type @[t]@ with
-- @'listOf'' arbitrary@ instead of @'Test.QuickCheck.listOf' arbitrary@ (i.e., @arbitrary@ for
-- lists).
genericArbitraryRec
  :: (GArbitrary SizedOptsDef a)
  => Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRec = genericArbitraryWith sizedOptsDef

-- | 'genericArbitrary' with explicit generators.
--
-- === Example
--
-- > genericArbitraryG customGens (17 % 19 % ())
--
-- where, the generators for 'String' and 'Int' fields are overridden as
-- follows, for example:
--
-- @
-- customGens :: Gen String ':+' Gen Int
-- customGens =
--   (filter (/= '\NUL') '<$>' arbitrary) ':+'
--   (getNonNegative '<$>' arbitrary)
-- @
--
-- === Note on multiple matches
--
-- Multiple generators may match a given field: the first will be chosen.
genericArbitraryG
  :: (GArbitrary (SetGens genList UnsizedOpts) a)
  => genList
  -> Weights a
  -> Gen a
genericArbitraryG gs = genericArbitraryWith opts
  where
    opts = setGenerators gs unsizedOpts

-- | 'genericArbitraryU' with explicit generators.
-- See also 'genericArbitraryG'.
genericArbitraryUG
  :: (GArbitrary (SetGens genList UnsizedOpts) a, GUniformWeight a)
  => genList
  -> Gen a
genericArbitraryUG gs = genericArbitraryG gs uniform

-- | 'genericArbitrarySingle' with explicit generators.
-- See also 'genericArbitraryG'.
genericArbitrarySingleG
  :: (GArbitrary (SetGens genList UnsizedOpts) a, Weights_ (Rep a) ~ L c0)
  => genList
  -> Gen a
genericArbitrarySingleG = genericArbitraryUG

-- | 'genericArbitraryRec' with explicit generators.
-- See also 'genericArbitraryG'.
genericArbitraryRecG
  :: (GArbitrary (SetGens genList SizedOpts) a)
  => genList
  -> Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitraryRecG gs = genericArbitraryWith opts
  where
    opts = setGenerators gs sizedOpts

-- | General generic generator with custom options.
genericArbitraryWith
  :: (GArbitrary opts a)
  => opts -> Weights a -> Gen a
genericArbitraryWith opts (Weights w n) =
  fmap to (ga opts w n)

-- * Internal

type family Weights_ (f :: Type -> Type) :: Type where
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
--
-- Note: these annotations are only checked on GHC 8.0 or newer. They are
-- ignored on older GHCs.
newtype W (c :: Symbol) = W Int deriving Num

-- | A smart constructor to specify a custom distribution.
-- It can be omitted for the '%' operator is overloaded to
-- insert it.
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

-- | A synonym for @(~)@, except on GHC 7.10 and older, where it's the trivial
-- constraint. See note on 'W'.
#if __GLASGOW_HASKELL__ >= 800
class (a ~ b) => a ~. b
instance (a ~ b) => a ~. b
#else
class a ~. b
instance a ~. b
#endif

class WeightBuilder' w where

  -- | A binary constructor for building up trees of weights.
  (%) :: (c ~. First' w) => W c -> Prec' w -> w

instance WeightBuilder (Weights_ (Rep a)) => WeightBuilder' (Weights a) where
  w % prec = weights (w %. prec)

instance WeightBuilder a => WeightBuilder' (a, Int, r) where
  (%) = (%.)

class WeightBuilder a where
  type Prec a r

  (%.) :: (c ~. First a) => W c -> Prec a r -> (a, Int, r)

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

-- | Derived uniform distribution of constructors for @a@.
class UniformWeight_ (Rep a) => GUniformWeight a
instance UniformWeight_ (Rep a) => GUniformWeight a


-- | Type-level options for 'GArbitrary'.
--
-- Note: it is recommended to avoid referring to the 'Options' type
-- explicitly in code, as the set of options may change in the future.
-- Instead, use the provided synonyms and setters:
--
-- - Synonyms: 'UnsizedOpts', 'SizedOpts', 'SizedOptsDef'
-- - Setters: 'SetSized', 'SetUnsized', 'SetGens'
newtype Options (s :: Sizing) (genList :: Type) = Options
  { _generators :: genList
  }

-- | Default options for unsized generators.
unsizedOpts :: UnsizedOpts
unsizedOpts = Options ()

-- | Default options for sized generators.
sizedOpts :: SizedOpts
sizedOpts = Options ()

-- | Default options overriding the list generator using 'listOf''.
sizedOptsDef :: SizedOptsDef
sizedOptsDef = Options (Gen1 listOf' :+ ())


-- | Whether to decrease the size parameter before generating fields.
--
-- The 'Sized' option makes the size parameter decrease in the following way:
-- - Constructors with one field decrease the size parameter by 1 to generate
--   that field.
-- - Constructors with more than one field split the size parameter among all
--   fields; the size parameter is rounded down to then be divided equally.
data Sizing
  = Sized     -- ^ Decrease the size parameter when running generators for fields
  | Unsized   -- ^ Don't touch the size parameter

type UnsizedOpts = Options 'Unsized ()
type SizedOpts = Options 'Sized ()
type SizedOptsDef = Options 'Sized (Gen1 [] :+ ())

type family SizingOf opts :: Sizing
type instance SizingOf (Options s _g) = s

type family SetSized (o :: Type) :: Type
type instance SetSized (Options s g) = Options 'Sized g

type family SetUnsized (o :: Type) :: Type
type instance SetUnsized (Options s g) = Options 'Unsized g

setSized :: Options s g -> Options 'Sized g
setSized = coerce

setUnsized :: Options s g -> Options 'Unsized g
setUnsized = coerce

-- | Heterogeneous list of generators.
data a :+ b = a :+ b

infixr 1 :+


type family GeneratorsOf opts :: Type
type instance GeneratorsOf (Options _s g) = g

class HasGenerators opts where
  generators :: opts -> GeneratorsOf opts

instance HasGenerators (Options s g) where
  generators = _generators

setGenerators :: genList -> Options s g0 -> Options s genList
setGenerators gens (Options _) = Options gens


type family SetGens (g :: Type) opts
type instance SetGens g (Options s _g) = Options s g

#if __GLASGOW_HASKELL__ >= 800
-- | Custom generator for record fields named @s@.
--
-- If there is a field named @s@ with a different type,
-- this will result in a type error.
--
-- /Available only for @base >= 4.9@ (@GHC >= 8.0.1@)./
newtype FieldGen (s :: Symbol) a = FieldGen { unFieldGen :: Gen a }

-- | 'FieldGen' constructor with the field name given via a proxy.
fieldGen :: proxy s -> Gen a -> FieldGen s a
fieldGen _ = FieldGen

-- | Custom generator for the @i@-th field of the constructor named @c@.
-- Fields are 0-indexed.
--
-- /Available only for @base >= 4.9@ (@GHC >= 8.0.1@)./
newtype ConstrGen (c :: Symbol) (i :: Nat) a = ConstrGen { unConstrGen :: Gen a }

-- | 'ConstrGen' constructor with the constructor name given via a proxy.
constrGen :: proxy '(c, i) -> Gen a -> ConstrGen c i a
constrGen _ = ConstrGen
#endif

-- | Custom generators for \"containers\" of kind @Type -> Type@, parameterized
-- by the generator for \"contained elements\".
--
-- A custom generator @'Gen1' f@ will be used for any field whose type has the
-- form @f x@, requiring a generator of @x@. The generator for @x@ will be
-- constructed using the list of custom generators if possible, otherwise
-- an instance @Arbitrary x@ will be required.
newtype Gen1 f = Gen1 { unGen1 :: forall a. Gen a -> Gen (f a) }

-- | Custom generators for unary type constructors that are not \"containers\",
-- i.e., which don't require a generator of @a@ to generate an @f a@.
--
-- A custom generator @'Gen1_' f@ will be used for any field whose type has the
-- form @f x@.
newtype Gen1_ f = Gen1_ { unGen1_ :: forall a. Gen (f a) }

-- | An alternative to 'vectorOf' that divides the size parameter by the
-- length of the list.
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' 0 = \_ -> pure []
vectorOf' i = scale (`div` i) . vectorOf i

-- | An alternative to 'Test.QuickCheck.listOf' that divides the size parameter
-- by the length of the list.
-- The length follows a geometric distribution of parameter
-- @1/(sqrt size + 1)@.
listOf' :: Gen a -> Gen [a]
listOf' g = sized $ \n -> do
  i <- geom n
  vectorOf' i g

-- | An alternative to 'Test.QuickCheck.listOf1' (nonempty lists) that divides
-- the size parameter by the length of the list.
-- The length (minus one) follows a geometric distribution of parameter
-- @1/(sqrt size + 1)@.
listOf1' :: Gen a -> Gen [a]
listOf1' g = liftA2 (:) g (listOf' g)

-- | Geometric distribution of parameter @1/(sqrt n + 1)@ (@n >= 0@).
geom :: Int -> Gen Int
geom 0 = pure 0
geom n = go 0 where
  n' = fromIntegral n
  p = 1 / (sqrt n' + 1) :: Double
  go r = do
    x <- choose (0, 1)
    if x < p then
      pure r
    else
      go $! (r + 1)

---

-- | Generic Arbitrary
class GA opts f where
  ga :: opts -> Weights_ f -> Int -> Gen (f p)

-- | Generic Arbitrary
class (Generic a, GA opts (Rep a)) => GArbitrary opts a
instance (Generic a, GA opts (Rep a)) => GArbitrary opts a

instance GA opts f => GA opts (M1 D c f) where
  ga z w n = fmap M1 (ga z w n)
  {-# INLINE ga #-}

instance (GASum opts f, GASum opts g) => GA opts (f :+: g) where
  ga = gaSum'
  {-# INLINE ga #-}

instance GAProduct (SizingOf opts) (Name c) opts f => GA opts (M1 C c f) where
  ga z _ _ = fmap M1 (gaProduct (Proxy :: Proxy '(SizingOf opts, Name c)) z)
  {-# INLINE ga #-}

gaSum' :: GASum opts f => opts -> Weights_ f -> Int -> Gen (f p)
gaSum' z w n = do
  i <- choose (0, n-1)
  gaSum z i w
{-# INLINE gaSum' #-}

class GASum opts f where
  gaSum :: opts -> Int -> Weights_ f -> Gen (f p)

instance (GASum opts f, GASum opts g) => GASum opts (f :+: g) where
  gaSum z i (N a n b)
    | i < n = fmap L1 (gaSum z i a)
    | otherwise = fmap R1 (gaSum z (i - n) b)
  {-# INLINE gaSum #-}

instance GAProduct (SizingOf opts) (Name c) opts f => GASum opts (M1 C c f) where
  gaSum z _ _ = fmap M1 (gaProduct (Proxy :: Proxy '(SizingOf opts, Name c)) z)
  {-# INLINE gaSum #-}


class GAProduct (s :: Sizing) (c :: Maybe Symbol) opts f where
  gaProduct :: proxys '(s, c) -> opts -> Gen (f p)

instance GAProduct' c 0 opts f => GAProduct 'Unsized c opts f where
  gaProduct _ = gaProduct' (Proxy :: Proxy '(c, 0))
  {-# INLINE gaProduct #-}

-- Single-field constructors: decrease size by 1.
instance {-# OVERLAPPING #-} GAProduct' c 0 opts (S1 d f)
  => GAProduct 'Sized c opts (S1 d f) where
  gaProduct _ = scale (\n -> max 0 (n-1)) . gaProduct' (Proxy :: Proxy '(c, 0))

instance (GAProduct' c 0 opts f, KnownNat (Arity f)) => GAProduct 'Sized c opts f where
  gaProduct _ = scale (`div` arity) . gaProduct' (Proxy :: Proxy '(c, 0))
    where
      arity = fromInteger (natVal (Proxy :: Proxy (Arity f)))
  {-# INLINE gaProduct #-}

instance {-# OVERLAPPING #-} GAProduct 'Sized c opts U1 where
  gaProduct _ _ = pure U1
  {-# INLINE gaProduct #-}


class GAProduct' (c :: Maybe Symbol) (i :: Nat) opts f where
  gaProduct' :: proxy '(c, i) -> opts -> Gen (f p)

instance GAProduct' c i opts U1 where
  gaProduct' _ _ = pure U1
  {-# INLINE gaProduct' #-}

instance
  ( HasGenerators opts
  , ArbitraryOr gs () gs '(c, i, Name d) a
  , gs ~ GeneratorsOf opts )
  => GAProduct' c i opts (S1 d (K1 _k a)) where
  gaProduct' _ opts = fmap (M1 . K1) (arbitraryOr sel gs () gs)
    where
      sel = Proxy :: Proxy '(c, i, Name d)
      gs = generators opts
  {-# INLINE gaProduct' #-}

instance (GAProduct' c i opts f, GAProduct' c (i + Arity f) opts g) => GAProduct' c i opts (f :*: g) where
  -- TODO: Why does this inline better than eta-reducing? (GHC-8.2)
  gaProduct' px = (liftA2 . liftA2) (:*:)
    (gaProduct' px)
    (gaProduct' (Proxy :: Proxy '(c, i + Arity f)))
  {-# INLINE gaProduct' #-}


type family Arity f :: Nat where
  Arity (f :*: g) = Arity f + Arity g
  Arity (M1 _i _c _f) = 1

-- | Given a list of custom generators @gs@, find one that applies, or use
-- @Arbitrary a@ by default.
--
-- @g@ and @gs@ follow this little state machine:
--
-- >           g,      gs | result
-- > ---------------------+-----------------------------
-- >          (),      () | END
-- >          (), g :+ gs | g, gs
-- >          (),      g  | g, () when g is not (_ :+ _)
-- >      g :+ h,      gs | g, h :+ gs
-- >       Gen a,      gs | END if matching, else (), gs
-- >  FieldGen a,      gs | idem
-- > ConstrGen a,      gs | idem
-- >      Gen1 a,      gs | idem
-- >     Gen1_ a,      gs | idem
class ArbitraryOr (fullGenList :: Type) (g :: Type) (gs :: Type)
        (sel :: (Maybe Symbol, Nat, Maybe Symbol)) a where
  arbitraryOr :: proxy sel -> fullGenList -> g -> gs -> Gen a

-- | All candidates have been exhausted
instance Arbitrary a => ArbitraryOr fg () () sel a where
  arbitraryOr _ _ _ _ = arbitrary
  {-# INLINE arbitraryOr #-}

-- | Examine the next candidate
instance ArbitraryOr fg b g sel a => ArbitraryOr fg () (b :+ g) sel a where
  arbitraryOr sel fg () (b :+ gens) = arbitraryOr sel fg b gens
  {-# INLINE arbitraryOr #-}

-- | Examine the last candidate (@g@ is not of the form @_ :+ _@)
instance {-# OVERLAPS #-} ArbitraryOr fg g () sel a => ArbitraryOr fg () g sel a where
  arbitraryOr sel fg () g = arbitraryOr sel fg g ()

-- | This can happen if the generators form a tree rather than a list, for whatever reason.
instance ArbitraryOr fg g (h :+ gs) sel a => ArbitraryOr fg (g :+ h) gs sel a where
  arbitraryOr sel fg (g :+ h) gs = arbitraryOr sel fg g (h :+ gs)

-- | None of the INCOHERENT instances match, discard the candidate @g@ and look
-- at the rest of the list @gs@.
instance {-# OVERLAPPABLE #-} ArbitraryOr fg () gs sel a => ArbitraryOr fg g gs sel a where
  arbitraryOr sel fg _ = arbitraryOr sel fg ()

-- | Matching custom generator for @a@.
instance {-# INCOHERENT #-} ArbitraryOr fg (Gen a) g sel a where
  arbitraryOr _ _ gen _ = gen
  {-# INLINE arbitraryOr #-}

#if __GLASGOW_HASKELL__ >= 800
-- | Matching custom generator for field @s@.
instance {-# INCOHERENT #-} (a ~ a') => ArbitraryOr fg (FieldGen s a) g '(con, i, 'Just s) a' where
  arbitraryOr _ _ (FieldGen gen) _ = gen
  {-# INLINE arbitraryOr #-}

-- | Matching custom generator for @i@-th field of constructor @c@.
instance {-# INCOHERENT #-} (a ~ a') => ArbitraryOr fg (ConstrGen c i a) g '( 'Just c, i, s) a' where
  arbitraryOr _ _ (ConstrGen gen) _ = gen
  {-# INLINE arbitraryOr #-}

-- | Get the name contained in a 'Meta' tag.
type family Name (d :: Meta) :: Maybe Symbol
type instance Name ('MetaSel mn su ss ds) = mn
type instance Name ('MetaCons n _f _s) = 'Just n
#else
type Name d = (Nothing :: Maybe Symbol)
#endif

-- | Matching custom generator for non-container @f@
instance {-# INCOHERENT #-} ArbitraryOr fg (Gen1_ f) g sel (f a) where
  arbitraryOr _ _ (Gen1_ gen) _ = gen

-- | Matching custom generator for container @f@. Start the search for containee @a@,
-- discarding field information.
instance {-# INCOHERENT #-} ArbitraryOr fg () fg '( 'Nothing, 0, 'Nothing) a
  => ArbitraryOr fg (Gen1 f) g sel (f a) where
  arbitraryOr _ fg (Gen1 gen) _ = gen (arbitraryOr noSel fg () fg)
    where noSel = Proxy :: Proxy '( 'Nothing, 0, 'Nothing)

newtype Weighted a = Weighted (Maybe (Int -> Gen a, Int))
  deriving Functor

instance Applicative Weighted where
  pure a = Weighted (Just ((pure . pure) a, 1))
  Weighted f <*> Weighted a = Weighted $ liftA2 g f a
    where
      g (f1, m) (a1, n) =
        ( \i ->
            let (j, k) = i `divMod` m
            in f1 j <*> a1 k
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

