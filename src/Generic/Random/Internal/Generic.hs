{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Control.Applicative (Alternative(..), liftA2)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

import Data.Proxy (Proxy(..))
import Data.Type.Bool (type (&&))
import Data.Type.Equality (type (==))

import GHC.Generics hiding (S, prec)
import GHC.TypeLits (KnownNat, Nat, Symbol, type (+), natVal)
import Test.QuickCheck (Arbitrary(..), Gen, choose, scale, sized, vectorOf)

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
  Weights_ (M1 C ('MetaCons c _i _j) _f) = L c

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

class WeightBuilder' w where

  -- | A binary constructor for building up trees of weights.
  (%) :: (c ~ First' w) => W c -> Prec' w -> w

instance WeightBuilder (Weights_ (Rep a)) => WeightBuilder' (Weights a) where
  w % prec = weights (w %. prec)

instance WeightBuilder a => WeightBuilder' (a, Int, r) where
  (%) = (%.)

class WeightBuilder a where
  type Prec a r

  (%.) :: (c ~ First a) => W c -> Prec a r -> (a, Int, r)

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
-- Instead, use the provided synonyms ('UnsizedOpts', 'SizedOpts', 'SizedOptsDef')
-- and the setter 'SetOptions' (abbreviated as @('<+')@).
newtype Options (c :: Coherence) (s :: Sizing) (genList :: Type) = Options
  { _generators :: genList
  }

-- | Setter for 'Options'.
--
-- This subsumes the other setters: 'SetSized', 'SetUnsized', 'SetGens'.
--
-- @since 1.4.0.0
type family SetOptions (x :: k) (o :: Type) :: Type
type instance SetOptions (s :: Sizing) (Options c _s g) = Options c s g
type instance SetOptions (c :: Coherence) (Options _c s g) = Options c s g
type instance SetOptions (g :: Type) (Options c s _g) = Options c s g

-- | Infix flipped synonym for 'Options'.
--
-- @since 1.4.0.0
type (<+) o x = SetOptions x o
infixl 1 <+


type UnsizedOpts = Options 'INCOHERENT 'Unsized ()
type SizedOpts = Options 'INCOHERENT 'Sized ()
type SizedOptsDef = Options 'INCOHERENT 'Sized (Gen1 [] :+ ())

-- | Like 'UnsizedOpts', but using coherent instances by default.
--
-- @since 1.4.0.0
type CohUnsizedOpts = Options 'COHERENT 'Unsized ()

-- | Like 'SizedOpts', but using coherent instances by default.
--
-- @since 1.4.0.0
type CohSizedOpts = Options 'COHERENT 'Sized ()

-- | Coerce an 'Options' value between types with the same representation.
--
-- @since 1.4.0.0
setOpts :: forall x o. (Coercible o (SetOptions x o)) => o -> SetOptions x o
setOpts = coerce

-- | Default options for unsized generators.
unsizedOpts :: UnsizedOpts
unsizedOpts = Options ()

-- | Default options for sized generators.
sizedOpts :: SizedOpts
sizedOpts = Options ()

-- | Default options overriding the list generator using 'listOf''.
sizedOptsDef :: SizedOptsDef
sizedOptsDef = Options (Gen1 listOf' :+ ())

-- | Like 'unsizedOpts', but using coherent instances by default.
cohUnsizedOpts :: CohUnsizedOpts
cohUnsizedOpts = Options ()

-- | Like 'sizedOpts' but using coherent instances by default.
cohSizedOpts :: CohSizedOpts
cohSizedOpts = Options ()


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

type family SizingOf opts :: Sizing
type instance SizingOf (Options _c s _g) = s

type family SetSized (o :: Type) :: Type
type instance SetSized (Options c s g) = Options c 'Sized g

type family SetUnsized (o :: Type) :: Type
type instance SetUnsized (Options c s g) = Options c 'Unsized g

setSized :: Options c s g -> Options c 'Sized g
setSized = coerce

setUnsized :: Options c s g -> Options c 'Unsized g
setUnsized = coerce


-- | For custom generators to work with parameterized types, incoherent
-- instances must be used internally.
-- In practice, the resulting behavior is what users want 100% of the time,
-- so you should forget this option even exists.
--
-- === __Details__
--
-- tl;dr:
--
-- - The default setting is 'INCOHERENT'.
-- - You can use 'COHERENT' (via 'CohUnsizedOpts' or 'CohSizedOpts') to make
--   sure that no incoherent instances ever occur in your instance search.
-- - In 'COHERENT' mode, you can still selectively enable 'Incoherent' for
--   individual generators. Doing this carefully might ensure a unique solution
--   for instance resolution, even if incoherent.
--
-- The default configuration of generic-random does a decent job if
-- we trust GHC implements precisely the instance resolution algorithm as
-- described in the GHC manual:
--
-- - https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances
--
-- While that assumption holds in practice, it is overly context-dependent
-- (to know the context leading to a particular choice, we must replay the
-- whole resolution algorithm).
-- In particular, this algorithm may find one solution, but it is not
-- guaranteed to be unique: the behavior of the program is dependent on
-- implementation details.
--
-- A notable property to consider of an implicit type system (such as type
-- classes) is coherence: the behavior of the program is stable under
-- specialization.
--
-- This sounds nice on paper, but actually leads to surprising behavior for
-- generic implementations with parameterized types, such as generic-random.
--
-- To address that, the coherence property can be relaxd by users, by
-- explicitly allowing some custom generators to be chosen incoherently. With
-- appropriate precautions, it is possible to ensure a weaker property which
-- nevertheless helps keep type inference predictable: when a solution is
-- found, it is unique.
-- (This is assuredly weaker, i.e., is not stable under specialization.)
--
-- @since 1.4.0.0
data Coherence
  = INCOHERENT  -- ^ Match custom generators incoherently.
  | COHERENT
    -- ^ Match custom generators coherently by default
    -- (can be locally bypassed with 'Incoherent').

type family CoherenceOf (o :: Type) :: Coherence
type instance CoherenceOf (Options c _s _g) = c

-- | Match this generator incoherently when the 'COHERENT' option is set.
newtype Incoherent g = Incoherent g


-- | Heterogeneous list of generators.
data a :+ b = a :+ b

infixr 1 :+


type family GeneratorsOf opts :: Type
type instance GeneratorsOf (Options _c _s g) = g

class HasGenerators opts where
  generators :: opts -> GeneratorsOf opts

instance HasGenerators (Options c s g) where
  generators = _generators

-- | Define the set of custom generators.
--
-- Note: for recursive types which can recursively appear inside lists or other
-- containers, you may want to include a custom generator to decrease the size
-- when generating such containers.
--
-- See also the Note about lists in "Generic.Random.Tutorial#notelists".
setGenerators :: genList -> Options c s g0 -> Options c s genList
setGenerators gens (Options _) = Options gens

type family SetGens (g :: Type) opts
type instance SetGens g (Options c s _g) = Options c s g


-- | Custom generator for record fields named @s@.
--
-- If there is a field named @s@ with a different type,
-- this will result in a type error.
newtype FieldGen (s :: Symbol) a = FieldGen { unFieldGen :: Gen a }

-- | 'FieldGen' constructor with the field name given via a proxy.
fieldGen :: proxy s -> Gen a -> FieldGen s a
fieldGen _ = FieldGen

-- | Custom generator for the @i@-th field of the constructor named @c@.
-- Fields are 0-indexed.
newtype ConstrGen (c :: Symbol) (i :: Nat) a = ConstrGen { unConstrGen :: Gen a }

-- | 'ConstrGen' constructor with the constructor name given via a proxy.
constrGen :: proxy '(c, i) -> Gen a -> ConstrGen c i a
constrGen _ = ConstrGen

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
  , FindGen 'Shift ('S gs coh '(c, i, Name d)) () gs a
  , gs ~ GeneratorsOf opts
  , coh ~ CoherenceOf opts )
  => GAProduct' c i opts (S1 d (K1 _k a)) where
  gaProduct' _ opts = fmap (M1 . K1) (findGen (is, s, gs) () gs)
    where
      is = Proxy :: Proxy 'Shift
      s = Proxy :: Proxy ('S gs coh '(c, i, Name d))
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

-- | Given a list of custom generators @g :+ gs@, find one that applies,
-- or use @Arbitrary a@ by default.
--
-- @g@ and @gs@ follow this little state machine:
--
-- >           g,      gs | result
-- > ---------------------+-----------------------------
-- >          (),      () | END
-- >          (), g :+ gs | g, gs
-- >          (),      g  | g, () when g is not (_ :+ _)
-- >      g :+ h,      gs | g, h :+ gs
-- >       Gen a,      gs | END if g matches, else ((), gs)
-- >  FieldGen a,      gs | idem
-- > ConstrGen a,      gs | idem
-- >      Gen1 a,      gs | idem
-- >     Gen1_ a,      gs | idem
class FindGen (i :: AInstr) (s :: AStore) (g :: Type) (gs :: Type) (a :: Type) where
  findGen :: (Proxy i, Proxy s, FullGenListOf s) -> g -> gs -> Gen a

data AInstr = Shift | Match Coherence | MatchCoh Bool
data AStore = S Type Coherence ASel

type ASel = (Maybe Symbol, Nat, Maybe Symbol)

iShift :: Proxy 'Shift
iShift = Proxy

type family FullGenListOf (s :: AStore) :: Type where
  FullGenListOf ('S fg _coh _sel) = fg

type family ACoherenceOf (s :: AStore) :: Coherence where
  ACoherenceOf ('S _fg coh _sel) = coh

type family ASelOf (s :: AStore) :: ASel where
  ASelOf ('S _fg _coh sel) = sel

-- | All candidates have been exhausted
instance Arbitrary a => FindGen 'Shift s () () a where
  findGen _ _ _ = arbitrary
  {-# INLINEABLE findGen #-}

-- | Examine the next candidate
instance FindGen 'Shift s b g a => FindGen 'Shift s () (b :+ g) a where
  findGen p () (b :+ gens) = findGen p b gens
  {-# INLINEABLE findGen #-}

-- | Examine the last candidate (@g@ is not of the form @_ :+ _@)
instance {-# OVERLAPS #-} FindGen 'Shift s g () a => FindGen 'Shift s () g a where
  findGen p () g = findGen p g ()

-- | This can happen if the generators form a tree rather than a list, for whatever reason.
instance FindGen 'Shift s g (h :+ gs) a => FindGen 'Shift s (g :+ h) gs a where
  findGen p (g :+ h) gs = findGen p g (h :+ gs)

instance FindGen ('Match 'INCOHERENT) s g gs a => FindGen 'Shift s (Incoherent g) gs a where
  findGen (_, s, fg) (Incoherent g) = findGen (im, s, fg) g where
    im = Proxy :: Proxy ('Match 'INCOHERENT)

-- | If none of the above matches, then @g@ should be a simple generator,
-- and we test whether it matches the type @a@.
instance {-# OVERLAPPABLE #-} FindGen ('Match (ACoherenceOf s)) s g gs a
  => FindGen 'Shift s g gs a where
  findGen (_, s, fg) = findGen (im, s, fg) where
    im = Proxy :: Proxy ('Match (ACoherenceOf s))

-- INCOHERENT

-- | None of the INCOHERENT instances match, discard the candidate @g@ and look
-- at the rest of the list @gs@.
instance FindGen 'Shift s () gs a
  => FindGen ('Match 'INCOHERENT) s _g gs a where
  findGen (_, s, fg) _ = findGen (iShift, s, fg) () where

-- | Matching custom generator for @a@.
instance {-# INCOHERENT #-} FindGen ('Match 'INCOHERENT) s (Gen a) gs a where
  findGen _ gen _ = gen
  {-# INLINEABLE findGen #-}

-- | Matching custom generator for non-container @f@.
instance {-# INCOHERENT #-} FindGen ('Match 'INCOHERENT) s (Gen1_ f) gs (f a) where
  findGen _ (Gen1_ gen) _ = gen

-- | Matching custom generator for container @f@. Start the search for containee @a@,
-- discarding field information.
instance {-# INCOHERENT #-} FindGen 'Shift ('S fg coh DummySel) () fg a
  => FindGen ('Match 'INCOHERENT) ('S fg coh _sel) (Gen1 f) gs (f a) where
  findGen (_, _, fg) (Gen1 gen) _ = gen (findGen (iShift, s, fg) () fg) where
    s  = Proxy :: Proxy ('S fg coh DummySel)

type DummySel = '( 'Nothing, 0, 'Nothing)

-- | Matching custom generator for field @s@.
instance {-# INCOHERENT #-} (a ~ a')
  => FindGen ('Match 'INCOHERENT) ('S _fg _coh '(con, i, 'Just s)) (FieldGen s a) gs a' where
  findGen _ (FieldGen gen) _ = gen
  {-# INLINEABLE findGen #-}

-- | Matching custom generator for @i@-th field of constructor @c@.
instance {-# INCOHERENT #-} (a ~ a')
  => FindGen ('Match 'INCOHERENT) ('S _fg _coh '( 'Just c, i, s)) (ConstrGen c i a) gs a' where
  findGen _ (ConstrGen gen) _ = gen
  {-# INLINEABLE findGen #-}

-- | Get the name contained in a 'Meta' tag.
type family Name (d :: Meta) :: Maybe Symbol
type instance Name ('MetaSel mn su ss ds) = mn
type instance Name ('MetaCons n _f _s) = 'Just n

-- COHERENT

-- Use a type famaily to do the matching coherently.
instance FindGen ('MatchCoh (Matches (ASelOf s) g a)) s g gs a
  => FindGen ('Match 'COHERENT) s g gs a where
  findGen (_, s, fg) = findGen (im, s, fg) where
    im = Proxy :: Proxy ('MatchCoh (Matches (ASelOf s) g a))

type family Matches (s :: ASel) (g :: Type) (a :: Type) :: Bool where
  Matches _sel (Gen b) a = b == a
  Matches _sel (Gen1_ f) (f a) = 'True
  Matches _sel (Gen1_ f) a = 'False
  Matches _sel (Gen1 f) (f a) = 'True
  Matches _sel (Gen1 f) a = 'False
  Matches '(_c, i,  s) (FieldGen s1 b) a = s == 'Just s1 && b == a
  Matches '( c, i, _s) (ConstrGen c1 j b) a = c == 'Just c1 && i == j && b == a

-- If there is no match, skip and shift.
instance FindGen 'Shift s () gs a => FindGen ('MatchCoh 'False) s _g gs a where
  findGen (_, s, fg) _ = findGen (iShift, s, fg) () where

-- If there is a match, the search terminates

instance (a ~ a') => FindGen ('MatchCoh 'True) s (Gen a) gs a' where
  findGen _ g _ = g

instance (f x ~ a') => FindGen ('MatchCoh 'True) s (Gen1_ f) gs a' where
  findGen _ (Gen1_ g) _ = g

instance (f x ~ a', FindGen 'Shift ('S fg coh DummySel) () fg x)
  => FindGen ('MatchCoh 'True) ('S fg coh _sel) (Gen1 f) gs a' where
  findGen (_, _, fg) (Gen1 gen) _ = gen (findGen (iShift, s, fg) () fg) where
    s  = Proxy :: Proxy ('S fg coh DummySel)

-- | Matching custom generator for field @s@.
instance (a ~ a')
  => FindGen ('MatchCoh 'True) s (FieldGen sn a) gs a' where
  findGen _ (FieldGen gen) _ = gen

-- | Matching custom generator for @i@-th field of constructor @c@.
instance (a ~ a')
  => FindGen ('MatchCoh 'True) s (ConstrGen c i a) gs a' where
  findGen _ (ConstrGen gen) _ = gen

--

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

