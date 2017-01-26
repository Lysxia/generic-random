{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Random.Internal.Generic where

import Control.Applicative
import Data.Coerce
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics hiding ( S )
import GHC.TypeLits
import Test.QuickCheck

-- * Random generators

-- | Pick a constructor with a given distribution, and fill its fields
-- recursively.
--
-- The distribution of constructors can be specified using either 'uniform'
-- or 'weights', the latter expecting the weights in the same order as the
-- data type definition.
--
-- === Uniform distribution
--
-- For @Tree@, @'genericArbitrary' 'uniform'@ produces code equivalent to the
-- following:
--
-- > genericArbitrary uniform :: Arbitrary a => Gen (Tree a)
-- > genericArbitrary uniform =
-- >   oneof
-- >     [ Leaf <$> arbitrary                -- Uses Arbitrary a
-- >     , Node <$> arbitrary <*> arbitrary  -- Uses Arbitrary (Tree a)
-- >     ]
--
-- Note that for many types, a uniform distribution tends to produce big
-- values. For instance for @Tree a@, generated values are finite but the
-- __average__ number of @Leaf@ and @Node@ constructors is __infinite__.
--
-- === Custom distribution
--
-- For @Tree@, 'genericArbitrary' produces code equivalent to the following:
--
-- > genericArbitrary :: Arbitrary a => Weights (Tree a) -> Gen (Tree a)
-- > genericArbitrary (weighted (x % y % ())) =
-- >   frequency
-- >     [ (x, Leaf <$> arbitrary)
-- >     , (y, Node <$> arbitrary <*> arbitrary)
-- >     ]

genericArbitrary
  :: forall a
  .  (Generic a, GA Unsized (Rep a))
  => Weights a
  -> Gen a
genericArbitrary (Weights w n) = (unGen' . fmap to) (ga w n :: Gen' Unsized (Rep a p))

-- | The size parameter of 'Gen' is divided among the fields of the chosen
-- constructor.  When it reaches zero, the generator selects a finite term
-- whenever it can find any of the given type.
--
-- The natural number @n@ determines the maximum /depth/ of terms that can be
-- used to end recursion.
-- It is encoded using @'Z' :: 'Z'@ and @'S' :: n -> 'S' n@.
--
-- > genericArbitrary' n (weights (...))
--
-- With @n = 'Z'@, the generator looks for a simple nullary constructor.  If none
-- exist at the current type, as is the case for our @Tree@ type, it carries on
-- as in 'genericArbitrary'.
--
-- > genericArbitrary' Z :: Arbitrary a => Weights (Tree a) -> Gen (Tree a)
-- > genericArbitrary' Z (weights (x % y % ())) =
-- >   frequency
-- >     [ (x, Leaf <$> arbitrary)
-- >     , (y, scale (`div` 2) $ Node <$> arbitrary <*> arbitrary)
-- >     ]
-- >     -- 2 because Node is 2-ary.
--
-- Here is another example:
--
-- > data Tree' = Leaf1 | Leaf2 | Node3 Tree' Tree' Tree'
-- >   deriving Generic
-- >
-- > instance Arbitrary Tree' where
-- >   arbitrary = genericArbitrary' Z (weights (1 % 2 % 3 % ()))
--
-- 'genericArbitrary'' is equivalent to:
--
-- > genericArbitrary' Z :: Weights Tree' -> Gen Tree'
-- > genericArbitrary' Z (weights (x % y % z % ())) =
-- >   sized $ \n ->
-- >     if n == 0 then
-- >       -- If the size parameter is zero, the non-nullary alternative is discarded.
-- >       frequency $
-- >         [ (x, return Leaf1)
-- >         , (y, return Leaf2)
-- >         ]
-- >     else
-- >       frequency $
-- >         [ (x, return Leaf1)
-- >         , (y, return Leaf2)
-- >         , (z, resize (n `div` 3) node)
-- >         ]
-- >         -- 3 because Node3 is 3-ary
-- >   where
-- >     node = Node3 <$> arbitrary <*> arbitrary <*> arbitrary
--
-- To increase the chances of termination when no nullary constructor is directly
-- available, such as in @Tree@, we can pass a larger depth @n@. The effectiveness
-- of this parameter depends on the concrete type the generator is used for.
--
-- For instance, if we want to generate a value of type @Tree ()@, there is a
-- value of depth 1 (represented by @'S' 'Z'@) that we can use to end
-- recursion: @Leaf ()@.
--
-- > genericArbitrary' (S Z) :: Weights (Tree ()) -> Gen (Tree ())
-- > genericArbitrary' (S Z) (weights (x % y % ())) =
-- >   sized $ \n ->
-- >     if n == 0 then
-- >       return (Leaf ())
-- >     else
-- >       frequency
-- >         [ (x, Leaf <$> arbitrary)
-- >         , (y, scale (`div` 2) $ Node <$> arbitrary <*> arbitrary)
-- >         ]
--
-- Because the argument of @Tree@ must be inspected in order to discover
-- values of type @Tree ()@, we incur some extra constraints if we want
-- polymorphism.
--
-- @FlexibleContexts@ and @UndecidableInstances@ are also required.
--
-- > instance (Arbitrary a, Generic a, ListBaseCases Z (Rep a))
-- >   => Arbitrary (Tree a) where
-- >   arbitrary = genericArbitrary' (S Z) (weights (1 % 2 % ()))
--
-- A synonym is provided for brevity.
--
-- > instance (Arbitrary a, BaseCases' Z a) => Arbitrary (Tree a) where
-- >   arbitrary = genericArbitrary' (S Z) (weights (1 % 2 % ()))

genericArbitrary'
  :: forall n a
  . (Generic a, GA (Sized n) (Rep a))
  => n
  -> Weights a  -- ^ List of weights for every constructor
  -> Gen a
genericArbitrary' _ (Weights w n) =
  (unGen' . fmap to) (ga w n :: Gen' (Sized n) (Rep a p))


-- * Internal

type family Weights_ (f :: * -> *) :: * where
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

newtype Gen' sized a = Gen' { unGen' :: Gen a }
  deriving (Functor, Applicative, Monad)

data Sized n
data Unsized

sized' :: (Int -> Gen' sized a) -> Gen' sized a
sized' g = Gen' . sized $ \sz -> unGen' (g sz)

-- | Generic Arbitrary
class GA sized f where
  ga :: Weights_ f -> Int -> Gen' sized (f p)

instance GA sized f => GA sized (M1 D c f) where
  ga w n = fmap M1 (ga w n)

instance GAProduct f => GA Unsized (M1 C c f) where
  ga _ _ = (Gen' . fmap M1) gaProduct

instance (GAProduct f, KnownNat (Arity f)) => GA (Sized n) (M1 C c f) where
  ga _ _ = Gen' (scale (`div` arity) gaProduct)
    where
      arity = fromInteger (natVal' (proxy# :: Proxy# (Arity f)))

instance (GASum (Sized n) f, GASum (Sized n) g, BaseCases n f, BaseCases n g)
  => GA (Sized n) (f :+: g) where
  ga w n = sized' $ \sz ->
    case unTagged (baseCases w n :: Tagged n (Weighted ((f :+: g) p))) of
      Weighted (Just (bc, n)) | sz == 0 -> Gen' (choose (0, n - 1) >>= bc)
      _ -> gaSum' w n

instance (GASum Unsized f, GASum Unsized g) => GA Unsized (f :+: g) where
  ga = gaSum'

gArbitrarySingle
  :: forall sized f p c0
  .  (GA sized f, Weights_ f ~ L c0)
  => Gen' sized (f p)
gArbitrarySingle = ga L 0

gaSum' :: GASum sized f => Weights_ f -> Int -> Gen' sized (f p)
gaSum' w n = do
  i <- Gen' $ choose (0, n-1)
  gaSum i w

class GASum sized f where
  gaSum :: Int -> Weights_ f -> Gen' sized (f p)

instance (GASum sized f, GASum sized g) => GASum sized (f :+: g) where
  gaSum i (N a n b)
    | i < n = fmap L1 (gaSum i a)
    | otherwise = fmap R1 (gaSum (i - n) b)

instance GAProduct f => GASum sized (M1 i c f) where
  gaSum _ _ = Gen' gaProduct


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


newtype Tagged a b = Tagged { unTagged :: b }
  deriving Functor

-- $nat
-- Use the 'Z' and 'S' data types to define the depths of values used
-- by 'genericArbitrary'' to make generators terminate.

-- | Zero
data Z = Z

-- | Successor
data S n = S n

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

class BaseCases n f where
  baseCases :: Weights_ f -> Int -> Tagged n (Weighted (f p))

instance (BaseCases n f, BaseCases n g) => BaseCases n (f :+: g) where
  baseCases (N a m b) n =
    concat
      ((fmap . fmap) L1 (baseCases a m))
      ((fmap . fmap) R1 (baseCases b (n - m)))
    where
      concat :: Alternative u => Tagged n (u a) -> Tagged n (u a) -> Tagged n (u a)
      concat (Tagged a) (Tagged b) = Tagged (a <|> b)

instance ListBaseCases n f => BaseCases n (M1 i c f) where
  baseCases _ n = fmap reweigh listBaseCases
    where
      reweigh :: Weighted a -> Weighted a
      reweigh (Weighted h) = Weighted (fmap (\(g, _) -> (g, n)) h)

-- | A @ListBaseCases n ('Rep' a)@ constraint basically provides the list of
-- values of type @a@ with depth at most @n@.
class ListBaseCases n f where
  listBaseCases :: Alternative u => Tagged n (u (f p))

-- | For convenience.
type BaseCases' n a = (Generic a, ListBaseCases n (Rep a))

instance ListBaseCases n U1 where
  listBaseCases = Tagged (pure U1)

instance ListBaseCases n f => ListBaseCases n (M1 i c f) where
  listBaseCases = (fmap . fmap) M1 listBaseCases

instance ListBaseCases Z (K1 i c) where
  listBaseCases = Tagged empty

instance (Generic c, ListBaseCases n (Rep c)) => ListBaseCases (S n) (K1 i c) where
  listBaseCases = (retag . (fmap . fmap) (K1 . to)) listBaseCases
    where
      retag :: Tagged n a -> Tagged (S n) a
      retag = coerce

instance (ListBaseCases n f, ListBaseCases n g) => ListBaseCases n (f :+: g) where
  listBaseCases =
    concat
      ((fmap . fmap) L1 listBaseCases)
      ((fmap . fmap) R1 listBaseCases)
    where
      concat :: Alternative u => Tagged n (u a) -> Tagged n (u a) -> Tagged n (u a)
      concat (Tagged a) (Tagged b) = Tagged (a <|> b)

instance (ListBaseCases n f, ListBaseCases n g) => ListBaseCases n (f :*: g) where
  listBaseCases = liftedP listBaseCases listBaseCases
    where
      liftedP
        :: Applicative u
        => Tagged n (u (f p))
        -> Tagged n (u (g p))
        -> Tagged n (u ((f :*: g) p))
      liftedP (Tagged f) (Tagged g) = Tagged (liftA2 (:*:) f g)
