{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
module Generic.Random.Internal.Generic where

import Control.Applicative
import GHC.Generics
import Test.QuickCheck

-- * Random generators

-- | Pick a constructor with uniform probability, and fill its fields
-- recursively.
--
-- An equivalent definition for @Tree@ is:
--
-- > genericArbitrary :: Arbitrary a => Gen (Tree a)
-- > genericArbitrary =
-- >   oneof
-- >     [ Leaf <$> arbitrary
-- >     , Node <$> arbitrary <*> arbitrary
-- >     ]
--
-- Note that for many types, 'genericArbitrary' tends to produce big values.
-- For instance for @Tree a@ values are finite but the average number of
-- @Leaf@ and @Node@ constructors is infinite.

genericArbitrary :: (Generic a, GA Unsized (Rep a)) => Gen a
genericArbitrary = ($ repeat 1) . unFreq . fmap to $ ga @Unsized


-- | This allows to specify the probability distribution of constructors
-- as a list of weights, in the same order as the data type definition.
--
-- An equivalent definition for @Tree@ is:
--
-- > genericArbitraryFrequency :: Arbitrary a => [Int] -> Gen (Tree a)
-- > genericArbitraryFrequency [x, y] =
-- >   frequency
-- >     [ (x, Leaf <$> arbitrary)
-- >     , (y, Node <$> arbitrary <*> arbitrary)
-- >     ]

genericArbitraryFrequency :: (Generic a, GA Unsized (Rep a)) => [Int] -> Gen a
genericArbitraryFrequency = unFreq . fmap to $ ga @Unsized


-- | The size parameter is divided among the fields of the chosen constructor.
-- When it reaches zero, the generator selects nullary constructors whenever
-- possible.
--
-- This has little effect on the above @Tree@ type, as it has no nullary
-- constructors.
--
-- An equivalent definition for @Tree@ is:
--
-- > genericArbitraryFrequency' :: Arbitrary a => [Int] -> Gen (Tree a)
-- > genericArbitraryFrequency' [x, y] =
-- >   frequency
-- >     [ (x, Leaf <$> arbitrary)
-- >     , (y, scale (`div` 2) $ Node <$> arbitrary <*> arbitrary)
-- >     ]
-- >     -- 2 because Node is 2-ary.
--
-- Here is another more interesting example:
--
-- > data Tree' = Leaf1 | Leaf2 | Node3 Tree' Tree' Tree'
-- >   deriving Generic
-- >
-- > instance Arbitrary Tree' where
-- >   arbitrary = genericArbitraryFrequency' [1, 2, 3]
--
-- The definition of 'genericArbitraryFrequencySized' is equivalent to:
--
-- > genericArbitraryFrequency' :: [Int] -> Gen Tree'
-- > genericArbitraryFrequency' [x, y, z] =
-- >   sized $ \n ->
-- >     frequency $
-- >       [ (x, return Leaf1)
-- >       , (y, return Leaf2)
-- >       ] ++
-- >       [ (z, resize (n `div` 3) node) | n > 0 ]
-- >       -- 3 because Node3 is 3-ary
-- >   where
-- >     node = Node3 <$> arbitrary <*> arbitrary <*> arbitrary
--
-- If the size parameter is zero, the non-nullary alternative is discarded.

genericArbitraryFrequency' :: (Generic a, GA Sized (Rep a)) => [Int] -> Gen a
genericArbitraryFrequency' = unFreq . fmap to $ ga @Sized


-- | Like 'genericArbitraryFrequencySized', but with uniformly distributed
-- constructors.

genericArbitrary' :: (Generic a, GA Sized (Rep a)) => Gen a
genericArbitrary' = ($ repeat 1) . unFreq . fmap to $ ga @Sized


-- * Internal

newtype Freq sized a = Freq { unFreq :: [Int] -> Gen a }
  deriving Functor

instance Applicative (Freq sized) where
  pure = Freq . pure . pure
  Freq f <*> Freq x = Freq (liftA2 (<*>) f x)

newtype Gen' sized a = Gen' { unGen' :: Gen a }
  deriving (Functor, Applicative)

data Sized
data Unsized

liftGen :: Gen a -> Freq sized a
liftGen = Freq . const

-- | Generic Arbitrary
class GA sized f where
  ga :: Freq sized (f p)

instance GA sized U1 where
  ga = pure U1

instance Arbitrary c => GA sized (K1 i c) where
  ga = liftGen . fmap K1 $ arbitrary

instance GA sized f => GA sized (M1 i c f) where
  ga = fmap M1 ga

instance (GASum Sized f, GASum Sized g) => GA Sized (f :+: g) where
  ga = frequency' gaSum
    where
      frequency' :: [(Bool, Gen' sized a)] -> Freq sized a
      frequency' as = Freq $ \ws ->
        let
          was = zip ws as
          units = [(w, a) | (w, (True, Gen' a)) <- was]
        in
          sized $ \sz -> frequency $
            if sz == 0 && not (null units) then
              units
            else
              [(w, a) | (w, (_, Gen' a)) <- was]

instance (GASum Unsized f, GASum Unsized g) => GA Unsized (f :+: g) where
  ga = frequency' gaSum
    where
      frequency' :: [(Bool, Gen' sized a)] -> Freq sized a
      frequency' as = Freq $ \ws -> frequency
        [(w, a) | (w, (_, Gen' a)) <- zip ws as]

instance (GA Unsized f, GA Unsized g) => GA Unsized (f :*: g) where
  ga = liftA2 (:*:) ga ga

instance (GAProduct f, GAProduct g) => GA Sized (f :*: g) where
  ga = constScale' a
    where
      constScale' :: Gen' Unsized a -> Freq Sized a
      constScale' = Freq . const . scale (`div` n) . unGen'
      (n, a) = gaProduct


gArbitrarySingle :: forall sized f p . GA sized f => Gen' sized (f p)
gArbitrarySingle = Gen' (unFreq (ga :: Freq sized (f p)) [1])


class GASum sized f where
  gaSum :: [(Bool, Gen' sized (f p))]

instance (GASum sized f, GASum sized g) => GASum sized (f :+: g) where
  gaSum = (fmap . fmap . fmap) L1 gaSum ++ (fmap . fmap . fmap) R1 gaSum

instance forall sized i c f
  . (GA sized f, IsUnit f) => GASum sized (M1 i c f) where
  gaSum = [(isUnit (undefined :: f p), gArbitrarySingle)]


class IsUnit f where
  isUnit :: f p -> Bool

instance IsUnit U1 where
  isUnit _ = True

instance IsUnit f => IsUnit (M1 i c f) where
  isUnit (M1 x) = isUnit x

instance IsUnit (K1 i c) where
  isUnit _ = False

instance IsUnit (f :*: g) where
  isUnit _ = False


class GAProduct f where
  gaProduct :: (Int, Gen' Unsized (f p))

instance GA Unsized f => GAProduct (M1 i c f) where
  gaProduct = (1, gArbitrarySingle)

instance (GAProduct f, GAProduct g) => GAProduct (f :*: g) where
  gaProduct = (m + n, liftA2 (:*:) a b)
    where
      (m, a) = gaProduct
      (n, b) = gaProduct
