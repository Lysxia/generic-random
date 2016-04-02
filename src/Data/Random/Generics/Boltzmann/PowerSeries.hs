module Data.Random.Generics.Boltzmann.PowerSeries where

import Data.List
import Data.Functor

-- | A power series with coefficients of type @a@.
newtype PowerSeries a = PowerSeries [a]

-- | We can save some work by skipping leading zeroes.
--
-- We don't represent leading zeroes with a single @Int@ because that's
-- not lazy enough for our purposes.
--
-- @negate@, @abs@, @signum@ are left unimplemented because we won't need them
-- (and @signum@ is impossible anyway).
instance (Eq a, Num a) => Num (PowerSeries a) where
  PowerSeries a + PowerSeries b = PowerSeries (a +. b)
    where
      (0 : a) +. (0 : b) = 0 : a +. b
      (a0 : a) +. (b0 : b) = a0 + b0 : a +. b
      [] +. b = b ; a +. [] = a
  PowerSeries a * PowerSeries b = PowerSeries (a *. b)
    where
      (0 : a) *. b = 0 : a *. b
      a *. (0 : b) = 0 : a *. b
      a *. b = drop 1 $
        zipWith convolute (inits (a ++ zero' b)) (inits (b ++ zero' a))
      convolute a b = sum (zipWith (*) (reverse a) b)
  fromInteger n = PowerSeries (fromInteger n : [])

  negate = error "negate is not defined for PowerSeries."
  abs = error "abs is not defined for PowerSeries."
  signum = error "signum is not defined for PowerSeries."

x :: Num a => Int -> PowerSeries a
x n = PowerSeries (replicate n 0 ++ [1])

zero' :: Num a => [a] -> [a]
zero' xs = xs $> 0

factor :: (Eq a, Num a) => PowerSeries a -> (Int, [a])
factor (PowerSeries as) = (length zs, as')
  where
    (zs, as') = span (== 0) as

takeCoef :: Int -> PowerSeries a -> [a]
takeCoef n (PowerSeries a) = take n a
