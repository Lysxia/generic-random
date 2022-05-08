Generic random generators [![Hackage](https://img.shields.io/hackage/v/generic-random.svg)](https://hackage.haskell.org/package/generic-random) [![Build Status](https://github.com/Lysxia/generic-random/actions/workflows/ci.yml/badge.svg)](https://github.com/Lysxia/generic-random/actions/workflows/ci.yml)
=========================

Generic random generators
to implement `Arbitrary` instances for [QuickCheck](https://hackage.haskell.org/package/QuickCheck)

Automating the `arbitrary` boilerplate also ensures that when a type changes to
have more or fewer constructors, then the generator either fixes itself to
generate that new case (when using the `uniform` distribution) or causes a
compilation error so you remember to fix it (when using an explicit
distribution).

This package also offers a simple (optional) strategy to ensure termination for
recursive types:
make `Test.QuickCheck.Gen`'s size parameter decrease at every recursive call;
when it reaches zero, sample directly from a trivially terminating generator
given explicitly (`genericArbitraryRec` and `withBaseCase`) or implicitly
(`genericArbitrary'`).

Example
-------

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Test.QuickCheck
import Generic.Random

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return Leaf

-- Equivalent to
-- > arbitrary =
-- >   sized $ \n ->
-- >     if n == 0 then
-- >       return Leaf
-- >     else
-- >       oneof
-- >         [ return Leaf
-- >         , resize (n `div` 3) $
-- >             Node <$> arbitrary <*> arbitrary <*> arbitrary
-- >         ]

main :: IO ()
main = sample (arbitrary :: Gen (Tree ()))
```

Related
-------

- The following two packages also derive random generators, but only with a uniform
  distribution of constructors:

    + [quickcheck-arbitrary-template](https://hackage.haskell.org/package/quickcheck-arbitrary-template) (TH)
    + [generic-arbitrary](https://hackage.haskell.org/package/generic-arbitrary-0.1.0) (GHC Generics)

- [testing-feat](http://hackage.haskell.org/package/testing-feat):
  derive enumerations for algebraic data types, which can be turned into random generators (TH).

- [boltzmann-samplers](https://hackage.haskell.org/package/boltzmann-samplers):
  derive Boltzmann samplers (SYB).
