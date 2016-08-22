Generic random generators [![Hackage](https://img.shields.io/hackage/v/generic-random.svg)](https://hackage.haskell.org/package/generic-random) [![Build Status](https://travis-ci.org/Lysxia/generic-random.svg)](https://travis-ci.org/Lysxia/generic-random)
=========================

`Generic.Random.Data`
---------------------

Define sized random generators for almost any type.

```haskell
    {-# LANGUAGE DeriveDataTypeable #-}

    import Data.Data
    import Test.QuickCheck
    import Generic.Random.Data

    data Term = Lambda Int Term | App Term Term | Var Int
      deriving (Show, Data)

    instance Arbitrary Term where
      arbitrary = sized $ generatorPWith [positiveInts]

    positiveInts :: Alias Gen
    positiveInts =
      alias $ \() -> fmap getPositive arbitrary :: Gen Int

    main = sample (arbitrary :: Gen Term)
```

- Objects of the same size (number of constructors) occur with the same
  probability (see Duchon et al., references below).
- Implements rejection sampling and pointing.
- Uses `Data.Data` generics.
- Works with QuickCheck and MonadRandom, but also similar user-defined monads
  for randomness (just implement `MonadRandomLike`).
- Can be tweaked somewhat with user defined generators.

`Generic.Random.Generic`
------------------------

Say goodbye to `Constructor <$> arbitrary <*> arbitrary <*> arbitrary`-boilerplate.

```haskell
    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics ( Generic )
    import Test.QuickCheck
    import Generic.Random.Generic

    data Tree a = Leaf | Node (Tree a) a (Tree a)
      deriving (Show, Generic)

    instance Arbitrary a => Arbitrary (Tree a) where
      arbitrary = genericArbitrary' Z

    -- Equivalent to
    -- > arbitrary =
    -- >   sized $ \n ->
    -- >     if n == 0 then
    -- >       return Leaf
    -- >     else
    -- >       oneof
    -- >         [ return Leaf
    -- >         , Node <$> arbitrary <*> arbitrary <*> arbitrary
    -- >         ]

    main = sample (arbitrary :: Gen (Tree ()))
```

- User-specified distribution of constructors.
- A simple (optional) strategy to ensure termination: `Test.QuickCheck.Gen`'s
  size parameter decreases at every recursive `genericArbitrary'` call; when it
  reaches zero, sample directly from a finite set of finite values.
- Uses `GHC.Generics` generics.
- Just for QuickCheck's `arbitrary`.
- More flexible than `Generic.Random.Data`'s Boltzmann samplers, which compute
  fixed weights for a given target size and concrete type, but with a less
  regular distribution.

`Generic.Random.Boltzmann`
--------------------------

An experimental interface to obtain Boltzmann samplers from an applicative
specification of a combinatorial system.

No documentation (yet).

References
----------

Papers about Boltzmann samplers, used in `Generic.Random.Data`:

- The core theory of Boltzmann samplers is described in
  [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf),
  P. Duchon, P. Flajolet, G. Louchard, G. Schaeffer.

- The numerical evaluation of recursively defined generating functions
  is taken from
  [Boltzmann Oracle for Combinatorial Systems](http://www.dmtcs.org/pdfpapers/dmAI0132.pdf),
  C. Pivoteau, B. Salvy, M. Soria.
