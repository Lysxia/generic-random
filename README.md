Generic random generators [![Hackage](https://img.shields.io/hackage/v/generic-random.svg)](https://hackage.haskell.org/package/generic-random) [![Build Status](https://travis-ci.org/Lysxia/generic-random.svg)](https://travis-ci.org/Lysxia/generic-random)
=========================

Say goodbye to `Constructor <$> arbitrary <*> arbitrary <*> arbitrary`-boilerplate.

```haskell
    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics ( Generic )
    import Test.QuickCheck
    import Generic.Random.Generic

    data Tree a = Leaf | Node (Tree a) a (Tree a)
      deriving (Show, Generic)

    instance Arbitrary a => Arbitrary (Tree a) where
      arbitrary = genericArbitrary' Z uniform

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

- User-specified distribution of constructors, with a compile-time check that
  weights have been specified for all constructors.
- A simple (optional) strategy to ensure termination: `Test.QuickCheck.Gen`'s
  size parameter decreases at every recursive `genericArbitrary'` call; when it
  reaches zero, sample directly from a finite set of finite values.
