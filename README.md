Generic random generators
=========================

Define sized random generators for almost any type.

    {-# LANGUAGE DeriveDataTypeable #-}
    import Data.Data
    import Test.QuickCheck
    import Data.Random.Generics

    data Term = Lambda Int Term | App Term Term | Var Int
      deriving (Show, Data)

    instance Arbitrary Term where
      arbitrary = sized (generator asGen)

    main = sample (arbitrary :: Gen Term)

- Objects of the same size (number of constructors) occur with the same
  probability (see Duchon et al., references below).
- Implements rejection sampling and pointing.
- Works with QuickCheck and MonadRandom.
- Can be extended or modified with user defined generators.

References
----------

- The core theory of Boltzmann samplers is described in
  [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf),
  P. Duchon, P. Flajolet, G. Louchard, G. Schaeffer.

- The numerical evaluation of recursively defined generating functions
  is taken from
  [Boltzmann Oracle for Combinatorial Systems](http://www.dmtcs.org/pdfpapers/dmAI0132.pdf),
  C. Pivoteau, B. Salvy, M. Soria.
