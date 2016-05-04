-- | Generic Boltzmann samplers.

module Data.Random.Generics (
  Size',
  -- * Main functions
  -- $sized
  generatorSR,
  generatorP,
  generatorPR,
  generatorR,
  -- ** Fixed size
  -- $fixed
  generatorP',
  generatorPR',
  generatorR',
  generator',
  -- * Generators with aliases
  -- $aliases
  generatorSRWith,
  generatorPWith,
  generatorPRWith,
  generatorRWith,
  -- ** Fixed size
  generatorPWith',
  generatorPRWith',
  generatorRWith',
  generatorWith',
  -- * Other generators
  -- $other
  Points,
  generatorM,
  generatorMR,
  generator_,
  generatorR_,
  -- * Auxiliary definitions
  -- ** Type classes
  MonadRandomLike (..),
  AMonadRandom (..),
  -- ** Alias
  alias,
  aliasR,
  coerceAlias,
  coerceAliases,
  Alias (..),
  AliasR,
  ) where

import Data.Data
import Data.Random.Generics.Internal
import Data.Random.Generics.Internal.Types

-- * Main functions

-- $sized
-- When these functions and their @_With@ counterparts below are specialized,
-- the numerical /oracles/ are computed once and for all, so they can be reused
-- for different sizes.
--
-- === Suffixes
--
-- [@S@] Singular sampler.
--
--     This works with recursive tree-like structures, as opposed to (lists of)
--     structures with bounded size. More precisely, the generating function of
--     the given type should have a finite radius of convergence, with a
--     singularity of a certain kind (see Duchon et al., reference in the
--     README), so that the oracle can be evaluated at that point.
--
--     This has the advantage of using the same oracle for all size parameters,
--     which simply specify a target size interval.
--
-- [@P@] Generator of pointed values.
--
--     It usually has a flatter distribution of sizes than a simple Boltzmann
--     sampler, making it an efficient alternative to rejection sampling.
--
--     It also works on more types, particularly lists and finite types,
--     but relies on multiple oracles.
--
-- [@R@] Rejection sampling.
--
--     These generators filter out values whose sizes are not within some
--     interval. In the first two sections, that interval is implicit:
--     @[(1-'epsilon')*size, (1+'epsilon')*size]@, for @'epsilon' = 0.1@.
--
--     The generator restarts as soon as it has produced more constructors than
--     the upper bound, this strategy is called /ceiled rejection sampling/.
--
-- = Pointing
--
-- The /pointing/ of a type @t@ is a derived type whose values are essentially
-- values of type @t@, with one of their constructors being "pointed".
-- Alternatively, we may turn every constructor into variants that indicate
-- the position of points.
--
-- @
--   -- Original type
--   data Tree = Node Tree Tree | Leaf
--   -- Pointing of Tree
--   data Tree'
--     = Tree' Tree -- Point at the root
--     | Node'0 Tree' Tree -- Point to the left
--     | Node'1 Tree Tree' -- Point to the right
-- @
--
-- Pointed values are easily mapped back to the original type by erasing the
-- point. Pointing makes larger values occur much more frequently, while
-- preserving the uniformness of the distribution conditionally to a fixed
-- size.
--

-- | @
--   'generatorSR' :: Int -> 'Gen' a
--   'asMonadRandom' . 'generatorSR' :: 'MonadRandom' m => Int -> m a
-- @
--
-- Singular ceiled rejection sampler.
generatorSR :: (Data a, MonadRandomLike m) => Size' -> m a
generatorSR = generatorSRWith []

-- | @
--   'generatorP' :: Int -> 'Gen' a
--   'asMonadRandom' . 'generatorP' :: 'MonadRandom' m => Int -> m a
-- @
--
-- Generator of pointed values.

generatorP :: (Data a, MonadRandomLike m) => Size' -> m a
generatorP = generatorPWith []

-- | Pointed generator with rejection.
generatorPR :: (Data a, MonadRandomLike m) => Size' -> m a
generatorPR = generatorPRWith []

-- | Generator with rejection and dynamic average size.
generatorR :: (Data a, MonadRandomLike m) => Size' -> m a
generatorR = generatorRWith []

-- ** Fixed size

-- $fixed
-- The @'@ suffix indicates functions which do not do any
-- precomputation before passing the size parameter.
--
-- This means that oracles are computed from scratch for every size value,
-- which may incur a significant overhead.

-- | Pointed generator.
generatorP' :: (Data a, MonadRandomLike m) => Size' -> m a
generatorP' = generatorPWith' []

-- | Pointed generator with rejection.
generatorPR' :: (Data a, MonadRandomLike m) => Size' -> m a
generatorPR' = generatorPRWith' []

-- | Ceiled rejection sampler with given average size.
generatorR' :: (Data a, MonadRandomLike m) => Size' -> m a
generatorR' = generatorRWith' []

-- | Basic boltzmann sampler with no optimization.
generator' :: (Data a, MonadRandomLike m) => Size' -> m a
generator' = generatorWith' []

-- * Generators with aliases

-- $aliases
-- Boltzmann samplers can normally be defined only for types @a@ such that:
--
-- - they are instances of 'Data';
-- - the set of types of subterms of values of type @a@ is finite;
-- - and all of these types have at least one finite value (i.e., values with
--   finitely many constructors).
--
-- Examples of misbehaving types are:
--
-- - @a -> b -- Not Data@
-- - @data E a = L a | R (E [a]) -- Contains a, [a], [[a]], [[[a]]], etc.@
-- - @data I = C I -- No finite value@
--
-- = Alias
--
-- The 'Alias' type works around these limitations ('AliasR' for rejection
-- samplers).
-- This existential wrapper around a user-defined function @f :: a -> m b@
-- makes @generic-random@ view occurences of the type @b@ as @a@ when
-- processing a recursive system of types, possibly stopping some infinite
-- unrolling of type definitions. When a value of type @b@ needs to be
-- generated, it generates an @a@ which is passed to @f@.
--
-- @
--   let
--     as = ['aliasR' $ \\() -> return (L []) :: 'Gen' (E [[Int]])]
--   in
--     'generatorSRWith' as 'asGen' :: 'Size' -> 'Gen' (E Int)
-- @
--
-- Another use case is to plug in user-defined generators where the default is
-- not satisfactory, for example, to get positive @Int@s:
--
-- @
--   let
--     as = ['alias' $ \\() -> 'choose' (0, 100) :: 'Gen' Int)]
--   in
--     'generatorPWith' as 'asGen' :: 'Size' -> 'Gen' [Int]
-- @

generatorSRWith
  :: (Data a, MonadRandomLike m) => [AliasR m] -> Size' -> m a
generatorSRWith aliases =
  generatorR_ aliases 0 Nothing . tolerance epsilon

generatorPRWith
  :: (Data a, MonadRandomLike m) => [AliasR m] -> Size' -> m a
generatorPRWith aliases size' =
  generatorMR aliases 1 size' (tolerance epsilon size')

generatorPWith
  :: (Data a, MonadRandomLike m) => [Alias m] -> Size' -> m a
generatorPWith aliases = generatorM aliases 1

generatorRWith
  :: (Data a, MonadRandomLike m) => [AliasR m] -> Size' -> m a
generatorRWith aliases size' =
  generatorMR aliases 0 size' (tolerance epsilon size')

-- ** Fixed size

generatorPWith'
  :: (Data a, MonadRandomLike m) => [Alias m] -> Size' -> m a
generatorPWith' aliases = generator_ aliases 1 . Just

generatorPRWith'
  :: (Data a, MonadRandomLike m) => [AliasR m] -> Size' -> m a
generatorPRWith' aliases size' =
  generatorR_ aliases 1 (Just size') (tolerance epsilon size')

generatorRWith'
  :: (Data a, MonadRandomLike m) => [AliasR m] -> Size' -> m a
generatorRWith' aliases size' =
  generatorR_ aliases 0 (Just size') (tolerance epsilon size')

generatorWith'
  :: (Data a, MonadRandomLike m) => [Alias m] -> Size' -> m a
generatorWith' aliases = generator_ aliases 0 . Just

-- * Other generators

-- $other Used in the implementation of the generators above.
-- These also allow to apply pointing more than once.
--
-- === Suffixes
--
-- [@M@] Sized generators are memoized for some sparsely chosen values of
-- sizes. Subsequently supplied sizes are approximated by the closest larger
-- value. This strategy avoids recomputing too many oracles. Aside from
-- singular samplers, all other generators above not marked by @'@ use this.
--
-- [@_@] If the size parameter is @Nothing@, produces the singular generator
-- (associated with the suffix @S@); otherwise the generator produces values
-- with average size equal to the given value.

generatorM
  :: (Data a, MonadRandomLike m)
  => [Alias m] -> Points -> Size' -> m a
generatorM = memo make apply

generatorMR
  :: (Data a, MonadRandomLike m)
  => [AliasR m] -> Points -> Size' -> (Size', Size') -> m a
generatorMR = memo makeR applyR

-- | Boltzmann sampler without rejection.
generator_
  :: (Data a, MonadRandomLike m)
  => [Alias m] -> Points -> Maybe Size' -> m a
generator_ aliases = apply (make aliases [])

-- | Boltzmann sampler with rejection.
generatorR_
  :: (Data a, MonadRandomLike m)
  => [AliasR m] -> Points -> Maybe Size' -> (Size', Size') -> m a
generatorR_ aliases = applyR (makeR aliases [])
