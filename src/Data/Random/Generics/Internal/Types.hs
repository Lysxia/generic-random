{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
module Data.Random.Generics.Internal.Types where

import Data.Data
import GHC.Stack ( CallStack, showCallStack )

data SomeData m where
  SomeData :: Data a => m a -> SomeData m

type SomeData' = SomeData Proxy

-- | Dummy instance for debugging.
instance Show (SomeData m) where
  show _ = "SomeData"

data Alias m where
  Alias :: (Data a, Data b) => !(m a -> m b) -> Alias m

-- | Dummy instance for debugging.
instance Show (Alias m) where
  show _ = "Alias"

-- | > composeCast f g = f . g
composeCastM :: forall a b c d m
  . (?loc :: CallStack, Typeable b, Typeable c)
  => (m c -> d) -> (a -> m b) -> (a -> d)
composeCastM f g | Just Refl <- eqT :: Maybe (b :~: c) = f . g
composeCastM _ _ = castError ([] :: [b]) ([] :: [c])

castM :: forall a b m
  . (?loc :: CallStack, Typeable a, Typeable b)
  => m a -> m b
castM a | Just Refl <- eqT :: Maybe (a :~: b) = a
castM a = let x = castError a x in x

unSomeData :: (?loc :: CallStack, Typeable a) => SomeData m -> m a
unSomeData (SomeData a) = castM a

applyCast :: (Typeable a, Data b) => (m a -> m b) -> SomeData m -> SomeData m
applyCast f = SomeData . f . unSomeData

castError :: (?loc :: CallStack, Typeable a, Typeable b)
  => proxy a -> proxy' b -> c
castError a b = error $ unlines
  [ "Error trying to cast"
  , "  " ++ show (typeRep a)
  , "to"
  , "  " ++ show (typeRep b)
  , showCallStack ?loc
  ]

withProxy :: (?loc :: CallStack) => (a -> b) -> proxy a -> b
withProxy f _ =
  f (error $ "This should not be evaluated\n" ++ showCallStack ?loc)

reproxy :: proxy a -> Proxy a
reproxy _ = Proxy

proxyType :: m a -> proxy a -> m a
proxyType = const

someData' :: Data a => proxy a -> SomeData'
someData' = SomeData . reproxy
