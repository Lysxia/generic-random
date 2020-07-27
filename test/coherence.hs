{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# LANGUAGE
    BangPatterns,
    DataKinds,
    DeriveGeneric,
    ScopedTypeVariables,
    TypeOperators,
    RebindableSyntax,
    TypeApplications #-}

import Control.Monad (replicateM)
import Control.Exception
import System.Exit (exitFailure)
import Data.Foldable (find, traverse_)
import Data.Maybe (catMaybes)

import GHC.Generics ( Generic )
import Test.QuickCheck (Arbitrary (..), Gen, sample, generate)
import Prelude

import Generic.Random

-- @T0@, @T1@: Override the @Int@ generator in the presence of a type parameter @a@.

-- Counterexample that's not supposed to type check.
-- Use BangPatterns so we can force it with just seq.
data T0 a = N0 !a !Int
  deriving (Generic, Show)

instance Arbitrary a => Arbitrary (T0 a) where
  arbitrary = genericArbitraryWith
      (setGenerators customGens cohSizedOpts)
      uniform
    where
      customGens :: Gen Int
      customGens = pure 33


-- This one works.
data T1 a = N1 a Int
  deriving (Generic, Show)

instance Arbitrary a => Arbitrary (T1 a) where
  arbitrary = genericArbitraryWith
      (setGenerators customGens cohSizedOpts)
      uniform
    where
      customGens :: Incoherent (Gen a) :+ Gen Int
      customGens = Incoherent arbitrary :+ pure 33

check1 :: T1 a -> Bool
check1 (N1 _ n) = n == 33


-- A bigger example to cover the remaining generator types.
data T2 a = N2
  { f2a :: a
  , f2b :: Int
  , f2c :: [Int]
  , f2d :: Maybe Int
  , f2e :: Int
  , f2g :: Int
  , f2h :: [a]
  } deriving (Show, Generic)

instance Arbitrary a => Arbitrary (T2 a) where
  arbitrary = genericArbitraryWith
      (setGenerators customGens cohSizedOpts)
      uniform
    where
      -- Hack to allow annotating each generator in the list while avoiding parentheses
      (>>) = (:+)
      customGens = do
        Incoherent arbitrary :: Incoherent (Gen a)
        Incoherent (FieldGen ((: []) <$> arbitrary))
                             :: Incoherent (FieldGen "f2h" [a])
        Gen1_ (pure Nothing) :: Gen1_ Maybe
        Gen1 (fmap (\x -> [x, x])) :: Gen1 []
        ConstrGen (pure 88)  :: ConstrGen "N2" 4 Int
        FieldGen  (pure 77)  :: FieldGen "f2g" Int
        pure 33              :: Gen Int

check2 :: T2 a -> Bool
check2 t =
     f2b t == 33
  && length (f2c t) == 2
  && f2d t == Nothing
  && f2e t == 88
  && f2g t == 77
  && length (f2h t) == 1


type Error = String

expectTypeError :: IO a -> IO (Maybe Error)
expectTypeError gen = do
  r <- try (gen >>= evaluate)
  case r of
    Left (e :: TypeError) -> pure Nothing  -- success
    Right _ -> (pure . Just) "Unexpected evaluation (expected a type error)"


sample_ :: Show a => (a -> Bool) -> Gen a -> IO (Maybe Error)
sample_ check g = do
  xs <- generate (replicateM 100 g)
  case find (not . check) xs of
    Nothing -> pure Nothing
    Just x -> (pure . Just) ("Invalid value: " ++ show x)


collectErrors :: [IO (Maybe Error)] -> IO ()
collectErrors xs = do
  es <- sequence xs
  case catMaybes es of
    [] -> pure ()
    es@(_ : _) -> do
      putStrLn "Test failed. Errors:"
      traverse_ putStrLn es
      exitFailure

main :: IO ()
main = collectErrors
  [ expectTypeError (generate (arbitrary :: Gen (T0 ())))
  , sample_ check1 (arbitrary :: Gen (T1 ()))
  , sample_ check2 (arbitrary :: Gen (T2 ()))
  ]
