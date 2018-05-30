-- An example of generic Arbitrary instances with explicit generators
-- for some types of fields for which there is no 'Arbitrary' instance
-- or the existing one is unsatisfactory.
--
-- For example, Postgres can't handle strings containing NUL characters
-- see https://github.com/lpsmith/postgresql-simple/issues/223
-- so applications may want to generate data without them.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char (isAlphaNum)
import Data.Text as T (Text, pack, unpack)
import GHC.Generics (Generic)
import Test.QuickCheck

import Generic.Random

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
  shrink = fmap pack . shrink . unpack

data R = R                    -- Two constraints:
  { name :: Text              -- names and
  , address :: Text           -- addresses don't contain '\NUL'
  , id_ :: Text               -- IDs are alphanumeric
  } deriving (Show, Generic)

instance Arbitrary R where
  arbitrary = genericArbitrarySingleG gens
    where
      gens =
        (FieldGen (pack . filter isAlphaNum <$> scale (* 5) arbitrary)
          :: FieldGen "id_" Text) :+
        (pack . filter (/= '\NUL') <$> arbitrary) :+
        ()

  shrink = genericShrink

newtype Bugged a = Bugged a deriving Show

instance Arbitrary (Bugged R) where
  arbitrary = Bugged <$> genericArbitrarySingle
  shrink (Bugged r) = Bugged <$> shrink r

main :: IO ()
main = do
  sample (arbitrary :: Gen R)
  let prop_nameNullFree r = all (/= '\NUL') (unpack (name r))
      prop_idAlpha r = all isAlphaNum (unpack (id_ r))
      qc prop = quickCheckWith stdArgs{maxSuccess = 1000} prop
  qc prop_nameNullFree
  qc prop_idAlpha
  qc $ expectFailure $ \(Bugged r) -> prop_nameNullFree r
  qc $ expectFailure $ \(Bugged r) -> prop_idAlpha r
