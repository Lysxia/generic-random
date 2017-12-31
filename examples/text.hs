-- An example of generic Arbitrary instances with explicit generators
-- for some types of fields for which there is no 'Arbitrary' instance
-- or the existing one is unsatisfactory.
--
-- For example, Postgres can't handle strings containing NUL characters
-- see https://github.com/lpsmith/postgresql-simple/issues/223
-- so applications may want to generate data without them.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Test.QuickCheck

import Generic.Random

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
  shrink = fmap pack . shrink . unpack

data R = R
  { name :: Text
  , address :: Text
  } deriving (Show, Generic)

instance Arbitrary R where
  arbitrary = genericArbitrarySingleG gens
    where
      gens = (pack . filter (/= '\NUL') <$> arbitrary) :@ Nil

  shrink = genericShrink

newtype Bugged a = Bugged a deriving Show

instance Arbitrary (Bugged R) where
  arbitrary = Bugged <$> genericArbitrarySingle
  shrink (Bugged r) = Bugged <$> shrink r

main :: IO ()
main = do
  sample (arbitrary :: Gen R)
  let prop_nameNullFree r = all (/= '\NUL') (unpack (name r))
      qc prop = quickCheckWith stdArgs{maxSuccess = 1000} prop
  qc prop_nameNullFree
  qc $ expectFailure $ \(Bugged r) -> prop_nameNullFree r
