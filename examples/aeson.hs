{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
import Data.Aeson
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific
import qualified Data.Vector as Vector
import Data.Text ( Text )
import qualified Data.Text as Text
import Test.QuickCheck
import Generic.Random.Data

instance Arbitrary Value where
  arbitrary = sized $ generatorPWith aliases

aliases :: [Alias Gen]
aliases =
  [ alias $ return . (HashMap.fromList :: [(Text, Value)] -> _)
  , alias $ return . (Vector.fromList :: [Value] -> _)
  , alias $ \ () -> fmap (realToFrac :: Double -> Scientific) arbitrary
  , alias $ return . Text.pack
  , alias $ (const arbitrary :: [()] -> Gen String)
  ]

main :: IO ()
main = do
  sample (arbitrary :: Gen Value)
  quickCheck $ \(v :: Value) -> (decode . encode) v === Just v
