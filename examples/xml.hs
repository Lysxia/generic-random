import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import Test.QuickCheck
import Text.XML
import Generic.Random.Data

instance Arbitrary Document where
  arbitrary = sized $ generatorSRWith [textAlias] . (4 *)

textAlias :: AliasR Gen
textAlias = aliasR $ \() -> fmap Text.pack (listOf (choose ('a', 'z')))

main = sample' (arbitrary :: Gen Document) >>=
  mapM_ (Text.putStrLn . renderText def)
