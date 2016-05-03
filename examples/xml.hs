import Control.Monad
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import Test.QuickCheck
import Text.XML
import Data.Random.Generics

instance Arbitrary Document where
  arbitrary = sized $ generatorWith [textAlias] asGen . (4 *)

textAlias :: AliasR Gen
textAlias = aliasR $ \() -> fmap Text.pack (listOf (choose ('a', 'z')))

main = sample' (arbitrary :: Gen Document) >>=
  mapM_ (Text.putStrLn . renderText def)
