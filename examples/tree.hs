import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.Vector as V
import Test.QuickCheck
import Generic.Random.Boltzmann

data T = L | N T T
  deriving Show

size :: T -> Int
size L = 1
size (N l r) = 1 + size l + size r

s :: Module f => System f T ()
s = System dim sys
  where
    dim = 3
    sys x self = (
      let
        [leaf, node, tree] = V.toList self
      in
        V.fromList
          [ x $> L
          , x *> (N <$> tree <*> tree)
          , leaf <|> node
          ], ())

g :: Gen T
g = sizedGenerator s 2 0 (Just 10)

totalSize :: Gen Int
totalSize = fmap sum (replicateM 1000 (fmap size g))

oracle = solveSized s 2 0 (Just 10)

main = do
  print oracle
  sample g
  sample totalSize
