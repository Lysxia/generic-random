-- Example of using @Alias@ to restrict the domain of some subterms.
--
-- We generate a list of @Int@ between 0 and 100.
-- The alias wraps a user-defined generator for @Int@ values.

{-# LANGUAGE ScopedTypeVariables #-}
import Data.Random.Generics
import Control.Monad.Random

gen :: IO [Int]
gen = asMonadRandom $ pointedGeneratorWith aliases 20
  where
    aliases = coerceAliases [alias $ \() -> getRandomR (0, 100) :: IO Int]

main :: IO ()
main = gen >>= print
