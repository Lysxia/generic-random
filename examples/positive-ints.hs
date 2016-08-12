-- Example of using @Alias@ to restrict the domain of some subterms.
--
-- We generate a list of @Int@ between 0 and 100.
-- The alias wraps a user-defined generator for @Int@ values.

{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Random
import Generic.Random.Data

gen :: IO [Int]
gen = asMonadRandom $ generatorPWith aliases 20
  where
    aliases = coerceAliases [alias $ \() -> getRandomR (0, 100) :: IO Int]

main :: IO ()
main = gen >>= print
