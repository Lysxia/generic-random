import Control.Applicative
import Data.Generics.Random.Boltzmann.GeneratingFunctions
import Numeric.LinearAlgebra
import Test.HUnit

main = runTestTT $ TestList
  [ test_solve
  ]

test_solve = "F(x) = 1 + x F(x)^2" ~: do
    print xs
    print (evalDeltas <$> xs <*> pure es)
  where
    x : fx : f'x : _ = fmap X [0 ..]
    expectedSize x n = (n * fx, x * f'x)
    es =
      [ expectedSize x 10000
      , (fx, 1 + x * fx * fx)
      , (f'x, fx * fx + 2 * x * f'x * fx)
      ]
    xs = solveEquations defSolveArgs es (vector [0, 1, 1])
