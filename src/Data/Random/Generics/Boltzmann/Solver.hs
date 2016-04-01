-- | Representing systems of generating functions

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, RecordWildCards #-}
module Data.Random.Generics.Boltzmann.Solver where

import Control.Applicative
import Data.Data
import Data.Function
import Data.List
import Generics.Deriving (Generic)
import Numeric.LinearAlgebra

data Exp
  = X Int
  | Sum [Exp]
  | Prod [Exp]
  deriving (Eq, Ord, Show, Data, Generic, Typeable)

instance Num Exp where
  a + b = sum' [a, b]
  a * b = prod' [a, b]
  (-) = error "(-) is not defined for Exp."
  negate = error "negate is not defined for Exp."
  abs = id
  signum x | isZero x = Zero
  signum x = x
  fromInteger n = Sum (replicate (fromInteger n) One)

pattern Zero = Sum []
pattern One = Prod []

isZero Zero = True
isZero _ = False

isOne One = True
isOne _ = False

eval :: Num a => (Int -> a) -> Exp -> a
eval x (X i) = x i
eval x (Sum xs) = sum (fmap (eval x) xs)
eval x (Prod xs) = product (fmap (eval x) xs)

differentiate :: Int -> Exp -> Exp
differentiate i (X j) | i == j = One
differentiate _ (X _) = Zero
differentiate i (Sum xs) = sum' $ fmap (differentiate i) xs
differentiate i (Prod xs) = sum' $
  [ prod' (differentiate i x : xs') | (x, xs') <- selectFrom xs ]

selectFrom :: [a] -> [(a, [a])]
selectFrom xs =
  [ (x, a ++ b) | (a, x : b) <- (liftA2 zip inits tails) xs ]

prod' :: [Exp] -> Exp
prod' xs | any isZero xs = Zero
prod' xs = (Prod . flattenProds . filter (not . isOne)) xs
  where
    flattenProds = (>>= \case Prod xs -> xs ; x -> [x])

sum' :: [Exp] -> Exp
sum' = Sum . flattenSums . filter (not . isZero)
  where
    flattenSums = (>>= \case Sum xs -> xs ; x -> [x])

-- | e1 == e2, also e1 - e2 == 0
type Equation = (Exp, Exp)

-- Evaluate the difference of expressions.
evalDelta :: Vector R -> Equation -> R
evalDelta x (e1, e2) = ((-) `on` eval (x !)) e1 e2

evalDeltas :: Vector R -> [Equation] -> Vector R
evalDeltas x = vector . fmap (evalDelta x)

-- Evaluate the jacobian of the differences of expressions [e1i - e2i | i]
deltaJacobian :: [Equation] -> [[Equation]]
deltaJacobian es =
    [ [ (d i e1, d i e2) | i <- [0 .. n-1] ] | (e1, e2) <- es ]
  where n = length es ; d = differentiate

evalDeltaJacobian :: Vector R -> [[Equation]] -> Matrix R
evalDeltaJacobian x = fromLists . (fmap . fmap) (evalDelta x)

data SolveArgs = SolveArgs
  { accuracy :: Double
  , numIterations :: Int
  } deriving (Eq, Ord, Show)

defSolveArgs = SolveArgs 1e-8 100

solve
  :: SolveArgs
  -> (Vector R -> Vector R)
  -> (Vector R -> Matrix R)
  -> Vector R -- Initial guess
  -> Maybe (Vector R)
solve SolveArgs{..} f jacobian = newton numIterations
  where
    newton 0 _ = Nothing
    newton n x =
      let y = f x
      in if norm_Inf y > accuracy then
        newton (n - 1) (x - jacobian x <\> y)
      else
        Just x

solveEquations
  :: SolveArgs
  -> [Equation]
  -> Vector R -- Initial guess
  -> Maybe (Vector R)
solveEquations args es = solve args f jacobian
  where
    f x = evalDeltas x es
    jacobian x = evalDeltaJacobian x jb
    jb = deltaJacobian es

solveEquations'
  :: SolveArgs
  -> [Equation]
  -> Vector R
solveEquations' args es = retry [0, 0.1 .. 1]
  where
    n = length es
    retry [] = error "Root not found."
    retry (x : xs) =
      case solveEquations args es (vector (x : replicate (n-1) 100)) of
        Nothing -> retry xs
        Just x' -> x'
