-- | Solve systems of equations

{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, RecordWildCards #-}
module Data.Random.Generics.Boltzmann.Solver where

import Data.Data
import Data.Function
import Numeric.LinearAlgebra

data Exp a
  = X Int
  | Constant a
  | Exp a :+: Exp a
  | Exp a :*: Exp a
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

instance (Eq a, Num a) => Num (Exp a) where
  Zero + b = b
  a + Zero = a
  Constant m + Constant n = Constant (m + n)
  Constant m + (Constant n :+: b) = Constant (m + n) :+: b
  a@(Constant _) + b = a :+: b
  a + b@(Constant _) = b :+: a
  (c@(Constant _) :+: a) + b = c + (a + b)
  a + (c@(Constant _) :+: b) = c :+: (a :+: b)
  a + b = a :+: b

  Zero * _ = Zero
  _ * Zero = Zero
  One * b = b
  a * One = a
  Constant m * Constant n = Constant (m * n)
  Constant m * (Constant n :*: b) = Constant (m * n) :*: b
  a@(Constant _) * b = a :*: b
  a * b@(Constant _) = b :*: a
  (c@(Constant _) :*: a) * b = c * (a * b)
  a * (c@(Constant _) :*: b) = c :*: (a :*: b)
  a * b = a :*: b

  (-) = error "(-) is not defined for Exp."
  negate = error "negate is not defined for Exp."
  abs = id
  signum x | isZero x = 0
  signum _ = 1
  fromInteger = Constant . fromInteger

pattern Zero = Constant 0
pattern One = Constant 1

isZero, isOne :: (Eq a, Num a) => Exp a -> Bool

isZero Zero = True
isZero _ = False

isOne One = True
isOne _ = False

eval :: Num a => (Int -> a) -> Exp a -> a
eval x (X i) = x i
eval _ (Constant a) = a
eval x (a :+: b) = eval x a + eval x b
eval x (a :*: b) = eval x a * eval x b

differentiate :: (Eq a, Num a) => Int -> Exp a -> Exp a
differentiate i (X j) | i == j = One
differentiate _ (X _) = Zero
differentiate _ (Constant _) = Zero
differentiate i (a :+: b) = differentiate i a + differentiate i b
differentiate i (a :*: b) = (differentiate i a * b) + (a * differentiate i b)

-- | e1 == e2, also e1 - e2 == 0
data Equation a = Exp a := Exp a
  deriving (Eq, Ord, Show)

infix 4 :=

-- Evaluate the difference of expressions.
evalDelta :: Vector R -> Equation R -> R
evalDelta x (e1 := e2) = ((-) `on` eval (x !)) e1 e2

evalDeltas :: Vector R -> [Equation R] -> Vector R
evalDeltas x = vector . fmap (evalDelta x)

-- Evaluate the jacobian of the differences of expressions [e1i - e2i | i]
deltaJacobian :: (Eq a, Num a) => [Equation a] -> [[Equation a]]
deltaJacobian es =
    [ [ d i e1 := d i e2 | i <- [0 .. n-1] ] | e1 := e2 <- es ]
  where n = length es ; d = differentiate

evalDeltaJacobian :: Vector R -> [[Equation R]] -> Matrix R
evalDeltaJacobian x = fromLists . (fmap . fmap) (evalDelta x)

data SolveArgs = SolveArgs
  { accuracy :: Double
  , numIterations :: Int
  } deriving (Eq, Ord, Show)

defSolveArgs :: SolveArgs
defSolveArgs = SolveArgs 1e-8 20

solve
  :: SolveArgs
  -> (Vector R -> Vector R)
  -> (Vector R -> Matrix R)
  -> Vector R -- Initial guess
  -> Maybe (Vector R)
solve SolveArgs{..} f jacobian = newton numIterations
  where
    newton 0 _ = Nothing
    newton n x
      | norm_y == 1/0 = Nothing
      | norm_y > accuracy = newton (n - 1) (x - jacobian x <\> y)
      | otherwise = Just x
      where
        norm_y = norm_Inf y
        y = f x

solveEquations
  :: SolveArgs
  -> [Equation R]
  -> Vector R -- Initial guess
  -> Maybe (Vector R)
solveEquations args es = solve args f jacobian
  where
    f x = evalDeltas x es
    jacobian x = evalDeltaJacobian x jb
    jb = deltaJacobian es

solveEquations'
  :: SolveArgs
  -> [Equation R]
  -> Vector R
solveEquations' args es = retry [0, 0.1 .. 1]
  where
    n = length es
    retry [] = error "Root not found."
    retry (x : xs) =
      case solveEquations args es (vector (x : replicate (n-1) 100)) of
        Nothing -> retry xs
        Just x' -> x'
