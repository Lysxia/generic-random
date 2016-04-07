-- | Solve systems of equations

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, RecordWildCards #-}
module Data.Random.Generics.Boltzmann.Solver where

import Control.Applicative
import Data.Data
import Data.Function
import Data.List
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet
import Generics.Deriving ( Generic )
import Numeric.LinearAlgebra

data Exp
  = X Int
  | Constant Integer
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
  fromInteger = Constant

pattern Zero = Constant 0
pattern One = Constant 1

isZero, isOne :: Exp -> Bool

isZero Zero = True
isZero _ = False

isOne One = True
isOne _ = False

eval :: Num a => (Int -> a) -> Exp -> a
eval x (X i) = x i
eval _ (Constant n) = fromInteger n
eval x (Sum xs) = sum (fmap (eval x) xs)
eval x (Prod xs) = product (fmap (eval x) xs)

differentiate :: Int -> Exp -> Exp
differentiate i (X j) | i == j = One
differentiate _ (X _) = Zero
differentiate _ (Constant _) = Zero
differentiate i (Sum xs) = sum' $ fmap (differentiate i) xs
differentiate i (Prod xs) = sum' $
  [ prod' (differentiate i x : xs') | (x, xs') <- selectFrom xs ]

selectFrom :: [a] -> [(a, [a])]
selectFrom xs =
  [ (x, a ++ b) | (a, x : b) <- (liftA2 zip inits tails) xs ]

prod' :: [Exp] -> Exp
prod' = prod_ 1 []
  where
    prod_ 0 _ _ = Zero
    prod_ n xs' (Prod xs0 : xs) = prod_ n xs' (xs0 ++ xs)
    prod_ n xs' (Constant m : xs) = prod_ (n * m) xs' xs
    prod_ n xs' (x : xs) = prod_ n (x : xs') xs
    prod_ n [] [] = Constant n
    prod_ 1 [x] [] = x
    prod_ 1 xs' [] = Prod (reverse xs')
    prod_ n xs' [] = Prod (Constant n : reverse xs')

sum' :: [Exp] -> Exp
sum' = sum_ 0 []
  where
    sum_ n xs' (Sum xs0 : xs) = sum_ n xs' (xs0 ++ xs)
    sum_ n xs' (Constant m : xs) = sum_ (n + m) xs' xs
    sum_ n xs' (x : xs) = sum_ n (x : xs') xs
    sum_ n [] [] = Constant n
    sum_ 0 [x] [] = x
    sum_ 0 xs' [] = Sum (reverse xs')
    sum_ n xs' [] = Sum (Constant n : reverse xs')

collectXs :: Exp -> IntSet
collectXs (X i) = IntSet.singleton i
collectXs (Constant _) = IntSet.empty
collectXs (Sum xs) = (IntSet.unions . fmap collectXs) xs
collectXs (Prod xs) = (IntSet.unions . fmap collectXs) xs

-- | e1 == e2, also e1 - e2 == 0
data Equation = Exp := Exp
  deriving (Eq, Ord, Show)

infix 4 :=

-- Evaluate the difference of expressions.
evalDelta :: Vector R -> Equation -> R
evalDelta x (e1 := e2) = ((-) `on` eval (x !)) e1 e2

evalDeltas :: Vector R -> [Equation] -> Vector R
evalDeltas x = vector . fmap (evalDelta x)

-- Evaluate the jacobian of the differences of expressions [e1i - e2i | i]
deltaJacobian :: [Equation] -> [[Equation]]
deltaJacobian es =
    [ [ d i e1 := d i e2 | i <- [0 .. n-1] ] | e1 := e2 <- es ]
  where n = length es ; d = differentiate

evalDeltaJacobian :: Vector R -> [[Equation]] -> Matrix R
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
