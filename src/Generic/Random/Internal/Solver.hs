-- | Solve systems of equations

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}
module Generic.Random.Internal.Solver where

import Control.Applicative
import Data.AEq ( (~==) )
import Numeric.AD.Mode
import Numeric.AD.Mode.Forward
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

data SolveArgs = SolveArgs
  { accuracy :: Double
  , numIterations :: Int
  } deriving (Eq, Ord, Show)

defSolveArgs :: SolveArgs
defSolveArgs = SolveArgs 1e-8 20

findZero
  :: SolveArgs
  -> (forall s. V.Vector (AD s (Forward R)) -> V.Vector (AD s (Forward R)))
  -> Vector R
  -> Maybe (Vector R)
findZero SolveArgs{..} f = newton numIterations
  where
    newton 0 _ = Nothing
    newton n x
      | norm_y == 1/0 = Nothing
      | norm_y > accuracy = newton (n - 1) (x - jacobian <\> y)
      | otherwise = Just x
      where
        norm_y = norm_Inf y
        jacobian = (fromRows . V.toList . fmap (V.convert . snd)) yj
        y = (V.convert . fmap fst) yj
        yj = jacobian' f (S.convert x)

fixedPoint
  :: SolveArgs
  -> (forall a. (Mode a, Scalar a ~ R) => V.Vector a -> V.Vector a)
  -> V.Vector R
  -> Maybe (V.Vector R)
fixedPoint args f =
  fmap S.convert . findZero args (liftA2 (V.zipWith (-)) f id) . S.convert

-- | Assuming @p . f@ is satisfied only for positive values in some interval
-- @(0, r]@, find @f r@.
search :: (Double -> a) -> (a -> Bool) -> (Double, a)
search f p = search' e0 (0 : [2 ^ n | n <- [0 .. 100 :: Int]])
  where
    search' y (x : xs@(x' : _))
      | p y' = search' y' xs
      | otherwise = search'' y x x'
      where y' = f x'
    search' _ _ = error "Solution not found. Uncontradictable predicate?"
    search'' y x x'
      | x ~== x' = (x, y)
      | p y_ = search'' y_ x_ x'
      | otherwise = search'' y x x_
      where
        x_ = (x + x') / 2
        y_ = f x_
    e0 = error "Solution not found. Unsatisfiable predicate?"
