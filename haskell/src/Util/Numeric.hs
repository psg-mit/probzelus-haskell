module Util.Numeric where

import Control.Arrow (second)
import Numeric.Log

average :: Fractional a => [a] -> a
average = go 0 0 where
  go n x [] = x / fromIntegral n
  go n x (z : zs) = go (n + 1) (x + z) zs

shiftByMax :: (Precise d, RealFloat d, Ord d) => [(a, Log d)] -> [(a, Log d)]
shiftByMax xs = map (second (/ x')) xs
  where x' = maximum $ map snd xs

exponentiateWeights :: (Precise d, RealFloat d, Ord d) => [(a, Log d)] -> [(a, d)]
exponentiateWeights = map (second (exp . ln)) . shiftByMax

weightedAverage :: Fractional a => [(a, a)] -> a
weightedAverage = go 0 0 where
  go w x [] = x / w
  go w x ((y, ll) : ys) = go (w + ll) (ll * y + x) ys

weightedAverageGeneric :: Fractional a => (a -> b -> b) -> b -> (b -> b -> b) -> [(b, a)] -> b
weightedAverageGeneric scale zero plus = go 0 zero where
  go w x [] = scale (recip w) x
  go w x ((y, ll) : ys) = go (w + ll) (scale ll y `plus` x) ys