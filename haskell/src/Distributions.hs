module Distributions where

import Control.Monad (replicateM)
import qualified Control.Monad.Bayes.Class as B
import Control.Monad.Bayes.Class (MonadSample, MonadCond)
import Numeric.SpecFunctions (logGamma)

import qualified Data.Vector as V

import Numeric.Log (Log (Exp))

import SymbolicArithmetic

type Distr = Distr' Double

data Distr' d a = Distr
  { sample :: forall m. MonadSample m => m a
  , score :: a -> d
  }

factor :: MonadCond m => Double -> m ()
factor = B.score . Numeric.Log.Exp

negInf :: Double
negInf = - 1 / 0

observe :: MonadCond m => Distr a -> a -> m ()
observe d x = factor (score d x)

data ExpFam a = ExpFam
  { dim :: Int
  , suffStats :: [Exp () Double]
  , logPartition :: [Double] -> Double
  , logBaseMeas :: Exp () Double
  , efSample :: forall m. MonadSample m => [Double] -> m a
  , efRepr :: a -> Double
  , mle :: [Double] -> [Double]
  , efDescribe :: [Double] -> String }

bernoulliEF :: ExpFam Bool
bernoulliEF = ExpFam
  { dim = 1
  , suffStats = [Var ()]
  , logPartition = undefined
  , logBaseMeas = undefined
  , efSample = \[p] -> B.bernoulli p
  , efRepr = \x -> if x then 1 else 0
  , mle = id
  , efDescribe = \[eta] -> let z = exp eta in "Bernoulli(" ++ show (z / (1 + z)) ++ ")"
  }

betaEF :: ExpFam Double
betaEF = ExpFam
  { dim = 2
  , suffStats = let x = Var () in  [log x, log (1 - x)]
  , logPartition = \[a, b] -> logBeta a b
  , logBaseMeas = let x = Var () in - log x - log (1 - x)
  , efSample = \[a, b] -> B.beta a b
  , efRepr = id
  , mle = \[lx, lox] -> undefined
  , efDescribe = \[a, b] -> "Beta(" ++ show a ++ ", " ++ show b ++ ")"
  }

logBeta :: Double -> Double -> Double
logBeta a b = logGamma a + logGamma b - logGamma (a + b)

logChoose :: Int -> Int -> Double
logChoose n k = logFact n - logFact k - logFact (n - k) where
  logFact i = logGamma (fromIntegral i + 1)

normalEF :: ExpFam Double
normalEF = ExpFam
  { dim = 2
  , suffStats = let x = Var () in [x, x^2]
  , logPartition = \[eta1, eta2] -> - eta1^2 / (4 * eta2) - 1 / 2 * log(-2 * eta2)
  , logBaseMeas = - 1 / 2 * log (2 * pi)
  , efSample = \eta -> let (mu, sigma2) = conv eta in
      B.normal mu (sqrt sigma2)
  , efRepr = id
  , mle = \[mu, ex2] -> let sigma2 = ex2 - mu^2 in normalNatParams mu sigma2
  , efDescribe = \eta -> let (mu, sigma2) = conv eta in
    "Normal(" ++ show mu ++ ", " ++ show sigma2 ++ ")"
  }
  where
  conv [eta1, eta2] = let sigma2 = - 1 / (2 * eta2) in (sigma2 * eta1, sigma2)
  conv _ = error "conv"

-- indep :: ExpFam a -> ExpFam b -> ExpFam (a, b)
-- indep efa efb = ExpFam
--   { dim = dim efa + dim efb
--   , suffStats = \(a, b) -> suffStats efa a ++ suffStats efb b
--   , logPartition = \eta -> let (eta1, eta2) = split eta in
--       logPartition efa eta1 + logPartition efb eta2
--   , logBaseMeas = \(a, b) -> logBaseMeas efa a + logBaseMeas efb b
--   , efSample = \eta -> let (eta1, eta2) = split eta in
--       (,) <$> efSample efa eta1 <*> efSample efb eta2
--   , mle = \ss -> let (ss1, ss2) = split ss in
--       mle efa ss1 ++ mle efb ss2
--   , efDescribe = \eta -> let (eta1, eta2) = split eta in
--     "Indep(" ++ efDescribe efa eta1 ++ ", " ++ efDescribe efb eta2 ++ ")"
--   }
--   where
--   split = splitAt (dim efa)

normalNatParams :: Fractional d => d -> d -> [d]
normalNatParams mu sigma2 = [mu / sigma2, -1 / (2 * sigma2)]

dirac :: Eq a => a -> Distr a
dirac x = Distr (return x) (\y -> if x == y then 0 else negInf)

dot :: Num d => [d] -> [d] -> d
dot xs ys = sum (zipWith (*) xs ys)

efToDistr :: ExpFam a -> [Double] -> Distr a
efToDistr ef naturalParams =
  Distr (efSample ef naturalParams)
  (\x -> let ev = eval (\_ -> efRepr ef x) in
    naturalParams `dot` map ev (suffStats ef) - logPartition ef naturalParams + ev (logBaseMeas ef))

bernoulli :: Double -> Distr Bool
bernoulli p = Distr (B.bernoulli p) (\b -> bernoulli_ll p (if b then 1 else 0))

categorical :: [Double] -> Distr Int
categorical ps = Distr (B.categorical (V.fromList ps)) (\i -> ps !! i)

bernoulli_ll :: Floating a => a -> a -> a
bernoulli_ll p b = b * log p + (1 - b) * log (1 - p)

-- WARNING: NOT NORMALIZED!
beta_ll :: Floating a => a -> a -> a -> a
beta_ll a b p = a * log p + b * log (1 - p)

beta :: Double -> Double -> Distr Double
beta a b = Distr (B.beta a b) (\x -> beta_ll a b x + logPartition betaEF [a, b])

gaussian_ll :: Floating a => a -> a -> a -> a
gaussian_ll mu sigma2 x = - x^2 / sigma2 + 2 * x * mu / sigma2 - mu^2 / sigma2 - log (2 * pi) - log sigma2

gaussian_ll' :: Floating a => a -> a -> a -> a
gaussian_ll' mu sigma2 x = - (x - mu)^2 / sigma2 - log (2 * pi) - log sigma2

gaussian :: Double -> Double -> Distr Double
gaussian mu sigma2 = Distr (B.normal mu (sqrt sigma2)) (gaussian_ll' mu sigma2)

normal = gaussian

poisson_ll :: Double -> Int -> Double
poisson_ll lambda k = fromIntegral k * log lambda - lambda - logGamma (fromIntegral k + 1)

poisson :: Double -> Distr Int
poisson lambda = Distr (B.poisson lambda) (poisson_ll lambda)

uniform :: Double -> Double -> Distr Double
uniform a b = Distr (B.uniform a b) $ \x ->
  if a <= x && x <= b
    then - log range
    else negInf
  where
  range = b - a

replicateNIID :: Int -> Distr a -> Distr [a]
replicateNIID n d = Distr (replicateM n (sample d)) (\obs ->
  if length obs == n
    then sum (map (score d) obs)
    else (-1 / 0))

uniformIntRange :: Int -> Distr Int
uniformIntRange max = Distr (B.uniformD [0 .. max - 1])
  (\obs -> if 0 <= obs && obs < max then - log (fromIntegral max) else -1 / 0)

bind :: Distr a -> (a -> Distr b) -> Distr (a, b)
bind d f = Distr (do {x <- sample d; y <- sample (f x); pure (x, y)})
                 (\(x, y) -> score d x + score (f x) y)

replicateIID :: forall a. Distr Int -> Distr a -> Distr [a]
replicateIID howMany d = Distr sam obs where
  sam :: MonadSample m => m [a]
  sam = do
    n <- sample howMany
    replicateM n (sample d)
  obs xs = score howMany (length xs) + sum (map (score d) xs)

geometric' :: MonadSample m => m Int
geometric' = do
  x <- B.bernoulli (0.5 :: Double)
  if x then return 0 else fmap (+1) geometric'

exponential :: MonadSample m => Double -> m Double
exponential lambda = do
  u <- B.random
  pure (- log u / lambda)

passert :: MonadCond m => Bool -> m ()
passert True = pure ()
passert False = B.score 0

data Some f where
  Some :: f a -> Some f

exponentialFamilies :: [Some ExpFam]
exponentialFamilies = [Some betaEF, Some bernoulliEF, Some normalEF]