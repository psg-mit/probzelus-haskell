{-# LANGUAGE Arrows, FlexibleContexts, ConstraintKinds #-}

module Examples.Isee (
  module Examples.Isee,
  Weighted
) where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Weighted (Weighted, runWeighted)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)
import Control.Monad.Bayes.Class (MonadSample, MonadCond)
import Control.Monad.State (StateT, MonadFix)
import Control.Monad.Trans (lift)

import qualified Distributions as D
import Data.Random (RVar, runRVar, StdRandom (..))
import Util.RandomUtil
import Util.Numeric (average)

import Numeric.Log (Log (Exp))

import Inference (zparticles, zdsparticles', zdsparticles)
import qualified Metaprob as G
import Metaprob (Gen, (~~), (|->), Trace)

import qualified Util.ZStream as ZS
import Util.ZStream (ZStream)

import DelayedSampling (DelayedSampling, DelayedSample, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (M, Expr, Expr' (..), marginal', Result, meanResult, zdeepForce)
import Util.Ref (Heap)


type Sampler = SamplerIO
type MSample = MonadSample
type Delayed = StateT Heap
type Delayed' s = StateT Heap


sampleProb :: Sampler a -> IO a
sampleProb = sampleIO

weightedSample :: Weighted Sampler a -> IO (a, Double)
weightedSample = sampleProb . fmap ((\(x, Exp w) -> (x, w))) . runWeighted

unLog :: Log Double -> Double
unLog (Exp x) = exp x

importanceSamplingExpectation :: Int -> Weighted Sampler Double -> Sampler Double
importanceSamplingExpectation n w = do
  xs <- replicateM n (runWeighted w)
  pure (sum [ unLog w * x | (x, w) <- xs] / sum [ unLog w | (x, w) <- xs] )

sampleVariance :: Int -> Sampler Double -> Sampler Double
sampleVariance n d = do
  xs <- replicateM n d
  let mean = sum xs / fromIntegral n
  pure (sum [ (x - mean)^2 | x <- xs ] / fromIntegral n)

sampleGen :: Gen Sampler a -> IO a
sampleGen = sampleProb . G.sim

sampleGenWithTrace :: Gen Sampler a -> IO (a, Trace)
sampleGenWithTrace = sampleProb . G.sim . G.traced



unbiasedCoinflip :: MSample m => Gen m Bool
unbiasedCoinflip = "ht" ~~ G.bernoulli 0.5


unbiasedCoinflips :: MSample m => Gen m [Bool]
unbiasedCoinflips = G.replicate 5 unbiasedCoinflip

biasedCoinflips :: MSample m => Gen m (Double, [Bool])
biasedCoinflips = do
  pr <- "pr" ~~ G.beta 0.5 0.5
  flips <- "flips" ~~ G.replicate 5 (G.bernoulli pr)
  return (pr, flips)

biasedAllHeads :: MSample m => Weighted m Double
biasedAllHeads = do
  (pr, flips) <- G.observing ("flips" |-> G.list (replicate 5 True)) biasedCoinflips
  return pr

biasedAllHeadsPosterior :: MSample m => Gen m Double
biasedAllHeadsPosterior = G.beta 5.5 0.5

biasedAllHeadsPosteriorImportanceSampler :: Weighted Sampler Double
biasedAllHeadsPosteriorImportanceSampler = lift (G.sim biasedAllHeadsPosterior)

-- Of course, the variance estimating the mean probability is lower with the exact posterior

biasedAllHeadsImportanceSamplingVariance :: Sampler Double
biasedAllHeadsImportanceSamplingVariance =
  sampleVariance 1000 (importanceSamplingExpectation 10 biasedAllHeads)

biasedAllHeadsExactPosteriorSamplingVariance :: Sampler Double
biasedAllHeadsExactPosteriorSamplingVariance =
  sampleVariance 1000 (importanceSamplingExpectation 10 biasedAllHeadsPosteriorImportanceSampler)

unbiasedAllHeads :: Weighted Sampler Double
unbiasedAllHeads = do
  replicateM 5 (D.observe (D.bernoulli 0.5) True)
  return 0.5

isCoinFair :: MSample m => Gen m (Bool, Double, [Bool])
isCoinFair = do
  coinFair <- "fair" ~~ G.bernoulli 0.7
  pr <- if coinFair
    then return 0.5
    else "pr" ~~ G.beta 0.5 0.5
  flips <- "flips" ~~ G.replicate 5 (G.bernoulli pr)
  return (coinFair, pr, flips)

biasedCoinflipsGen :: Gen Sampler Double
biasedCoinflipsGen = do
  pr <- "pr" ~~ G.beta 0.5 0.5
  "obs" ~~ G.replicate 5 (G.bernoulli pr)
  return pr

biasedCoinflipsGenAllHeads :: Weighted Sampler Double
biasedCoinflipsGenAllHeads = G.observing' ("obs" |-> G.list (replicate 5 True)) biasedCoinflipsGen



-- Okay, now Streams!

naturals :: Monad m => ZStream m () Double
naturals = ZS.fromStep (\n () -> return (n + 1, n + 1)) 0

delay :: Monad m => a -> ZStream m a a
delay init = ZS.fromStep (\last current -> return (current, last)) init

integrate :: Monad m => Double -> ZStream m Double Double
integrate init = ZS.fromStep (\currentSum toAdd -> let newSum = currentSum + toAdd in return (newSum, newSum)) init

fibonacci :: MonadFix m => ZStream m () Double
fibonacci = proc () -> do
  rec n <- ZS.returnA -< n' + n''
      n' <- delay 1 -< n
      n'' <- delay 1 -< n'
  ZS.returnA -< n

triangular_numbers :: Monad m => ZStream m () Double
triangular_numbers = proc () -> do
  n <- naturals -< ()
  integrate 0 -< n

-- Discrete time analogue of Ito processes
noisyIntegrate :: Double -> ZStream Sampler (Double, Double) Double
noisyIntegrate = ZS.fromStep $ \currentSum (mean, stdDev) -> do
    newSum <- D.sample (D.normal (currentSum + mean) stdDev)
    pure (newSum, newSum)

kalman1D :: ZStream Sampler () Double
kalman1D = proc () -> do
  actualPosition <- noisyIntegrate 0 -< (0, 1)
  ZS.zconstM (\x -> D.sample (D.normal x 3)) -< actualPosition

-- Discrete time analogue of Ito processes
noisyIntegrateGen :: Double -> ZStream (Gen Sampler) (Double, Double) Double
noisyIntegrateGen = ZS.fromStep $ \currentSum (mean, stdDev) -> do
    newSum <- G.normal (currentSum + mean) stdDev
    pure (newSum, newSum)

kalman1DGen :: ZStream (Gen Sampler) () (Double, Double)
kalman1DGen = proc () -> do
  actualPosition <- ZS.liftM ("x" ~~) (noisyIntegrateGen 0) -< (0, 1)
  observedPosition <- ZS.liftM ("obs" ~~) (ZS.always (\x -> G.normal x 3)) -< actualPosition
  ZS.returnA -< (actualPosition, observedPosition)

kalman1DObserved :: ZStream (Weighted Sampler) Double Double
kalman1DObserved = proc obs -> do
  (actualPosition, observedPosition) <- G.zobserving' kalman1DGen -< ((), "obs" |-> G.atom obs)
  ZS.returnA -< actualPosition

kalman1DParticles :: Int -> ZStream Sampler Double [Double]
kalman1DParticles numParticles = zparticles numParticles kalman1DObserved

kalman1DParticlesInference :: ZStream Sampler () (Double, Double, Double)
kalman1DParticlesInference = proc () -> do
  (actualPosition, observedPosition) <- ZS.liftM G.sim kalman1DGen -< ()
  particles <- kalman1DParticles 1000 -< observedPosition
  ZS.returnA -< (observedPosition, actualPosition, average particles)

runKalman1DParticles :: IO ()
runKalman1DParticles = ZS.runStream print (ZS.liftM sampleProb kalman1DParticlesInference)


-- Delayed sampling

noisyIntegrateDelayed :: DelayedSample m => Expr Double -> ZStream m (Expr Double, Double) (Expr Double)
noisyIntegrateDelayed = ZS.fromStep $ \currentSum (mean, stdDev) -> do
    newSum <- DS.sample (DS.normal (currentSum + mean) stdDev)
    pure (newSum, newSum)

kalman1DGenDelayed :: DelayedSample m => ZStream (Gen m) () (Expr Double, Expr Double)
kalman1DGenDelayed = proc () -> do
  actualPosition <- ZS.lift (noisyIntegrateDelayed (Const 0)) -< (Const 0, 1)
  observedPosition <- ZS.zconstM (\x -> "obs" ~~ (G.dsPrim (DS.normal x 3))) -< actualPosition
  ZS.returnA -< (actualPosition, observedPosition)

kalman1DObservedDelayed :: DelayedInfer m => ZStream m Double (Result Double)
kalman1DObservedDelayed = proc obs -> do
  (actualPosition, observedPosition) <- G.zobserving kalman1DGenDelayed -< ((), "obs" |-> G.atom obs)
  ZS.run -< marginal' actualPosition

-- Every particle will be identical, because everything is marginalized here
kalman1DParticlesDelayed :: ZStream Sampler Double [Result Double]
kalman1DParticlesDelayed = zdsparticles 1 kalman1DObservedDelayed

kalman1DParticlesInferenceDelayed :: ZStream Sampler () (Double, Double, [Result Double]) --Double)
kalman1DParticlesInferenceDelayed = proc () -> do
  (actualPosition, observedPosition) <- zdeepForce (ZS.liftM G.sim kalman1DGenDelayed) -< ()
  particles <- kalman1DParticlesDelayed -< observedPosition
  ZS.returnA -< (observedPosition, actualPosition, particles)-- average (map meanResult particles))

runKalman1DParticlesDelayed :: IO ()
runKalman1DParticlesDelayed = ZS.runStream print (ZS.liftM sampleProb kalman1DParticlesInferenceDelayed)

comparison :: ZStream Sampler () (Double, Double, Double)
comparison = proc () -> do
  (actualPosition, observedPosition) <- ZS.liftM G.sim kalman1DGen -< ()
  particles <- kalman1DParticles 10 -< observedPosition
  delayedParticles <- kalman1DParticlesDelayed -< observedPosition
  ZS.returnA -< (actualPosition, average particles - actualPosition, average (map meanResult delayedParticles) - actualPosition)