{-# LANGUAGE Arrows, FlexibleContexts #-}

module Examples.Demo (
  module Examples.Demo,
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
import Metaprob (Gen, (~~))

import qualified Util.ZStream as ZS
import Util.ZStream (ZStream)

import DelayedSampling (DelayedSampling, DelayedSample, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (M, Expr, Expr' (..), marginal', Result, meanResult, zdeepForce)
import Util.Ref (Heap)


type Sampler = SamplerIO
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










unbiasedCoinflip :: Sampler Bool
unbiasedCoinflip = D.sample (D.bernoulli 0.5)

unbiasedCoinflips :: Sampler [Bool]
unbiasedCoinflips = replicateM 5 unbiasedCoinflip

biasedCoinflips :: Sampler [Bool]
biasedCoinflips = do
  pr <- D.sample (D.beta 0.5 0.5)
  replicateM 5 (D.sample (D.bernoulli pr))

biasedAllHeads :: Weighted Sampler Double
biasedAllHeads = do
  pr <- D.sample (D.beta 0.5 0.5)
  replicateM 5 (D.observe (D.bernoulli pr) True)
  return pr

biasedAllHeadsPosterior :: Sampler Double
biasedAllHeadsPosterior = D.sample (D.beta 5.5 0.5)

biasedAllHeadsPosteriorImportanceSampler :: Weighted Sampler Double
biasedAllHeadsPosteriorImportanceSampler = lift biasedAllHeadsPosterior

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

isCoinFair :: Weighted Sampler (Bool, Double)
isCoinFair = do
  coinFair <- D.sample (D.bernoulli 0.7)
  coinPr <- if coinFair
    then unbiasedAllHeads
    else biasedAllHeads
  return (coinFair, coinPr)

-- This will think it is too likely that the coin is unfair
isCoinFairBroken :: Weighted Sampler (Bool, Double)
isCoinFairBroken = do
  coinFair <- D.sample (D.bernoulli 0.7)
  coinPr <- if coinFair
    then unbiasedAllHeads
    else lift biasedAllHeadsPosterior
  return (coinFair, coinPr)


biasedAllHeadsPosteriorAdjusted :: Weighted Sampler Double
biasedAllHeadsPosteriorAdjusted = do
  D.factor (D.logBeta 5.5 0.5 - D.logBeta 0.5 0.5)
  D.sample (D.beta 5.5 0.5)

isCoinFairImproved :: Weighted Sampler (Bool, Double)
isCoinFairImproved = do
    coinFair <- D.sample (D.bernoulli 0.7)
    coinPr <- if coinFair
      then unbiasedAllHeads
      else biasedAllHeadsPosteriorAdjusted
    return (coinFair, coinPr)

biasedCoinflipsGen :: Gen Sampler Double
biasedCoinflipsGen = do
  pr <- "pr" ~~ (G.prim (D.beta 0.5 0.5))
  "obs" ~~ (G.replicate 5 (G.prim (D.bernoulli pr)))
  return pr

biasedCoinflipsGenAllHeads :: Weighted Sampler Double
biasedCoinflipsGenAllHeads = G.observing' ("obs" G.|-> G.trList (replicate 5 True)) biasedCoinflipsGen



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
noisyIntegrate = ZS.fromStep $ \currentSum (mean, var) -> do
    newSum <- D.sample (D.normal (currentSum + mean) var)
    pure (newSum, newSum)

kalman1D :: ZStream Sampler () Double
kalman1D = proc () -> do
  actualPosition <- noisyIntegrate 0 -< (0, 1)
  ZS.zconstM (\x -> D.sample (D.normal x 3)) -< actualPosition

-- Discrete time analogue of Ito processes
noisyIntegrateGen :: Double -> ZStream (Gen Sampler) (Double, Double) Double
noisyIntegrateGen = ZS.fromStep $ \currentSum (mean, var) -> do
    newSum <- G.normal (currentSum + mean) var
    pure (newSum, newSum)

kalman1DGen :: ZStream (Gen Sampler) () (Double, Double)
kalman1DGen = proc () -> do
  actualPosition <- ZS.liftM ("x" ~~) (noisyIntegrateGen 0) -< (0, 1)
  observedPosition <- ZS.liftM ("obs" ~~) (ZS.always (\x -> G.prim (D.normal x 3))) -< actualPosition
  ZS.returnA -< (actualPosition, observedPosition)

kalman1DObserved :: ZStream (Weighted Sampler) Double Double
kalman1DObserved = proc obs -> do
  (actualPosition, observedPosition) <- G.zobserving' kalman1DGen -< ((), "obs" G.|-> G.obs obs)
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
noisyIntegrateDelayed = ZS.fromStep $ \currentSum (mean, var) -> do
    newSum <- DS.sample (DS.normal (currentSum + mean) var)
    pure (newSum, newSum)

kalman1DGenDelayed :: DelayedSample m => ZStream (Gen m) () (Expr Double, Expr Double)
kalman1DGenDelayed = proc () -> do
  actualPosition <- ZS.lift (noisyIntegrateDelayed (Const 0)) -< (Const 0, 1)
  observedPosition <- ZS.zconstM (\x -> "obs" ~~ (G.dsPrim (DS.normal x 3))) -< actualPosition
  ZS.returnA -< (actualPosition, observedPosition)

kalman1DObservedDelayed :: DelayedInfer m => ZStream m Double (Result Double)
kalman1DObservedDelayed = proc obs -> do
  (actualPosition, observedPosition) <- G.zobserving kalman1DGenDelayed -< ((), "obs" G.|-> G.obs obs)
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
  ZS.returnA -< (actualPosition, average particles, average (map meanResult delayedParticles))