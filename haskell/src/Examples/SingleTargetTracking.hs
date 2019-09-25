{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}

module Examples.SingleTargetTracking where

import Control.Arrow hiding ((|||))
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Bayes.Class (MonadSample, MonadCond, logCategorical)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)

import Data.Aeson (encode)
import Data.Maybe (fromJust)

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)

import Numeric.LinearAlgebra.Static

import Inference (zdsparticles, zunheap, zparticles)
import DelayedSampling (MDistr (..), children, state, State, MarginalT (..), SMarginalT(..), distr, typeOfDSDistr, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce, deepForce', zdeepForce')
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap, emptyHeap, readRef)

import qualified Metaprob as MP

import Examples.MultiTargetTracking (TrackG (..), MarginalTrack, Track, STrack)


model :: MonadState Heap m => MonadSample m => Bool -> ZStream (MP.Gen m) () (Expr (R 6), Expr (R 3))
model delay = ZS.fromStep step initPosVel
  where
  initPosVel :: Expr (R 6)
  initPosVel = Const (konst 0 :: R 6)
  step posvel () = do
    posvel' <- lift (force (DS.sample (DS.mvNormal (MVMul (Const motionMatrix) posvel) motionCov)))
    observation <- MP.at "obs" (MP.dsPrim (DS.mvNormal (MVMul (Const posFromPosVel) posvel') (10 * sym eye)))
    return (posvel', (posvel', observation))

  force = if delay then id else (>>= deepForce')
  -- Constants
  posCov = 0.01 * sym eye
  velCov = 0.1 * sym eye
  tdiff = 1
  motionCov :: Sym 6
  motionCov =
    sym $ konst tdiff * ((unSym posCov ||| (konst 0 :: Sq 3))
          ===
          ((konst 0 :: Sq 3) ||| unSym velCov))
  motionMatrix :: Sq 6
  motionMatrix =
      ((eye :: Sq 3) ||| (konst tdiff * eye :: Sq 3))
                    ===
      ((konst 0 :: Sq 3) ||| (eye :: Sq 3))
  posFromPosVel :: L 3 6
  posFromPosVel = (eye :: Sq 3) ||| (konst 0 :: Sq 3)

processObservationStream :: DelayedInfer m => Bool -> ZStream m (R 3) (Result (R 6))
processObservationStream delay = proc observations -> do
  (posvel, _) <- MP.zobserving (model delay) -< ((), MP.tr "obs" observations)
  ZS.run -< fromJust <$> marginal posvel

runInference :: Bool -> Int -> ZStream SamplerIO () (R 6, [Result (R 6)], R 3)
runInference delay numParticles = proc () -> do
  (groundTruth, obs) <- simulate (model True) -< ()
  particles <- zdsparticles numParticles (processObservationStream delay) -< obs
  returnA -< (groundTruth, particles, obs)

runExample :: Bool -> Int -> IO ()
runExample delay n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode . f) (runInference delay n)
  where f (gt, particles, obs) = ([Track gt 0 0], map (\x -> [Track x 0 0]) particles, [obs])

simulate = zdeepForce . ZS.liftM MP.sim