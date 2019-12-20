{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}

module Examples.MultiTargetTracking where

import Control.Arrow (returnA)
import Control.Monad (forM)
import Control.Monad.Bayes.Class (MonadSample, MonadInfer)

import qualified Data.Map as M
import Data.Maybe (fromJust)

import Numeric.LinearAlgebra.Static hiding ((<>))

import Inference (zdsparticles)
import DelayedSampling (DelayedSample, DelayedInfer, Result (..))
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Expr' (..), Expr, marginal, zdeepForce, deepForce', forgetE)
import Distributions
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Metaprob ((~~), Gen, lift, (|->), obs)
import qualified Metaprob as MP
import Examples.Demo (Sampler, Delayed, Weighted)

data ObsType = Clutter | NewTrack | Track Int
  deriving (Eq, Show, Ord)

filterM :: Monad m => (k -> v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterM f = M.traverseMaybeWithKey $ \k v -> do
  b <- f k v
  return (if b then Just v else Nothing)

type TrackMap = M.Map Int (Expr (R 4))

tdiff, birthRate, deathRate, clutterLambda, newTrackLambda, pd, survivalProb :: Double
tdiff = 1
birthRate = 0.1
deathRate = 0.02
clutterLambda = 3
newTrackLambda = birthRate * tdiff
pd = 0.8
survivalProb = exp (- tdiff * deathRate)

clutterDistr :: DS.Distr (Expr (R 2))
clutterDistr = DS.mvNormal (Const (0 :: R 2)) (10 * sym eye)

newTrackD :: DelayedSample m => m (Expr (R 4))
newTrackD = DS.sample (DS.mvNormal (Const mu) cov)
  where
  mu = 0 :: R 4
  cov = sym (((5 * eye :: Sq 2) ||| (0 :: Sq 2))
            ===
            ((0 :: Sq 2) ||| (0.1 * eye :: Sq 2)))

trackMotion :: DelayedSample m => Double -> Expr (R 4) -> m (Expr (R 4))
trackMotion tdiff track = DS.sample (DS.mvNormal (MVMul (Const motionMatrix) track) motionCov)
  where
  motionMatrix :: Sq 4
  motionMatrix = eye + konst tdiff *
    (((0 :: Sq 2) ||| (eye :: Sq 2))
    ===
    ((- 1 / 100 * eye :: Sq 2) ||| (-0.1 * eye :: Sq 2)))
  motionCov :: Sym 4
  motionCov = sym $ konst tdiff * (((0.01 * eye :: Sq 2) ||| (0 :: Sq 2))
                                  ===
                                  ((0 :: Sq 2) ||| (0.1 * eye :: Sq 2)))

trackMeasurement :: Expr (R 4) -> DS.Distr (Expr (R 2))
trackMeasurement posvel = DS.mvNormal (MVMul (Const posFromPosVel) posvel) (sym eye)
  where
  posFromPosVel :: L 2 4
  posFromPosVel = (eye :: Sq 2) ||| (0 :: Sq 2)

updateWithAssocs :: DelayedSample m => Int -> TrackMap -> [ObsType] -> m ((TrackMap, Int), [DS.Distr (Expr (R 2))])
updateWithAssocs nextTrackID updatedOldTracks assocs = do
  (obsDists, newTrackPVs) <- fmap mconcat . forM assocs $ \k -> case k of
    Clutter -> return ([clutterDistr], [])
    NewTrack -> do
      newTrackPV <- newTrackD
      return ([trackMeasurement newTrackPV], [newTrackPV])
    Track i -> return ([trackMeasurement (updatedOldTracks M.! i)], [])
  coasted <- filterM (\_ _ -> sample (bernoulli ((1 - pd) / (1 - survivalProb * pd)))) notObserved
  return ((mconcat (observed : coasted : zipWith M.singleton [nextTrackID..] newTrackPVs),
           nextTrackID + length newTrackPVs), obsDists)
  where
  (observed, notObserved) = M.partitionWithKey (\i _ -> Track i `elem` assocs) updatedOldTracks

sampleStep :: DelayedSample m => (TrackMap, Int) -> Gen m ((TrackMap, Int), [Expr (R 2)])
sampleStep (allOldTracks, nextTrackID) = do
  updatedOldTracks <- lift (mapM (trackMotion tdiff) allOldTracks)
  assocs <- "assocs" ~~ MP.prim (shuffleWithRepeats' countDistrs)
  (newTrackInfo, obsDists) <- lift (updateWithAssocs nextTrackID updatedOldTracks assocs)
  observations <- "observations" ~~ MP.isequence (map MP.dsPrim obsDists)
  return (newTrackInfo, observations)
  where
  countDistrs = M.singleton Clutter (poisson clutterLambda)
             <> M.singleton NewTrack (poisson newTrackLambda)
             <> M.mapKeys Track (fmap (\_ -> bernoulli01 (survivalProb * pd)) allOldTracks)

observeStep :: DelayedInfer m => (TrackMap, Int) -> [R 2] -> m ((TrackMap, Int), [Expr (R 2)])
observeStep (allOldTracks, nextTrackID) observations = do
  updatedOldTracks <- mapM (trackMotion tdiff) allOldTracks
  -- assocs <- proposeAssocs observations updatedOldTracks
  MP.observingWithProposal
    ("observations" |-> MP.trList observations) (sampleStep (allOldTracks, nextTrackID))
    "assocs" (proposeAssocs 0 observations updatedOldTracks)



proposeAssocs :: DelayedInfer m => Int -> [R 2] -> TrackMap -> Gen m [ObsType]
proposeAssocs j (obs : observations) remainingTracks = do
  newTrack <- lift $ newTrackD
  let distrs = M.singleton Clutter (clutterLambda, clutterDistr)
        <> M.singleton NewTrack (newTrackLambda, trackMeasurement newTrack)
        <> M.mapKeys Track (fmap (\t -> (survivalProb * pd, trackMeasurement t)) remainingTracks)
  likes <- lift $ mapM (\(intensity, d) -> (\ll -> intensity * exp ll) <$> DS.score d obs) distrs
  let assocDistr = let probs = fmap (/ sum likes) likes in categoricalM probs
  i <- ("assoc" ++ show j) ~~ MP.prim assocDistr
  let remainingTracks' = case i of
        Track k -> M.delete k remainingTracks
        _ -> remainingTracks
  (i :) <$> proposeAssocs (j + 1) observations remainingTracks'
proposeAssocs _ [] remainingTracks = return []

generateGroundTruth :: DelayedSample m => ZStream m () (TrackMap, [Expr (R 2)])
generateGroundTruth = ZS.fromStep stepf initState where
  initState = (mempty, 0)
  stepf state () = do
    (newState@(tracks', _), observations) <- MP.sim (sampleStep state)
    return (newState, (tracks', observations))

processObservations :: DelayedInfer m => ZStream m [R 2] (M.Map Int (Result (R 4)))
processObservations = ZS.fromStep stepf initState where
  initState = (mempty, 0)
  stepf state obs = do
    (newState@(tracks', _), _) <- observeStep state obs
    marginalTracks <- mapM (fmap fromJust . marginal) tracks'
    return (newState, marginalTracks)

runMTTPF :: Int -> ZStream Sampler () ([(Int, R 4)], [[(Int, Result (R 4))]], [R 2])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservations -< obs
  returnA -< (M.assocs groundTruth, map M.assocs particles, obs)