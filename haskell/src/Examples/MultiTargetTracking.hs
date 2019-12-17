{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}

module Examples.MultiTargetTracking where

import Control.Arrow (returnA)
import Control.Monad (forM)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Bayes.Class (MonadSample, categorical, MonadInfer)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.List (partition)
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
import Metaprob ((~~), Gen)
import qualified Metaprob as MP

data ObsType = Clutter | NewTrack | Track Int
  deriving (Eq, Show, Ord)

mapFilterM :: Monad m => (k -> v -> m Bool) -> M.Map k v -> m (M.Map k v)
mapFilterM f = M.traverseMaybeWithKey $ \k v -> do
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

probMeasurement :: Distr Int
probMeasurement = bernoulli01 (survivalProb * pd)

clutterDistr :: DS.Distr (Expr (R 2))
clutterDistr = DS.mvNormal (Const (0 :: R 2)) (10 * sym eye)

newTrackD :: DelayedSample m => m (Expr (R 4))
newTrackD = DS.sample (DS.mvNormal (Const mu) cov)
  where
  mu = konst 0 :: R 4
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

shufflingDistr :: TrackMap -> Distr [ObsType]
shufflingDistr updatedOldTracks = shuffleWithRepeats' $
     M.singleton Clutter (poisson clutterLambda)
  <> M.singleton NewTrack (poisson newTrackLambda)
  <> M.mapKeys Track (fmap (\_ -> probMeasurement) updatedOldTracks)

updateWithAssocs :: DelayedSample m => Int -> TrackMap -> [ObsType] -> m ((TrackMap, Int), [DS.Distr (Expr (R 2))])
updateWithAssocs nextTrackID updatedOldTracks assocs = do
  (obsDists, newTrackPVs) <- fmap mconcat . forM assocs $ \k -> case k of
    Clutter -> return ([clutterDistr], [])
    NewTrack -> do
      newTrackPV <- newTrackD
      return ([trackMeasurement newTrackPV], [newTrackPV])
    Track i -> return ([trackMeasurement (updatedOldTracks M.! i)], [])
  coasted <- mapFilterM (\_ _ -> sample (bernoulli ((1 - pd) / (1 - survivalProb * pd)))) notObserved
  return ((mconcat (observed : coasted : zipWith M.singleton [nextTrackID..] newTrackPVs),
           nextTrackID + length newTrackPVs), obsDists)
  where
  (observed, notObserved) = M.partitionWithKey (\i _ -> Track i `elem` assocs) updatedOldTracks

sampleStep :: DelayedSample m => (TrackMap, Int) -> Gen m ((TrackMap, Int), [Expr (R 2)])
sampleStep (allOldTracks, nextTrackID) = do
  updatedOldTracks <- lift (mapM (trackMotion tdiff) allOldTracks)
  assocs <- "assocs" ~~ MP.prim (shufflingDistr updatedOldTracks)
  (newTrackInfo, obsDists) <- lift (updateWithAssocs nextTrackID updatedOldTracks assocs)
  observations <- "observations" ~~ MP.isequence (map MP.dsPrim obsDists)
  return (newTrackInfo, observations)

observeStep :: DelayedInfer m => (TrackMap, Int) -> [R 2] -> m ((TrackMap, Int), [Expr (R 2)])
observeStep (allOldTracks, nextTrackID) observations = do
  updatedOldTracks <- mapM (trackMotion tdiff) allOldTracks
  assocs <- proposeAssocs observations updatedOldTracks
  MP.observing (MP.tr "assocs" assocs <> MP.trList "observations" observations)
    (sampleStep (allOldTracks, nextTrackID))

proposeAssocs :: DelayedInfer m => [R 2] -> TrackMap -> m [ObsType]
proposeAssocs (obs : observations) remainingTracks = do
  newTrack <- newTrackD
  let distrs = M.singleton Clutter clutterDistr <> M.singleton NewTrack (trackMeasurement newTrack)
        <> M.mapKeys Track (fmap trackMeasurement remainingTracks)
  let intensity i = case i of { Clutter -> clutterLambda ; NewTrack -> newTrackLambda ; Track _ -> survivalProb * pd }
  likes <- M.traverseWithKey (\i d -> (\ll -> intensity i * exp ll) <$> DS.score d obs) distrs
  let probs = fmap (/ sum likes) likes
  i <- sample (categoricalM probs)
  factor (- score (categoricalM probs) i) -- proposal correction
  let remainingTracks' = case i of
        Track k -> M.delete k remainingTracks
        _ -> remainingTracks
  (i :) <$> proposeAssocs observations remainingTracks'
proposeAssocs [] remainingTracks = return []

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

runMTTPF :: Int -> ZStream SamplerIO () ([(Int, R 4)], [[(Int, Result (R 4))]], [R 2])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservations -< obs
  returnA -< (M.assocs groundTruth, map M.assocs particles, obs)

runExample :: Int -> IO ()
runExample n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF n)