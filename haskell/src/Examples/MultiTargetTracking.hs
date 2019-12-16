{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.MultiTargetTracking where

import Control.Arrow (returnA)
import Control.Monad (replicateM, forM_, forM, filterM, zipWithM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Bayes.Class (MonadSample, categorical, MonadInfer)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Maybe (catMaybes)
import qualified Data.Vector.Storable as V (fromList)

import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Static hiding ((<>))

import Inference (zdsparticles)
import DelayedSampling (DelayedSample, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce, deepForce')
import Distributions (Distr, bernoulli, bernoulli01, poisson, sample, observe, factor,
  randomlyInterleave, randomlyInterleaveLogPDF, shuffleListLogPDF, shuffleWithRepeats)
import MVDistributions (shuffleList)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap)
import Util.Numeric (logFact)

import qualified Metaprob as MP

pickN :: Int -> [a] -> (a, [a])
pickN i xs = let (ys, (z : zs)) = splitAt i xs in (z, ys ++ zs)

data TrackG pv = Track
  { posvel :: pv
  , trackID :: Int
  }
  deriving (Generic, Show)

instance ToJSON pv => ToJSON (TrackG pv)

instance DeepForce pv => DeepForce (TrackG pv) where
  type Forced (TrackG pv) = TrackG (Forced pv)
  deepForce (Track pv i) = Track <$> deepForce pv <*> pure i
  deepConst (Track pv i) = Track (deepConst pv) i

type STrack = TrackG (Expr (R 4))
type Track = TrackG (R 4)
type MarginalTrack = TrackG (Result (R 4))

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

trackMotion :: DelayedSample m => Double -> STrack -> m STrack
trackMotion tdiff track = do
  pv' <- DS.sample (DS.mvNormal (MVMul (Const motionMatrix) (posvel track)) motionCov)
  return $ track { posvel = pv' }
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

shufflingDistr :: [STrack] -> Distr [Int]
shufflingDistr updatedOldTracks = shuffleWithRepeats $
  poisson clutterLambda : poisson newTrackLambda : map (\_ -> probMeasurement) updatedOldTracks

updateWithAssocs :: DelayedSample m => Int -> [STrack] -> [Int] -> m (([STrack], Int), [DS.Distr (Expr (R 2))])
updateWithAssocs nextTrackID updatedOldTracks allAssocs = go [] [] [] allAssocs
  where
  go obsDists newTrackPVs survivedTracks assocs = case assocs of
    [] -> do
      let notObserved = filter (\(i, _) -> not (i `elem` allAssocs)) (zip [2..] updatedOldTracks)
      coasted <- filterM (\_ -> sample (bernoulli ((1 - pd) / (1 - survivalProb * pd)))) (map snd notObserved)
      return ((survivedTracks ++ coasted ++ zipWith Track newTrackPVs [nextTrackID..], nextTrackID + length newTrackPVs), obsDists)
    i : is -> case i of
      0 -> go (clutterDistr : obsDists) newTrackPVs survivedTracks is
      1 -> do
        newTrackPV <- newTrackD
        go (trackMeasurement newTrackPV : obsDists) (newTrackPV : newTrackPVs) survivedTracks is
      _ -> let t = updatedOldTracks !! (i - 2) in
        go (trackMeasurement (posvel t) : obsDists) newTrackPVs (t : survivedTracks) is

sampleStep :: DelayedSample m => ([STrack], Int) -> MP.Gen m (([STrack], Int), [Expr (R 2)])
sampleStep (allOldTracks, nextTrackID) = do
  updatedOldTracks <- lift (mapM (trackMotion tdiff) allOldTracks)
  sampleStep' (updatedOldTracks, nextTrackID)

sampleStep' :: DelayedSample m => ([STrack], Int) -> MP.Gen m (([STrack], Int), [Expr (R 2)])
sampleStep' (updatedOldTracks, nextTrackID) = do
  assocs <- "assocs" MP.~~ MP.prim (shufflingDistr updatedOldTracks)
  (newTrackInfo, obsDists) <- lift (updateWithAssocs nextTrackID updatedOldTracks assocs)
  observations <- "observations" MP.~~ MP.isequence (map MP.dsPrim obsDists)
  return (newTrackInfo, observations)

observeStep :: DelayedInfer m => ([STrack], Int) -> [R 2] -> m (([STrack], Int), [Expr (R 2)])
observeStep (allOldTracks, nextTrackID) observations = do
  updatedOldTracks <- mapM (trackMotion tdiff) allOldTracks
  assocs <- reverse <$> proposeAssocs observations (zip [2..] updatedOldTracks)
  MP.observing (MP.tr "assocs" assocs <> MP.trList "observations" observations)
    (sampleStep' (updatedOldTracks, nextTrackID))

proposeAssocs :: DelayedInfer m => [R 2] -> [(Int, STrack)] -> m [Int]
proposeAssocs (obs : observations) remainingTracks = do
  newTrack <- newTrackD
  let distrs = clutterDistr : map trackMeasurement (newTrack : map (posvel . snd) remainingTracks)
  (clutterLike : newTrackLike : oldTrackLikes) <- mapM (\d -> exp <$> DS.score d obs) distrs
  let likes = clutterLambda * clutterLike : newTrackLambda * newTrackLike : oldTrackLikes
  let probs = map (/ sum likes) likes
  i <- categorical (V.fromList probs)
  factor (- log (probs !! i)) -- proposal correction
  case i of
    0 -> (0 :) <$> proposeAssocs observations remainingTracks
    1 -> (1 :) <$> proposeAssocs observations remainingTracks
    _ -> let ((i', t), remainingTracks') = pickN (i - 2) remainingTracks in
          (i' :) <$> proposeAssocs observations remainingTracks'
proposeAssocs [] remainingTracks = return []

generateGroundTruth :: DelayedSample m => ZStream m () ([STrack], [Expr (R 2)])
generateGroundTruth = ZS.fromStep stepf initState where
  initState = ([], 0)
  stepf state () = do
    (newState@(tracks', _), observations) <- MP.sim (sampleStep state)
    return (newState, (tracks', observations))

processObservations :: DelayedInfer m => ZStream m [R 2] [MarginalTrack]
processObservations = ZS.fromStep stepf initState where
  initState = ([], 0)
  stepf state obs = do
    (newState@(tracks', _), _) <- observeStep state obs
    marginalTracks <- forM tracks' $ \track -> do
      Just pv <- marginal (posvel track)
      return $ track { posvel = pv }
    return (newState, marginalTracks)

runMTTPF :: Int -> ZStream SamplerIO () ([Track], [[MarginalTrack]], [R 2])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservations -< obs
  returnA -< (groundTruth, particles, obs)

runExample :: Int -> IO ()
runExample n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF n)