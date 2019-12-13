{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.MultiTargetTracking where

import Control.Arrow (returnA)
import Control.Monad (replicateM, forM_, forM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Bayes.Class (MonadSample, categorical, MonadInfer)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Maybe (catMaybes)
import qualified Data.Vector.Storable as V (fromList)

import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Static

import Inference (zdsparticles)
import DelayedSampling (DelayedSample, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce, deepForce')
import Distributions (Distr, bernoulli, poisson, sample, observe, factor, randomlyInterleave)
import MVDistributions (shuffleList)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap)
import Util.Numeric (logFact)

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

type STrack = TrackG (Expr (R 6))
type Track = TrackG (R 6)
type MarginalTrack = TrackG (Result (R 6))

tdiff, birthRate, deathRate, clutterLambda, newTrackLambda :: Double
tdiff = 1
birthRate = 0.1
deathRate = 0.02
clutterLambda = 3
newTrackLambda = birthRate * tdiff

survivalDist :: Distr Bool
survivalDist = bernoulli (exp (- tdiff * deathRate))

clutterDistr :: DS.Distr (Expr (R 3))
clutterDistr = DS.mvNormal (Const (0 :: R 3)) (10 * sym eye)

newTrackD :: DelayedSample m => Int -> m STrack
newTrackD id = do
  pv <- DS.sample (DS.mvNormal (Const mu) cov)
  return (Track pv id)
  where
  mu = konst 0 :: R 6
  cov = sym (((5 * eye :: Sq 3) ||| (0 :: Sq 3))
            ===
            ((0 :: Sq 3) ||| (0.1 * eye :: Sq 3)))

trackMotion :: DelayedSample m => Double -> STrack -> m STrack
trackMotion tdiff track = do
  pv' <- DS.sample (DS.mvNormal (MVMul (Const motionMatrix) (posvel track)) motionCov)
  return $ track { posvel = pv' }
  where
  motionMatrix :: Sq 6
  motionMatrix = (eye :: Sq 6) + konst tdiff *
    (((0 :: Sq 3) ||| (eye :: Sq 3))
    ===
    ((- 1 / 100 * eye :: Sq 3) ||| (-0.1 * eye :: Sq 3)))
  motionCov :: Sym 6
  motionCov = sym $ konst tdiff * (((0.01 * eye :: Sq 3) ||| (0 :: Sq 3))
                                  ===
                                  ((0 :: Sq 3) ||| (0.1 * eye :: Sq 3)))

trackMeasurement :: STrack -> DS.Distr (Expr (R 3))
trackMeasurement track = DS.mvNormal (MVMul (Const posFromPosVel) (posvel track)) (sym eye)
  where
  posFromPosVel :: L 3 6
  posFromPosVel = (eye :: Sq 3) ||| (0 :: Sq 3)

sampleStep :: DelayedSample m => Int -> [STrack] -> m ([STrack], [Expr (R 3)], Int)
sampleStep nextTrackID allOldTracks = do
    survivedTracks <- catMaybes <$> (forM allOldTracks $ \t -> do
      survived <- sample survivalDist
      if survived
        then Just <$> trackMotion tdiff t
        else return Nothing)
    numNewTracks <- sample (poisson newTrackLambda)
    newTracks <- forM (take numNewTracks (iterate (+1) nextTrackID)) newTrackD
    shuffledSurvivedTracks <- shuffleList survivedTracks
    let tracks = newTracks : map (:[]) shuffledSurvivedTracks
    nclutter <- sample (poisson clutterLambda)
    let obsDists = replicate nclutter clutterDistr : map (map trackMeasurement) tracks
    shuffledObsDists <- randomlyInterleave obsDists
    observations <- mapM DS.sample shuffledObsDists
    return (survivedTracks ++ newTracks, observations, nextTrackID + numNewTracks)


observeStep :: DelayedInfer m => Int -> [STrack] -> [R 3] -> m ([STrack], Int)
observeStep nextTrackID allOldTracks allObservations = do
    updatedOldTracks <- mapM (trackMotion tdiff) allOldTracks
    go allObservations updatedOldTracks [] [] 0 nextTrackID
  where
  go (obs : observations) oldTracks newTracks survivedTracks nclutter tid = do
    newTrack <- newTrackD tid
    let distrs = clutterDistr : map trackMeasurement (newTrack : oldTracks)
    (clutterLike : newTrackLike : oldTrackLikes) <- mapM (\d -> exp <$> DS.score d obs) distrs
    let likes = clutterLambda * clutterLike : newTrackLambda * newTrackLike : oldTrackLikes
    let probs = map (/ sum likes) likes
    i <- categorical (V.fromList probs)
    DS.observe (distrs !! i) obs
    factor (- log (probs !! i)) -- proposal correction
    case i of
      0 -> go observations oldTracks newTracks survivedTracks (nclutter + 1) tid
      1 -> go observations oldTracks (newTrack : newTracks) survivedTracks nclutter (tid + 1)
      _ -> let (t, oldTracks') = pickN (i - 2) oldTracks in
           go observations oldTracks' newTracks (t : survivedTracks) nclutter tid
  go [] oldTracks newTracks survivedTracks nclutter tid = do
    forM_ oldTracks $ \_ -> observe survivalDist False
    forM_ survivedTracks $ \_ -> observe survivalDist True
    observe (poisson clutterLambda) nclutter
    observe (poisson newTrackLambda) (length newTracks)
    factor (- (logFact (length allObservations + nclutter) - logFact nclutter))
    return (survivedTracks ++ newTracks, tid)

generateGroundTruth :: DelayedSample m => ZStream m () ([STrack], [Expr (R 3)])
generateGroundTruth = ZS.fromStep stepf initState
  where
  initState :: ([TrackG pv], Int)
  initState = ([], 0)
  stepf (tracks, numTracks) () = do
    (tracks', observations, numTracks') <- sampleStep numTracks tracks
    return ((tracks', numTracks'), (tracks', observations))

processObservations :: DelayedInfer m => ZStream m [R 3] [MarginalTrack]
processObservations = ZS.fromStep stepf initState where
  initState = ([], 0)
  stepf (tracks, nextTrackID) obs = do
    newState@(tracks', nextTrackID') <- observeStep nextTrackID tracks obs
    marginalTracks <- forM tracks' $ \track -> do
      Just pv <- marginal (posvel track)
      return $ track { posvel = pv }
    return (newState, marginalTracks)

runMTTPF :: Int -> ZStream SamplerIO () ([Track], [[MarginalTrack]], [R 3])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservations -< obs
  returnA -< (groundTruth, particles, obs)

runExample :: Bool -> Int -> IO ()
runExample delay n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF n)