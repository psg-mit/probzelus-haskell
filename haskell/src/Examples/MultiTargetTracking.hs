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
import DelayedSampling (DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce, deepForce')
import Distributions (Distr, poisson, sample, observe, bernoulli, replicateIID, factor)
import MVDistributions (shuffleList)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap)

import Numeric.SpecFunctions (logGamma)

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
clutterLambda = 1
newTrackLambda = birthRate * tdiff

survivalDist :: Distr Bool
survivalDist = bernoulli (exp (- tdiff * deathRate))

clutterDistr :: DS.Distr (Expr (R 3))
clutterDistr = DS.mvNormal (Const (konst 0)) (10 * (sym eye))

newTrackD :: MonadState Heap m => MonadSample m => Int -> m STrack
newTrackD id = do
  pv <- DS.sample (DS.mvNormal (Const mu) cov)
  pure (Track pv id)
  where
  mu = konst 0 :: R 6
  cov = posVelCovBlocks (sym eye :: Sym 3) (0.001 * sym eye :: Sym 3)
  posVelCovBlocks :: Sym 3 -> Sym 3 -> Sym 6
  posVelCovBlocks pcov vcov = sym $
    ((unSym pcov ||| (konst 0 :: Sq 3))
          ===
    ((konst 0 :: Sq 3) ||| unSym vcov))

trackMotion :: MonadState Heap m => MonadSample m => Double -> STrack -> m STrack
trackMotion tdiff track = do
  pv' <- DS.sample (DS.mvNormal (MVMul (Const motionMatrix) (posvel track)) motionCov)
  pure $ track { posvel = pv' }
  where
  posCov = 0.01 * sym eye
  velCov = 0.1 * sym eye
  motionCov :: Sym 6
  motionCov = sym $ konst tdiff * ((unSym posCov ||| (konst 0 :: Sq 3))
                    ===
                    ((konst 0 :: Sq 3) ||| unSym velCov))
  motionMatrix :: Sq 6
  motionMatrix = ((eye :: Sq 3) ||| (konst tdiff * eye :: Sq 3))
                 ===
                 ((konst 0 :: Sq 3) ||| (eye :: Sq 3))

trackMeasurement :: STrack -> DS.Distr (Expr (R 3))
trackMeasurement track = DS.mvNormal (MVMul (Const posFromPosVel) (posvel track)) (sym eye)
  where
  posFromPosVel :: L 3 6
  posFromPosVel = (eye :: Sq 3) ||| (konst 0 :: Sq 3)

pickN :: Int -> [a] -> (a, [a])
pickN i xs = let (ys, (z : zs)) = splitAt i xs in (z, ys ++ zs)

allSample :: MonadState Heap m => MonadSample m =>
  Int -> [STrack] -> m ([STrack], [Expr (R 3)], Int)
allSample nextTrackID allOldTracks = do
    survivedTracks <- catMaybes <$> (forM allOldTracks $ \t -> do
      survived <- sample survivalDist
      if survived
        then Just <$> trackMotion tdiff t
        else return Nothing)
    nclutter <- sample (poisson clutterLambda)
    clutter <- replicateM nclutter (DS.sample clutterDistr)
    numNewTracks <- sample (poisson newTrackLambda)
    newTracks <- forM (take numNewTracks (iterate (+1) nextTrackID)) newTrackD
    let tracks = survivedTracks ++ newTracks
    observations <- mapM (DS.sample . trackMeasurement) tracks
    allObservations <- shuffleList (observations ++ clutter)
    return (tracks, allObservations, nextTrackID + numNewTracks)


allCustomProposal :: DelayedInfer m => Int -> [STrack] -> [R 3] -> m ([STrack], Int)
allCustomProposal nextTrackID allOldTracks allObservations = do
    updatedOldTracks <- mapM (trackMotion tdiff) allOldTracks
    go allObservations updatedOldTracks [] [] 0 nextTrackID
  where
  logFact n = logGamma (fromIntegral (n + 1))

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

generateGroundTruth :: MonadState Heap m => MonadSample m => ZStream m () ([STrack], [Expr (R 3)])
generateGroundTruth = ZS.fromStep stepf initState
  where
  initState :: ([TrackG pv], Int)
  initState = ([], 0)
  stepf (tracks, numTracks) () = do
    (tracks', observations, numTracks') <- allSample numTracks tracks
    return ((tracks', numTracks'), (tracks', observations))

processObservationsStream :: DelayedInfer m => ZStream m [R 3] [MarginalTrack]
processObservationsStream = ZS.fromStep stepf initState where
  initState = ([], 0)
  stepf (tracks, nextTrackID) obs = do
    newState@(tracks', nextTrackID') <- allCustomProposal nextTrackID tracks obs
    marginalTracks <- forM tracks' $ \track -> do
      Just pv <- marginal (posvel track)
      pure $ track { posvel = pv }
    return (newState, marginalTracks)

runMTTPF :: Int -> ZStream SamplerIO () ([Track], [[MarginalTrack]], [R 3])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservationsStream -< obs
  returnA -< (groundTruth, particles, obs)

runExample :: Bool -> Int -> IO ()
runExample delay n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF n)