{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.MultiTargetTracking where

import Control.Arrow hiding ((|||))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Bayes.Class (MonadSample, MonadCond, logCategorical)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)

import Data.Aeson hiding (Result)
import Data.Maybe (catMaybes)

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)

import Numeric.LinearAlgebra.Static

import Inference (zdsparticles, zunheap)
import DelayedSampling (MDistr (..), children, state, State, MarginalT (..), SMarginalT(..), distr, typeOfDSDistr, DelayedInfer)
import qualified SymbolicDistr as DS
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce, deepForce')
import Distributions (poisson, sample, observe, bernoulli, replicateIID, passert, factor)
import MVDistributions (shuffleList)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap, emptyHeap, readRef)

import Numeric.SpecFunctions (logGamma)
import Numeric.Log (Log (Exp), ln)

import qualified Data.Vector.Storable as V (fromList)

import qualified Metaprob as MP


data TrackG pv = Track
  { posvel :: pv
  , startTime :: Double
  , trackID :: Int
  }
  deriving (Generic, Show)

instance ToJSON pv => ToJSON (TrackG pv)

instance DeepForce pv => DeepForce (TrackG pv) where
  type Forced (TrackG pv) = TrackG (Forced pv)
  deepForce (Track pv h t) = Track <$> deepForce pv <*> deepForce h <*> pure t
  deepConst (Track pv h t) = Track (deepConst pv) (deepConst h) t

type STrack = TrackG (Expr (R 6))
type Track = TrackG (R 6)
type MarginalTrack = TrackG (Result (R 6))

tdiff :: Double
tdiff = 1

newTrack :: MonadState Heap m => MonadSample m => Double -> Int -> m STrack
newTrack t id = do
  pv <- DS.sample (DS.mvNormal (Const mu) cov)
  pure (Track pv t id)
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
  motionCov =
    sym $ konst tdiff * ((unSym posCov ||| (konst 0 :: Sq 3))
          ===
         ((konst 0 :: Sq 3) ||| unSym velCov))
  motionMatrix :: Sq 6
  motionMatrix =
      ((eye :: Sq 3) ||| (konst tdiff * eye :: Sq 3))
                    ===
      ((konst 0 :: Sq 3) ||| (eye :: Sq 3))

trackSurvivalMotion :: MonadState Heap m => MonadSample m => Double -> STrack -> m (Maybe STrack)
trackSurvivalMotion tdiff track = do
  survived <- sample (bernoulli (exp (- tdiff * deathRate)))
  if survived
    then Just <$> trackMotion tdiff track
    else pure Nothing
  where
  deathRate = 0.02

trackMeasurement :: STrack -> DS.Distr (Expr (R 3))
trackMeasurement track = DS.mvNormal (MVMul (Const posFromPosVel) (posvel track)) (sym eye)
  where
  posFromPosVel :: L 3 6
  posFromPosVel = (eye :: Sq 3) ||| (konst 0 :: Sq 3)

-- Naive association
associationWithClutter :: forall a b. DS.Distr [Expr b] -> (a -> DS.Distr (Expr b)) -> [a] -> DS.Distr [Expr b]
associationWithClutter cluttersD obsD tracks = DS.Distr is iobs undefined where
  is :: MonadState Heap m => MonadSample m => m [Expr b]
  is = do
    clutter <- DS.sample cluttersD
    observations <- mapM (DS.sample . obsD) tracks
    shuffleList (observations ++ clutter)
  defaultObserve :: DelayedInfer m => [b] -> m ()
  defaultObserve observations = do
    passert (length tracks <= length observations)
    observations' <- shuffleList observations
    let (observations'', clutter) = splitAt (length tracks) observations'
    mapM_ (\(t, o) -> DS.observe (obsD t) o) (zip tracks observations'')
    DS.observe cluttersD clutter
  iobs :: DelayedInfer m => [b] -> m ()
  iobs = if True
    then assocWithClutterCustomProposal cluttersD obsD tracks
    else defaultObserve

assocWithClutterCustomProposal :: DelayedInfer m => DS.Distr [Expr b] -> (a -> DS.Distr (Expr b)) -> [a] -> [b] -> m ()
assocWithClutterCustomProposal cluttersD obsD allTracks allObservations = go allTracks allObservations where
  numTracks = fromIntegral (length (allTracks))
  numObservations = fromIntegral (length (allObservations))
  go [] clutter = do
    let numClutter = fromIntegral (length clutter)
    factor (- (logGamma (numTracks + numClutter + 1) - logGamma (numClutter + 1)))
    DS.observe cluttersD clutter
  go (track : tracks) [] =
    passert False
  go (track : tracks) observations = do
    let obsDistr = obsD track
    lls <- map Exp <$> mapM (DS.score obsDistr) observations
    let totalLL = sum lls
    let adjLLs = map (/ totalLL) lls
    i <- logCategorical (V.fromList adjLLs)
    let (os1, obs : os2) = splitAt i observations
    DS.observe obsDistr obs
    factor (ln (recip (adjLLs !! i))) -- proposal correction
    go tracks (os1 ++ os2)




tracksMeasurement :: [STrack] -> DS.Distr [Expr (R 3)]
tracksMeasurement = associationWithClutter cluttersDistr trackMeasurement
  where
  cluttersDistr = DS.replicateIID (poisson amtClutter) clutterDistr
  amtClutter :: Double
  amtClutter = 1
  clutterDistr :: DS.Distr (Expr (R 3))
  clutterDistr = DS.mvNormal (Const (konst 0)) (10 * (sym eye))

tracksMotion :: MonadState Heap m => MonadSample m => Double -> Double -> [STrack] -> Int -> m ([STrack], Int)
tracksMotion t tdiff tracks numTracks = do
  liveTracks <- catMaybes <$> mapM (trackSurvivalMotion tdiff) tracks
  numNewTracks <- sample (poisson (birthRate * tdiff))
  newTracks <- sequence [ newTrack t (numTracks + i) | i <- [1..numNewTracks] ]
  pure (liveTracks ++ newTracks, numTracks + numNewTracks)
  where
  birthRate = 0.1

-- Note how extraneous state about the number of tracks and the time is kept hidden
zstepGen :: MonadState Heap m => MonadSample m => Bool -> ZStream (MP.Gen m) () ([STrack], [Expr (R 3)])
zstepGen delay = ZS.fromStep stepf initState
  where
  force = if delay then id else (>>= deepForce')
  initState :: (Double, [TrackG pv], Int)
  initState = (0, [], 0)
  stepf (t, tracks, numTracks) () = do
    (tracks', newNumTracks) <- lift (force (tracksMotion t tdiff tracks numTracks))
    observations <- MP.at "obs" (MP.dsPrim (tracksMeasurement tracks'))
    return ((t + tdiff, tracks', newNumTracks), (tracks', observations))

processObservationsStream :: DelayedInfer m => Bool -> ZStream m [R 3] [MarginalTrack]
processObservationsStream delay = proc observations -> do
  (tracks', _) <- MP.zobserving (zstepGen delay) -< ((), MP.tr "obs" observations)
  ZS.run -< mapM trackf tracks'
  where
  trackf :: DelayedInfer m => STrack -> m MarginalTrack
  trackf track = do
    Just pv <- marginal (posvel track)
    pure $ track { posvel = pv }

generateGroundTruth :: MonadState Heap m => MonadSample m => ZStream m () ([STrack], [Expr (R 3)])
generateGroundTruth = ZS.liftM MP.sim (zstepGen True)

runMTTPF :: Bool -> Int -> ZStream SamplerIO () ([Track], [[MarginalTrack]], [R 3])
runMTTPF delay numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles (processObservationsStream delay) -< obs
  returnA -< (groundTruth, particles, obs)


runExample :: Bool -> Int -> IO ()
runExample delay n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF delay n)