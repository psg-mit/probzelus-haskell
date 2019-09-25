{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.MultiCamera where

import Control.Arrow hiding ((|||))
import Control.Monad (replicateM, forM)
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
import DSProg (DeepForce (..), Result (..), Expr' (..), Expr, marginal, zdeepForce)
import Distributions (normal, poisson, sample, observe, bernoulli, replicateIID, passert, factor, uniformIntRange)
import MVDistributions (shuffleList, mvNormal, uniformNSphere)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS
import Util.Ref (MonadState, Heap, emptyHeap, readRef)

import Numeric.SpecFunctions (logGamma)
import Numeric.Log (Log (Exp), ln)

import qualified Data.Vector.Storable as V (fromList)

import qualified Metaprob as MP

numCameras :: Int
numCameras = 2

numPoses :: Int
numPoses = 10

type CameraID = Int
type Pose = Int

data TrackG pv = Track
  { posWH :: pv
  , startTime :: Double
  , trackID :: Int
  , identity :: Int
  , camera :: CameraID
  }
  deriving (Generic, Show)

instance ToJSON pv => ToJSON (TrackG pv)

instance DeepForce pv => DeepForce (TrackG pv) where
  type Forced (TrackG pv) = TrackG (Forced pv)
  deepForce (Track pv h t id c) = Track <$> deepForce pv <*> deepForce h <*> pure t <*> pure id <*> pure c
  deepConst (Track pv h t id c) = Track (deepConst pv) (deepConst h) t id c

type STrack = TrackG (Expr (R 4))
type Track = TrackG (R 4)
type MarginalTrack = TrackG (Result (R 4))

type Appearance = Expr (R 10)

tdiff :: Double
tdiff = 1

posWHCovBlocks :: Sym 2 -> Sym 2 -> Sym 4
posWHCovBlocks pcov whcov = sym $
  ((unSym pcov ||| (konst 0 :: Sq 2))
        ===
  ((konst 0 :: Sq 2) ||| unSym whcov))

newTrack'' :: MonadState Heap m => MonadSample m => Double -> Int -> Int -> Int -> m STrack
newTrack'' t tid id c = do
  pv <- DS.sample (DS.mvNormal (Const mu) cov)
  pure (Track pv t tid id c)
  where
  mu = (konst 0 :: R 2) # (konst 1 :: R 2)
  cov = posWHCovBlocks (sym eye :: Sym 2) (0.2 * sym eye :: Sym 2)

newTrack :: MonadState Heap m => MonadSample m => [Appearance] -> Double -> Int -> m (STrack, [Appearance])
newTrack appearances t tid = do
  camera <- sample (uniformIntRange numCameras)
  newAppearance <- sample (bernoulli (1 / (fromIntegral (length appearances) + 1)))
  (appearanceID, appearances') <- if newAppearance
    then do
         app <- DS.sample (DS.mvNormal (Const 0) (sym eye))
         pure (length appearances, appearances ++ [app])
    else do
         appID <- sample (uniformIntRange (length appearances))
         pure (appID, appearances)
  newTrack <- newTrack'' t tid appearanceID camera
  pure (newTrack, appearances')


trackMotion :: MonadState Heap m => MonadSample m => Double -> STrack -> m STrack
trackMotion tdiff track = do
  pv' <- DS.sample (DS.mvNormal (MVMul (Const motionMatrix) (posWH track)) motionCov)
  pure $ track { posWH = pv' }
  where
  motionMatrix = eye :: Sq 4
  motionCov :: Sym 4
  motionCov = sym (konst tdiff) * posWHCovBlocks (sym eye) (sym (konst 0.1 * eye))

trackSurvivalMotion :: MonadState Heap m => MonadSample m => Double -> STrack -> m (Maybe STrack)
trackSurvivalMotion tdiff track = do
  survived <- sample (bernoulli (exp (- tdiff * deathRate)))
  if survived
    then Just <$> trackMotion tdiff track
    else pure Nothing
  where
  deathRate = 0.02

trackMeasurement :: [Appearance] -> STrack -> DS.Distr (Expr (R 4), (R 10, Expr (R 1)))
trackMeasurement appearances track =
  DS.indep (DS.mvNormal (posWH track) (sym eye)) $
  DS.bind uniformNSphere $ \pose ->
    DS.mvNormal (MVMul (Const (row pose)) (appearances !! identity track)) (sym eye)

-- Naive association
associationWithClutter :: forall a b. DeepForce b => DS.Distr [b] -> (a -> DS.Distr b) -> [a] -> DS.Distr [b]
associationWithClutter cluttersD obsD tracks = DS.Distr is iobs undefined where
  is :: MonadState Heap m => MonadSample m => m [b]
  is = do
    clutter <- DS.sample cluttersD
    observations <- mapM (DS.sample . obsD) tracks
    shuffleList (observations ++ clutter)
  defaultObserve :: DelayedInfer m => [Forced b] -> m ()
  defaultObserve observations = do
    passert (length tracks <= length observations)
    observations' <- shuffleList observations
    let (observations'', clutter) = splitAt (length tracks) observations'
    mapM_ (\(t, o) -> DS.observe (obsD t) o) (zip tracks observations'')
    DS.observe cluttersD clutter
  iobs :: DelayedInfer m => [Forced b] -> m ()
  iobs = if True
    then assocWithClutterCustomProposal cluttersD obsD tracks
    else defaultObserve

assocWithClutterCustomProposal :: DelayedInfer m => DeepForce b => DS.Distr [b] -> (a -> DS.Distr b) -> [a] -> [Forced b] -> m ()
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


tracksMeasurement :: [Appearance] -> [STrack] -> DS.Distr [(Expr (R 4), (R 10, Expr (R 1)))]
tracksMeasurement appearances = associationWithClutter cluttersDistr (trackMeasurement appearances)
  where
  cluttersDistr = DS.replicateIID (poisson amtClutter) clutterDistr
  amtClutter :: Double
  amtClutter = 1
  clutterDistr :: DS.Distr (Expr (R 4), (R 10, Expr (R 1)))
  clutterDistr = DS.indep (DS.mvNormal (Const ((konst 0 :: R 2) # (konst 1 :: R 2))) (sym eye))
    $ DS.bind (mvNormal 0 (sym eye)) (\_ -> DS.mvNormal (Const 0) (sym eye))

-- NOTE: Currently, it is possible that we will generate a new track
-- with the same identity and camera as an existing track.
-- Should consider how to change the model so that this doesn't happen.
tracksMotion :: MonadState Heap m => MonadSample m => Double -> Double -> [STrack] -> Int -> [Appearance] -> m ([STrack], Int, [Appearance])
tracksMotion t tdiff tracks numTracks appearances = do
  liveTracks <- catMaybes <$> mapM (trackSurvivalMotion tdiff) tracks
  numNewTracks <- sample (poisson (birthRate * tdiff))
  (newTracks, appearances') <- generateNewTracks appearances (numTracks + 1) numNewTracks
  pure (liveTracks ++ newTracks, numTracks + numNewTracks, appearances')
  where
  generateNewTracks apps nt 0 = pure ([], apps)
  generateNewTracks apps nt n = do
    (newT, apps') <- newTrack apps t nt
    (\(x, y) -> (newT : x, y)) <$> generateNewTracks apps' (nt + 1) (n - 1)
  birthRate = 0.1

-- Note how extraneous state about the number of tracks and the time is kept hidden
zstepGen :: MonadState Heap m => MonadSample m => ZStream (MP.Gen m) () ([STrack], [(CameraID, R 10, Expr (R 4), Expr (R 1))])
zstepGen = ZS.fromStep stepf initState
  where
  initState :: (Double, [TrackG pv], Int, [Appearance])
  initState = (0, [], 0, [])
  stepf (t, tracks, numTracks, appearances) () = do
    (tracks', newNumTracks, appearances') <- lift (tracksMotion t tdiff tracks numTracks appearances)
    observations <- forM [0 .. numCameras - 1] $ \camID -> do
      obs <- MP.at ("obs." ++ show camID) (MP.dsPrim (tracksMeasurement appearances' (filter (\t -> camera t == camID) tracks')))
      return [ (camID, pose, pvwh, obsAppear) | (pvwh, (pose, obsAppear)) <- obs ]
    return ((t + tdiff, tracks', newNumTracks, appearances'), (tracks', concat observations))

processObservationsStream :: DelayedInfer m => ZStream m [(CameraID, R 10, R 4, R 1)] [MarginalTrack]
processObservationsStream = proc observations -> do
  (tracks', _) <- MP.zobserving zstepGen -< ((), MP.tr "obs" observations)
  ZS.run -< mapM trackf tracks'
  where
  trackf :: DelayedInfer m => STrack -> m MarginalTrack
  trackf track = do
    Just pv <- marginal (posWH track)
    pure $ track { posWH = pv }

generateGroundTruth :: MonadState Heap m => MonadSample m => ZStream m () ([STrack], [(CameraID, R 10, Expr (R 4), Expr (R 1))])
generateGroundTruth = ZS.liftM MP.sim zstepGen

runMTTPF :: Int -> ZStream SamplerIO () ([Track], [[MarginalTrack]], [(CameraID, R 10, R 4, R 1)])
runMTTPF numParticles = proc () -> do
  (groundTruth, obs) <- zdeepForce generateGroundTruth -< ()
  particles <- zdsparticles numParticles processObservationsStream -< obs
  returnA -< (groundTruth, particles, obs)

runExample :: IO ()
runExample = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (runMTTPF 100)