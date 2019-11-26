import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Bayes.Class (MonadSample, MonadInfer, MonadCond)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler (sampleIO)
import qualified Data.Random.Distribution.Categorical as Cat
import System.Random (getStdGen)

import qualified Distributions as D
import Util.Numeric (average)
import PProg
import Util.MStream as MS
import qualified Inference as I

import Numeric.Log (Log (Exp, ln))

data MotionType = Stationary | Walking | Running
  deriving (Eq, Show, Ord, Enum)

data Walker = Walker
  { position :: (Double, Double)
  , velocity :: (Double, Double)
  , motionType :: MotionType
  }

posNoise :: Double
posNoise = 0.01

coast :: MonadSample m => Double -> Walker -> m Walker
coast dt w = do
  x'' <- D.sample (D.normal x' stdDev)
  y'' <- D.sample (D.normal y' stdDev)
  return $ w { position = (x'', y'')}
  where
  stdDev = sqrt dt * posNoise
  (x', y') = both2 (+) (position w) (both (* dt) (velocity w))
  both2 f (x, y) (x', y') = (f x x', f y y')
  both f (x, y) = (f x, f y)

-- half lives of changing the motion type, in seconds
-- sometimes, we change to ourselves, to change direction
motionTypeTransition :: MotionType -> [(Log Double, MotionType)]
motionTypeTransition mt = case mt of
  Stationary -> [(30, Walking), (60 * 5, Running)]
  Walking -> [(1, Walking), (10, Stationary), (60, Running)]
  Running -> [(0.5, Running), (2, Walking)]

initVelocity :: MonadSample m => MotionType -> m (Double, Double)
initVelocity mt = case mt of
  Stationary -> pure (0, 0)
  Walking -> do
    speed <- D.sample (D.uniform 0 2)
    speedAtRandomDirection speed
  Running -> do
    speed <- D.sample (D.uniform 2 7)
    speedAtRandomDirection speed
  where
  speedAtRandomDirection speed = do
    angle <- D.sample (D.uniform 0 (2 * pi))
    return (speed * cos angle, speed * sin angle)

-- default units are seconds
motion :: MonadSample m => Double -> Walker -> m Walker
motion dt w = do
  -- tTransition <- exponential (sum (map (recip . fst) transLam))
  tTransition <- D.exponential (recip (sum (map (exp . ln . snd) transLam)))
  if tTransition > dt
    then coast dt w
    else do
      w' <- coast tTransition w
      mt <- I.labeledLogCategorical transLam
      vel <- initVelocity mt
      motion (dt - tTransition) (w' { velocity = vel, motionType = mt })
  where
  transLam = [ (mt, log 2 / p) | (p, mt) <- motionTypeTransition (motionType w) ]

positionStdDev :: Double
positionStdDev = 10

walkerMeasure :: MonadCond m => Walker -> (Double, Double) -> m ()
walkerMeasure w (mx, my) = do
  D.observe (D.normal x (positionStdDev^2)) mx
  D.observe (D.normal y (positionStdDev^2)) my
  where
  (x, y) = position w

walkerGenMeasurement :: MonadSample m => Walker -> m (Double, Double)
walkerGenMeasurement w = do
  mx <- D.sample (D.normal x positionStdDev)
  my <- D.sample (D.normal y positionStdDev)
  return (mx, my)
  where
  (x, y) = position w

walkerStep :: MonadInfer m => Double -> (Double, Double) -> Walker -> m Walker
walkerStep dt measuredPosition w = do
  w' <- motion dt w
  walkerMeasure w' measuredPosition
  return w'

walkerInit :: MonadSample m => m Walker
walkerInit = do
  mt <- I.labeledLogCategorical [(Stationary, 0.7), (Walking, 0.25), (Running, 0.05)]
  vel <- initVelocity mt
  return (Walker { position = (0, 0), velocity = vel, motionType = mt })

generateWalker :: MonadSample m => Int -> m [(Double, (Double, Double))]
generateWalker n = do
  w <- walkerInit
  go n w
  where
  go 0 w = return []
  go k w = do
    w' <- motion dt w
    mpos <- walkerGenMeasurement w'
    ((dt, mpos) :) <$> go (k - 1) w'
  dt = 10

walkerSimulate :: MonadInfer m => [(Double, (Double, Double))] -> MStream m Walker Walker
walkerSimulate measurements = do
  w <- MS.lift $ walkerInit
  go w measurements
  where
  go w [] = return w
  go w ((dt, measPos) : xs) = do
    w' <- MS.lift $ walkerStep dt measPos w
    MS.yield w'
    go w' xs

frequencies :: (Eq a, Ord a) => [a] -> [(Double, a)]
frequencies = Cat.toList . Cat.fromObservations

runOurFilter :: MonadSample m => MonadIO m => MStream (Weighted m) Walker a -> m ()
runOurFilter = void . MS.runStream (liftIO . print . summarize) . particles 1000
  where
  summarize :: [Walker] -> ([(Double, MotionType)], (Double, Double), (Double, Double))
  summarize ws =
    (frequencies (map motionType ws), averagingBoth position, averagingBoth velocity)
    where
    averagingBoth f = (averaging (fst . f), averaging (snd . f))
    averaging f = average (map f ws)

main :: IO ()
main = sampleIO $ do
  measurements <- generateWalker 1000
  runOurFilter $ walkerSimulate measurements