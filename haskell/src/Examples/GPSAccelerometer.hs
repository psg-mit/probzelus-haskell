{-# LANGUAGE Arrows, RecursiveDo #-}

module Examples.GPSAccelerometer where

import Control.Arrow (returnA, (>>>), arr, (<<<))
import Control.Monad.Bayes.Class (normal)
import Control.Monad.Bayes.Sampler (SamplerIO, sampleIO)
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (liftIO)
import qualified Util.ZStream as ZS
import Util.ZStream (ZStream)
import Distributions (sample, bernoulli, factor)
import Inference (zparticles)
import Control.Monad.Fix (MonadFix, mfix)

-- let node f (prob, x) = o where
--   rec init o = sample(prob, Distribution.bernoulli 0.5)
--   and () = factor(prob, if o then (-. 1. fby 0.) else (0. fby -. 0.5))

louis :: ZStream (Weighted SamplerIO) () Bool
louis = proc () -> do
  rec o <- ZS.init (\_ -> Distributions.sample (bernoulli 0.5)) -< ()
      otrue <- const (pure (-1)) `ZS.fby` pure 0 -< ()
      ofalse <- const (pure 0) `ZS.fby` (const (pure (-5000)) `ZS.fby` pure 0) -< ()
      () <- ZS.run -< factor (if o then otrue else ofalse)
  returnA -< o

fracTrue :: [Bool] -> Double
fracTrue bs = let (ntrue, n) = go 0 0 bs in fromIntegral ntrue / fromIntegral n where
  go :: Int -> Int -> [Bool] -> (Int, Int)
  go ntrue n []  = (ntrue, n)
  go ntrue n (x : xs) = go (if x then ntrue + 1 else ntrue) (n + 1) xs

louisResult :: ZStream SamplerIO () Double
louisResult = fracTrue <$> zparticles 1000 louis

runLouis :: IO ()
runLouis = sampleIO $ ZS.runStream (liftIO . print) louisResult

-- let node euler xprime = x where rec
--     x = xinit fby (pre x + step * (pre xprime))

-- let node main () = y where rec
--     y = yinit fby euler (f y)

euler :: MonadFix m => Double -> ZStream m Double Double
euler xinit = proc xprime -> do
  rec xprime' <- ZS.delay undefined -< xprime
      x' <- ZS.delay undefined -< x
      x <- ZS.rightArrow -< (xinit, x' + step * xprime')
  returnA -< x
  where
  step = 1

eulerMain :: MonadFix m => Double -> (Double -> Double) -> ZStream m () Double
eulerMain yinit f = proc () -> do
  rec y <- euler yinit -< f y
  returnA -< y

runEuler :: IO ()
runEuler = ZS.runStream print $ ZS.zconst (pure 1) >>> euler 0

runEulerMain :: IO ()
runEulerMain = ZS.runStream print $ eulerMain 1 (\x -> - x/100)


louis2 :: IO (Double, Double)
louis2 = sampleIO $ mfix f
  where
  f :: (Double, Double) -> SamplerIO (Double, Double)
  f ~(x, y) = do
    q <- normal 0 1
    pure (q, x)

nats :: Monad m => ZStream m () Int
nats = ZS.zcons (\() -> pure 0) (arr (+1) <<< nats)

nats' :: MonadFix m => ZStream m () Int
nats' = proc () -> do
  rec x <- ZS.delay 0 -< x + 1
  returnA -< x