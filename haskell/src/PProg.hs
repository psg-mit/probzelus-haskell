

module PProg where

import Control.Monad (forever, replicateM_, (>=>), void, replicateM)
import Control.Monad.Trans (lift, MonadIO (liftIO))
import Control.Monad.Bayes.Class (MonadSample, MonadInfer)
import Control.Monad.Bayes.Weighted

import qualified Distributions as D
import Util.Numeric
import qualified Util.MStream as MS
import Util.MStream (MStream)
import Inference hiding (factor)
import qualified Inference as I

import Numeric.Log (Log (Exp))

importance' :: Monad m => [(MStream (Weighted m) p a, Weight)] -> MStream m [(p, Weight)] [(a, Weight)]
importance' xs = MS.merge [ streamWeights w p | (p, w) <- xs]

-- `importance n p` runs importance sampling on `p` with
-- `n` particles.
importance :: Monad m => Int -> MStream (Weighted m) p a -> MStream m [(p, Weight)] [(a, Weight)]
importance n p = importance' (replicate n (p, 1))

particles' :: MonadSample m => [MStream (Weighted m) p a] -> MStream m [p] [a]
particles' xs = do
  xs' <- mapM (MS.lift . runWeighted . MS.step) xs
  xs'' <- I.resample2 xs'
  case MS.allOneSide xs'' of
    Just (Left as) -> do
      MS.yield (map fst as)
      PProg.particles' (map snd as)
    Just (Right bs) ->
      return bs
    _ -> error "Particles not uniform"

particles :: MonadSample m => Int -> MStream (Weighted m) p a -> MStream m [p] [a]
particles n p = I.particles' . map (streamWeights 1) $ replicate n p

-- The lazy version of `forever`.
-- *NOT* equivalent to the version of `forever` in the
-- standard library, which for some reason is insufficiently lazy!
forever' :: Monad m => m () -> m ()
forever' x = x >> forever' x

betaBernoulliModel :: MonadInfer m => MStream m Double ()
betaBernoulliModel = do
  pr <- D.sample (D.beta 1 1)
  MS.yield pr
  forever' $ do
    b <- D.sample (D.bernoulli 0.7)
    D.observe (D.bernoulli pr) b
    MS.yield pr

gaussianGaussianModel :: MonadInfer m => MStream m Double ()
gaussianGaussianModel = do
  mu <- D.sample (D.normal 0 (sqrt 100))
  MS.yield mu
  forever' $ do
    D.observe (D.normal (2 * mu) 1) 3.5
    MS.yield mu

runOneWeightedSample :: Show a => MonadIO m => MStream (Weighted m) a () -> m ()
runOneWeightedSample = void . MS.runStream (\(a, Exp w) -> liftIO (print (a, w))) . streamWeights 1

runParticleFilter :: MonadSample m => MonadIO m => MStream (Weighted m) Double () -> m ()
runParticleFilter = void . MS.runStream (liftIO . print . average) . PProg.particles 1000

runImportanceSampling :: MonadIO m => MStream (Weighted m) Double () -> m ()
runImportanceSampling = void
 . MS.runStream (liftIO . print . weightedAverage . exponentiateWeights)
 . importance 1000