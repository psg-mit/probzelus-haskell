
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE Arrows, RecursiveDo #-}

module Inference where

import Control.Arrow
import Control.Monad (replicateM)
import Control.Monad.Fix
import Control.Monad.State (StateT (..), runStateT, lift)

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

import Data.Functor.Identity (Identity)
import Data.Random hiding (normal, uniform, gamma, shuffle)
import qualified Data.Random.Distribution.Categorical as Cat

import qualified Data.Vector as V

import Numeric.Log (Log (Exp))
import Util.Numeric (exponentiateWeights)
import Util.MStream (MStream)
import qualified Util.MStream as MS
import Util.ZStream as ZS
import Util.Ref

type Weight = Log Double

instance MonadSample m => MonadSample (MStream m p) where
  random = MS.lift random

  uniform a b = MS.lift $ Control.Monad.Bayes.Class.uniform a b
  normal m s = MS.lift $ normal m s
  gamma shape scale = MS.lift $ gamma shape scale
  beta a b = MS.lift $ beta a b

  bernoulli p = MS.lift $ bernoulli p
  categorical ps = MS.lift $ categorical ps
  geometric p = MS.lift $ geometric p

  shuffle = MS.lift . shuffle


instance MonadCond m => MonadCond (MStream m p) where
  score = MS.lift . score

-- lift :: Functor m => m a -> Weighted m a
-- lift x = lift

catFromList' = Cat.fromList . map (\(x, w) -> (w, x))

resample :: [(a, Weight)] -> RVar [a]
resample xs = replicateM (length xs) . sample . catFromList' $ exponentiateWeights xs

-- weights must be normalized!
labeledLogCategorical :: MonadSample m => [(a, Weight)] -> m a
labeledLogCategorical xs = fmap (fst . (xs !!)) $ logCategorical (V.fromList (map snd xs))

resample2 :: MonadSample m => [(a, Weight)] -> m [a]
resample2 xs = replicateM (length xs) $ labeledLogCategorical (normalize xs)

normalize :: [(a, Weight)] -> [(a, Weight)]
normalize xs = map (\(x, w) -> (x, w / totalMass)) xs
  where
  totalMass = sum (map snd xs)

importanceResampling :: RVar (a, Weight) -> Int -> RVar a
importanceResampling x numSamples = do
  xs <- replicateM numSamples x
  sample (catFromList' (exponentiateWeights xs))

particles' :: MonadSample m => [MStream m (p, Weight) (a, Weight)] -> MStream m [p] [a]
particles' xs = do
  as <- MS.mergeWith (\ys -> resample2 [ ((y, s), w) | ((y, w), s) <- ys]) xs
  MS.lift (resample2 as)

streamWeights :: Monad m => Weight -> MStream (Weighted m) p a -> MStream m (p, Weight) (a, Weight)
streamWeights w (MS.Ret x) = MS.Ret (x, w)
streamWeights w (MS.Yield y x) = MS.Yield (y, w) (streamWeights w x)
streamWeights w (MS.Act x) = do
  (x', w') <- MS.lift $ runWeighted x
  streamWeights (w * w') x'

particles :: MonadSample m => Int -> MStream (Weighted m) p a -> MStream m [p] [a]
particles numParticles = particles' . replicate numParticles . streamWeights 1


-- Zelus streams

-- These four definitions comprise inference.

zunWeight' :: Functor m => Weight -> ZStream (Weighted m) a b -> ZStream m a (b, Weight)
zunWeight' w (ZStream f) = ZStream $ \a ->
    (\((f', b), w') -> let w'' = w * w' in (zunWeight' w'' f', (b, w''))) <$> runWeighted (f a)

-- Get one streaming importance sample
zunWeight :: Functor m => ZStream (Weighted m) a b -> ZStream m a (b, Weight)
zunWeight = zunWeight' 1

zparticles' :: MonadSample m => [ZStream (Weighted m) a b] -> ZStream m a [b]
zparticles' = fromStep $ \fs a -> do
  res <- sequence [ runWeighted (f a) | ZStream f <- fs ]
  unzip <$> resample2 res

-- Streaming particle filter
zparticles :: MonadSample m => Int -> ZStream (Weighted m) a b -> ZStream m a [b]
zparticles numParticles = zparticles' . replicate numParticles

zmergeState :: Monad m => s -> ZStream (StateT s m) a b -> ZStream m a b
zmergeState initState (ZStream f) = fromStep step (f, initState) where
  step (g, s) a = do
    ((ZStream g', b), s') <- runStateT (g a) s
    pure ((g', s'), b)

zunheap :: Monad m => ZStream (StateT Heap m) a b -> ZStream m a b
zunheap = zmergeState emptyHeap

zdsparticles :: MonadSample m => Int -> ZStream (StateT Heap (Weighted m)) a b -> ZStream m a [b]
zdsparticles numParticles = zparticles numParticles . zunheap

commuteWeightedState :: Monad m => Weighted (StateT s m) a -> StateT s (Weighted m) a
commuteWeightedState x = StateT $ \s -> withWeight $ do
  ((y, w), s') <- runStateT (runWeighted x) s
  pure ((y, s'), w)

zdsparticles' :: MonadSample m => Int -> ZStream (Weighted (StateT Heap m)) a b -> ZStream m a [b]
zdsparticles' numParticles = zdsparticles numParticles . ZS.liftM commuteWeightedState

zunheap' :: Monad m => ZStream (Weighted (StateT Heap m)) a b -> ZStream (Weighted m) a b
zunheap' = zunheap . ZS.liftM commuteWeightedState

-- The following are useful in defining semantics for a stream of results.

-- think t = RVarT
zunRVar' :: (Applicative m, Monad (t m)) => t m (ZStream (t m) a b) -> ZStream m a (t m b)
zunRVar' = fromStep $ \zs a ->
  let res = do {ZStream f <- zs; f a} in pure (fst <$> res, snd <$> res)

-- think t = RVarT
-- "Decorrelation of stream results"
zunRVar :: (Applicative m, Monad (t m)) => ZStream (t m) a b -> ZStream m a (t m b)
zunRVar = zunRVar' . pure

-- A weak attempt at normalization by using importance sampling
zwrInterp :: ZStream (Weighted RVar) a b -> ZStream Identity a (Int -> RVar b)
zwrInterp = fmap importanceResampling . zunRVar . zunWeight

rw :: MonadSample m => Double -> ZStream m () Double
rw x = z where
  z = zcons (\() -> normal x 1) (z >>> zconstM g)
  g :: MonadSample m => Double -> m Double
  g x = normal x 1

rw' :: MonadSample m => MonadFix m => ZStream m () Double
rw' = proc () -> do
  rec x' <- delay 0 -< x
      x <- zconstM (\z -> normal z 1) -< x'
  returnA -< x

rw2 :: MonadSample m => MonadFix m => ZStream m () Double
rw2 = proc () -> do
  rec x' <- delay 0 -< x
      x <- zconstM (\z -> normal z 1) -< x'
      y <- zconstM (\z -> normal z 1) -< x
  returnA -< y