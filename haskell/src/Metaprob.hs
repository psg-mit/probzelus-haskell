{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Metaprob where

import Control.Arrow (second)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted
import Control.Monad.RWS.CPS

import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as M

import Distributions (Distr)
import qualified Distributions as D

import Numeric.Log (Log (Exp))

import Data.Random (RVar)
import Util.RandomUtil
import Util.Ref (Heap)
import Util.ZStream (ZStream)
import qualified Util.ZStream as ZS

import qualified SymbolicDistr as DS
import DelayedSampling (DelayedInfer)
import DSProg (Forced (..))

type Trace = Map String Dynamic

newtype Weight = Weight (Log Double)
  deriving (Eq, Show, Ord)

deriving instance Num Weight

instance Semigroup Weight where
  (<>) = (*)

instance Monoid Weight where
  mempty = 1

type Inf m = RWST Trace (Trace, Weight) () m

data Gen m a = Gen
  { sim :: m a
  , makeConstrainedGen :: Inf (Gen m) a
  }

deriving instance Functor m => Functor (Gen m)

instance Monad m => Applicative (Gen m) where
  pure x = Gen (pure x) (pure x)
  f <*> x = Gen (sim f <*> sim x) (makeConstrainedGen f <*> makeConstrainedGen x)

instance Monad m => Monad (Gen m) where
  x >>= f = Gen (sim x >>= sim . f) (makeConstrainedGen x >>= makeConstrainedGen . f)

instance MonadTrans Gen where
  lift x = Gen x (lift (lift x))

mkInf :: Functor m => (Trace -> m (a, Trace, Log Double)) -> Inf m a
mkInf f = rwsT $ \obs () -> (\(x, t, w) -> (x, (), (t, Weight w))) <$> f obs

at :: Monad m => String -> Gen m a -> Gen m a
at k g = Gen (sim g) $
  withRWST (\t () -> (f t, ())) $
  mapRWST (fmap (\(a, (), (t, w)) -> (a, (), (M.mapKeys (k ++) t, w)))) $
  makeConstrainedGen g
  where
  n = length k
  f = M.mapKeys (drop n) . M.filterWithKey (\k' _ -> and (zipWith (==) k k'))

(~~) :: Monad m => String -> Gen m a -> Gen m a
(~~) = at

fromSamplerAndScorer :: Typeable a => Monad m => m a -> (a -> Double) -> Gen m a
fromSamplerAndScorer sampler scorer = Gen sampler $ mkInf $ \obs ->
  case M.lookup "" obs >>= fromDynamic of
    Nothing -> sampled <$> fromSamplerAndScorer sampler scorer
    Just x -> pure (x, M.singleton "" (toDyn x), Exp (scorer x))
  where
  sampled x = (x, M.singleton "" (toDyn x), 1)


prim :: MonadSample m => Typeable a => Distr a -> Gen m a
prim d = fromSamplerAndScorer (D.sample d) (D.score d)

dsPrim :: MonadState Heap m => MonadSample m => DeepForce a => Typeable a => Typeable (Forced a) => DS.Distr a -> Gen m a
dsPrim d = Gen (DS.sample d) $ mkInf $ \obs ->
  case M.lookup "" obs >>= fromDynamic of
    Nothing -> sampled <$> dsPrim d
    Just x -> do
      ((), w) <- lift $ runWeighted $ (DS.observe d x)
      pure (deepConst x, M.singleton "" (toDyn x), w)
  where
  sampled x = (x, M.singleton "" (toDyn x), 1)

simulate :: Functor m => Inf m a -> m (a, Trace, Log Double)
simulate g = fmap (\(a, s, (t, Weight w)) -> (a, t, w)) $ runRWST g mempty ()

fromInf :: Functor m => (Trace -> Gen m (a, Trace, Log Double)) -> Gen m a
fromInf f = Gen ((\(x, _, _) -> x) <$> sim (f mempty)) (mkInf f)

toInf :: Functor m => Gen m a -> Trace -> Gen m (a, Trace, Log Double)
toInf g t = fmap (\(a, s, (t, Weight ll)) -> (a, t, ll)) $ runRWST (makeConstrainedGen g) t ()

myInf :: MonadSample m => Gen m Double
myInf = fromInf $ \t -> do
  x <- at "myValue" $ prim $ D.normal 0 1
  pure (x, mempty, Exp (-1))

myGen :: MonadSample m => Gen m (Double, Double, Double)
myGen = do
  x <- at "normal1" $ prim (D.normal 0 1)
  y <- at "normal2" $ prim (D.normal x 1)
  z <- at "myInf" $ myInf
  pure (x, y, z)

tr :: Typeable a => String -> a -> Trace
tr k v = M.singleton k (toDyn v)

isequence :: Monad m => [Gen m a] -> Gen m [a]
isequence xs = sequence [ at ("." ++ show i) x | (i, x) <- zip [0..] xs ]

replicate :: Monad m => Int -> Gen m a -> Gen m [a]
replicate n g = isequence (Prelude.replicate n g)

trList :: Typeable a => String -> [a] -> Trace
trList k xs = mconcat [ tr (k ++ "." ++ show i) x | (i, x) <- zip [0..] xs]

observing :: MonadCond m => Trace -> Gen m a -> m a
observing t g = do
  (x, t, ll) <- sim (toInf g t)
  factor ll
  pure x

observing' :: Monad m => Trace -> Gen m a -> Weighted m a
observing' t g = withWeight $ do
  (x, t, ll) <- sim (toInf g t)
  pure (x, ll)

zobserving :: MonadCond m => ZStream (Gen m) a b -> ZStream m (a, Trace) b
zobserving (ZS.ZStream g) = ZS.fromStep step g
  where
  step f (a, t) = do
    ((ZS.ZStream f, x), outt, ll) <- sim (toInf (f a) t)
    factor ll
    pure (f, x)

zobserving' :: Monad m => ZStream (Gen m) a b -> ZStream (Weighted m) (a, Trace) b
zobserving' (ZS.ZStream g) = ZS.fromStep step g
  where
  step f (a, t) = withWeight $ do
    ((ZS.ZStream f, x), outt, ll) <- sim (toInf (f a) t)
    pure ((f, x), ll)

exactly :: MonadSample m => Eq a => Typeable a => a -> Gen m a
exactly x = prim (D.dirac x)

exactly' :: MonadSample m => Typeable a => a -> Gen m a
exactly' x = fromSamplerAndScorer (pure x) (error "Can't score")