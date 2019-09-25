{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}


module SymbolicDistr where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadSample)

import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat)

import Numeric.LinearAlgebra.Static (R, Sym, L)

import DelayedSampling hiding (sample, observe, score)
import qualified DelayedSampling as DelS
import qualified Distributions as D
import qualified MVDistributions as D
import DSProg
import Util.Ref (Heap, Ref, MonadState)

data Distr s = DeepForce s => Distr
  { sample :: forall m. MonadState Heap m => MonadSample m => m s
  , observe :: forall m. DelayedInfer m => Forced s -> m ()
  -- Score should not modify the heap, but currently we do not
  -- enforce this
  , score :: forall m. DelayedInfer m => Forced s -> m Double
  }

distrToSymDistr :: forall a. D.Distr a -> Distr (Expr a)
distrToSymDistr d = Distr (Const <$> D.sample d) (D.observe d)
  (pure . D.score d)

symDistrWithFallback  :: forall a. (forall m. MonadState Heap m => MonadSample m => m (D.Distr a))
  -> (forall m. MonadState Heap m => MonadSample m => m (Maybe (Expr a)))
  -> (forall m. DelayedInfer m => (a -> m (Maybe ())))
  -> (forall m. DelayedInfer m => (a -> m (Maybe Double)))
  -> Distr (Expr a)
symDistrWithFallback  d is iobs iscore = Distr is' iobs' iscore' where
  pd :: MonadState Heap m => MonadSample m => m (Distr (Expr a))
  pd = distrToSymDistr <$> d
  is' :: MonadState Heap m => MonadSample m => m (Expr a)
  is' = do
    mx <- is
    case mx of
      Nothing -> pd >>= sample
      Just x -> pure x
  iobs' :: DelayedInfer m => a -> m ()
  iobs' obs = do
    mupd <- iobs obs
    case mupd of
      Nothing -> do
        d <- pd
        observe d obs
      Just () -> pure ()
  iscore' :: DelayedInfer m => a -> m Double
  iscore' obs = do
    mupd <- iscore obs
    case mupd of
      Nothing -> do
        d <- pd
        score d obs
      Just ll -> pure ll

replicateIID :: forall a. DeepForce a => D.Distr Int -> Distr a -> Distr [a]
replicateIID howMany d = Distr sam obs scorer where
  sam :: MonadState Heap m => MonadSample m => m [a]
  sam = do
    n <- D.sample howMany
    replicateM n (sample d)
  obs :: DelayedInfer m => [Forced a] -> m ()
  obs xs = D.observe howMany (length xs) >> mapM_ (observe d) xs
  scorer :: DelayedInfer m => [Forced a] -> m Double
  scorer xs = do
    let ll = D.score howMany (length xs)
    ll' <- sum <$> mapM (score d) xs
    pure (ll + ll')

bind :: forall a b. a ~ Forced a => DeepForce a => DeepForce b => D.Distr a -> (a -> Distr b) -> Distr (a, b)
bind d f = Distr sam obs scorer where
  sam :: MonadState Heap m => MonadSample m => m (a, b)
  sam = do
    x <- D.sample d
    y <- sample (f x)
    pure (x, y)
  obs :: DelayedInfer m => (a, Forced b) -> m ()
  obs (x, y) = D.observe d x >> observe (f x) y
  scorer :: DelayedInfer m => (a, Forced b) -> m Double
  scorer (x, y) = (D.score d x +) <$> score (f x) y

indep :: forall a b. DeepForce a => DeepForce b => Distr a -> Distr b -> Distr (a, b)
indep dx dy = Distr ((,) <$> sample dx <*> sample dy)
                    (\(x, y) -> observe dx x >> observe dy y)
                    (\(x, y) -> (+) <$> score dx x <*> score dy y)

normal :: Expr Double -> Double -> Distr (Expr Double)
normal mu var = symDistrWithFallback (D.normal <$> force mu <*> pure var) is iobs iscore where
  is :: MonadState Heap m => MonadSample m => m (Maybe (Expr Double))
  is = do
      mu' <- realizedToConst mu
      case getAffine1 mu' of
        Nothing -> pure Nothing
        Just (b, mx) -> do
          case mx of
            Nothing -> do
              nref <- assumeConstant "" (MGaussian b var)
              Just <$> forgettableVar nref
            Just (RefNodeToT par, m) -> do
              ty <- typeOfRefNodeToT par
              case ty of
                SMGaussianT -> do
                  nref <- assumeConditional "" par (AffineMeanGaussian m b var)
                  Just <$> forgettableVar nref
                _ -> pure Nothing
  withPrior :: forall m x. MonadState Heap m => MonadSample m => (forall a. Double -> Double -> Ref (Node a MGaussianT) -> m x) -> m (Maybe x)
  withPrior f = do
    mu' <- realizedToConst mu
    case getAffine1 mu' of
      Nothing -> pure Nothing
      Just (b, Nothing) -> pure Nothing
      Just (b, Just (RefNodeToT par, m)) -> do
        ty <- typeOfRefNodeToT par
        case ty of
          SMGaussianT -> Just <$> f m b par
          _ -> pure Nothing
  iobs :: DelayedInfer m => Double -> m (Maybe ())
  iobs obs = withPrior $ \m b par -> observeConditional "" par (AffineMeanGaussian m b var) obs
  iscore :: DelayedInfer m => Double -> m (Maybe Double)
  iscore obs = withPrior $ \m b par -> scoreConditional "" par (AffineMeanGaussian m b var) obs

mvNormal :: forall n. KnownNat n => Expr (R n) -> Sym n -> Distr (Expr (R n))
mvNormal mu var = symDistrWithFallback (D.mvNormal <$> force mu <*> pure var) is iobs iscore where
  is :: MonadState Heap m => MonadSample m => m (Maybe (Expr (R n)))
  is = do
    mu' <- realizedToConst mu
    case getAffineMV1 mu' of
      Nothing -> pure Nothing
      Just (b, mx) -> do
        case mx of
          Nothing -> do
            nref <- assumeConstant "" (MMVGaussian b var)
            Just <$> forgettableVar nref
          Just (AffineMVMult (RefNodeToT par) m) -> do
            ty <- typeOfRefNodeToT par
            case ty of
              SMMVGaussianT -> do
                nref <- assumeConditional "" par (MVAffineMeanGaussian m b var)
                Just <$> forgettableVar nref
  withPrior :: forall m x. MonadState Heap m => MonadSample m => (forall a k (p :: Proxy k). KnownNat k => L n k -> R n -> Ref (Node a (MMVGaussianT p)) -> m x) -> m (Maybe x)
  withPrior f = do
    mu' <- realizedToConst mu
    case getAffineMV1 mu' of
      Nothing -> pure Nothing
      Just (b, Nothing) -> pure Nothing
      Just (b, Just (AffineMVMult (RefNodeToT par) m)) -> do
        ty <- typeOfRefNodeToT par
        case ty of
          SMMVGaussianT -> Just <$> f m b par
  iobs :: DelayedInfer m => R n -> m (Maybe ())
  iobs obs = withPrior $ \m b par -> observeConditional "" par (MVAffineMeanGaussian m b var) obs
  iscore :: DelayedInfer m => R n -> m (Maybe Double)
  iscore obs = withPrior $ \m b par -> scoreConditional "" par (MVAffineMeanGaussian m b var) obs

betaSD :: Double -> Double -> Distr (Expr Double)
betaSD a b = symDistrWithFallback  (pure $ D.beta a b) is iobs (const (pure Nothing)) where
  is :: MonadState Heap m => MonadSample m => m (Maybe (Expr Double))
  is = Just <$> (assumeConstant "" (MBeta a b) >>= forgettableVar)
  iobs obs = pure Nothing

bernoulliSD :: Expr Double -> Distr (Expr Bool)
bernoulliSD p = symDistrWithFallback (D.bernoulli <$> force p) is iobs iscore where
  withBetaPrior :: forall m x. MonadState Heap m => MonadSample m => (forall a. Ref (Node a MBetaT) -> m x) -> m (Maybe x)
  withBetaPrior f = case p of
    Var (RefNodeToT par) -> do
      ty <- typeOfRefNodeToT par
      case ty of
        SMBetaT -> do
          Just <$> f par
        _ -> pure Nothing
    _ -> pure Nothing
  is :: MonadState Heap m => MonadSample m => m (Maybe (Expr Bool))
  is = withBetaPrior $ \par ->
    forgettableVar =<< assumeConditional "" par CBernoulli
  iobs obs = withBetaPrior $ \par ->
    observeConditional "" par CBernoulli obs
  iscore obs = withBetaPrior $ \par ->
    scoreConditional "" par CBernoulli obs