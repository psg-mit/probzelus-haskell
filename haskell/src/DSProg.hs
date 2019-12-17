{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}


module DSProg (
  module DSProg,
  Result
) where

import Prelude hiding ((<>))

import Control.Monad (join, (<=<), forM_, void, replicateM)
import Control.Monad.State (StateT, MonadState (get, put), evalStateT)
import qualified Control.Monad.State as State
import Control.Monad.Bayes.Class (MonadSample, MonadInfer)
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Trans (liftIO)
import Control.Monad.Fix

import Data.Aeson (ToJSON (toJSON))
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map
import Data.Proxy (Proxy (Proxy))

import GHC.TypeLits

import Numeric.LinearAlgebra.Static hiding (Gaussian, M)

import Unsafe.Coerce (unsafeCoerce)

import qualified Util.AssocList as M
import Util.AssocList (Map)
import qualified Util.AssocListSet as S
import Util.AssocListSet (Set)
import qualified Util.MStream as MS
import Util.MStream (MStream)
import Distributions
import MVDistributions
import DelayedSampling
import Inference (particles, Weight, zunheap)
import Util.Numeric (average)
import Util.Ref

import qualified Util.ZStream as ZS

import Numeric.Log (Log (Exp))

instance ToJSON a => ToJSON (Result a) where
  toJSON (RConst x) = toJSON x
  toJSON (RMarginal d) = toJSON d

instance MonadState s m => MonadState s (Weighted m) where
  get = State.lift get
  put = State.lift . put
  state = State.lift . State.state

meanMDistr :: Double ~ MType a => MDistr a -> Double
meanMDistr (MGaussian mu var) = mu
meanMDistr (MBeta a b) = a / (a + b)

meanResult :: Result Double -> Double
meanResult (RConst x) = x
meanResult (RMarginal m) = meanMDistr m

addResult :: Num a => Result a -> Result a -> Maybe (Result a)
addResult (RConst x) (RConst y) = Just $ RConst (x + y)
addResult (RConst x) (RMarginal (MGaussian mu var)) = Just $ RMarginal (MGaussian (mu + x) var)
addResult (RMarginal (MGaussian mu var)) (RConst x) = addResult (RConst x) (RMarginal (MGaussian mu var))
addResult (RMarginal (MGaussian mu var)) (RMarginal (MGaussian mu' var')) = Nothing
  --  Just $ RMarginal (MGaussian (mu + mu') (var + var')) -- only valid when variables are independent
addResult _ _ = Nothing

negateResult :: Num a => Result a -> Maybe (Result a)
negateResult (RConst x) = Just $ RConst (- x)
negateResult (RMarginal (MGaussian mu var)) = Just $ RMarginal (MGaussian (- mu) var)
negateResult _ = Nothing

multResult :: Num a => Result a -> Result a -> Maybe (Result a)
multResult (RConst x) (RConst y) = Just $ RConst (x * y)
multResult (RConst x) (RMarginal (MGaussian mu var)) = Just $ RMarginal (MGaussian (x * mu) (x^2 * var))
multResult (RMarginal (MGaussian mu var)) (RConst x) = multResult (RConst x) (RMarginal (MGaussian mu var))
multResult _ _ = Nothing

data Expr' var a where
  Const :: a -> Expr' var a
  Var :: var a -> Expr' var a
  Plus :: Num a => Expr' var a -> Expr' var a -> Expr' var a
  Times :: Num a => Expr' var a -> Expr' var a -> Expr' var a
  MVMul :: (KnownNat m, KnownNat n) => Expr' var (L m n) -> Expr' var (R n) -> Expr' var (R m)

instance Show (RefNodeToT a) where
  show (RefNodeToT r) = show r

deriving instance (Show a) => Show (Expr' RefNodeToT a)

class DeepForce e where
  type Forced e :: *
  deepForce :: MonadState Heap m => MonadSample m => e -> m (Forced e)
  deepConst :: Forced e -> e

deepForce' :: MonadState Heap m => MonadSample m => DeepForce e => e -> m e
deepForce' = fmap deepConst . deepForce

instance DeepForce Double where
  type Forced Double = Double
  deepForce = pure
  deepConst = id

instance DeepForce Int where
  type Forced Int = Int
  deepForce = pure
  deepConst = id

instance DeepForce (R n) where
  type Forced (R n) = R n
  deepForce = pure
  deepConst = id

instance DeepForce (L m n) where
  type Forced (L m n) = L m n
  deepForce = pure
  deepConst = id

instance DeepForce (Expr a) where
  type Forced (Expr a) = a
  deepForce = force
  deepConst = Const

instance DeepForce e => DeepForce [e] where
  type Forced [e] = [Forced e]
  deepForce = mapM deepForce
  deepConst = map deepConst

instance DeepForce e => DeepForce (Data.Map.Map k e) where
  type Forced (Data.Map.Map k e) = Data.Map.Map k (Forced e)
  deepForce = mapM deepForce
  deepConst = fmap deepConst

instance (DeepForce a, DeepForce b) => DeepForce (a, b) where
  type Forced (a, b) = (Forced a, Forced b)
  deepForce (x, y) = (,) <$> deepForce x <*> deepForce y
  deepConst (x, y) = (deepConst x, deepConst y)

instance (DeepForce a, DeepForce b, DeepForce c) => DeepForce (a, b, c) where
  type Forced (a, b, c) = (Forced a, Forced b, Forced c)
  deepForce (x, y, z) = (,,) <$> deepForce x <*> deepForce y <*> deepForce z
  deepConst (x, y, z) = (deepConst x, deepConst y, deepConst z)

instance (DeepForce a, DeepForce b, DeepForce c, DeepForce d) => DeepForce (a, b, c, d) where
  type Forced (a, b, c, d) = (Forced a, Forced b, Forced c, Forced d)
  deepForce (x, y, z, w) = (,,,) <$> deepForce x <*> deepForce y <*> deepForce z <*> deepForce w
  deepConst (x, y, z, w) = (deepConst x, deepConst y, deepConst z, deepConst w)

type M = Weighted (StateT Heap SamplerIO)

evalM :: M a -> SamplerIO (Double, a)
evalM x = (\(x, Exp w) -> (w, x)) <$> evalStateT (runWeighted x) emptyHeap

forgettableVar :: MonadState Heap m => Ref (Node a b) -> m (Expr (MType b))
forgettableVar nref = do
  --  addFinalizer v (forget nref)
  pure v
  where
  v = Var (RefNodeToT nref)

type Expr = Expr' RefNodeToT

data RefNodeToT b where
  RefNodeToT :: Ref (Node a b) -> RefNodeToT (MType b)

instance Eq (RefNodeToT a) where
  RefNodeToT (Ref x) == RefNodeToT (Ref y) = x == unsafeCoerce y

instance Num a => Num (Expr' var a) where
  fromInteger = Const . fromInteger
  x + y = Plus x y
  x * y = Times x y




fmapExpr' :: forall f var var' a. Applicative f => (forall x. var x -> f (var' x)) -> Expr' var a -> f (Expr' var' a)
fmapExpr' varMap = f where
  f :: forall a. Expr' var a -> f (Expr' var' a)
  f (Const x) = pure $ Const x
  f (Var i) = Var <$> varMap i
  f (Plus x y) = Plus <$> f x <*> f y
  f (Times x y) = Times <$> f x <*> f y
  f (MVMul a x) = MVMul <$> f a <*> f x

read :: MonadState Heap m => Expr' RefNodeToT a -> m (Expr' NodeTo a)
read = fmapExpr' (\(RefNodeToT nref) -> NodeTo <$> readRef nref)

data AffineMVMult (m :: Nat) where
  AffineMVMult :: KnownNat n => RefNodeToT (R n) -> L m n -> AffineMVMult m

getAffineMV :: KnownNat m => Expr (R m) -> Maybe ([AffineMVMult m], R m)
getAffineMV (Const x) = Just ([], x)
getAffineMV (Var i) = Just ([AffineMVMult i eye], 0)
getAffineMV (Plus x y) = do
  (m1, b1) <- getAffineMV x
  (m2, b2) <- getAffineMV y
  Just (m1 ++ m2, b1 + b2)
getAffineMV (Times x y) = Nothing
getAffineMV (MVMul (Const f) x) = do
  (m, b) <- getAffineMV x
  Just ([ AffineMVMult z (f <> g) | AffineMVMult z g <- m ], f #> b)
getAffineMV (MVMul _ _) = Nothing

getAffineMV1 :: KnownNat m => Expr (R m) -> Maybe (R m, Maybe (AffineMVMult m))
getAffineMV1 e = getAffineMV e >>= toAffine
  where
  toAffine :: KnownNat m => ([AffineMVMult m], R m) -> Maybe (R m, Maybe (AffineMVMult m))
  toAffine (m, b) = do
    mx <- case m of
      [] -> Just Nothing
      [x] -> Just (Just x)
      _ -> Nothing
    pure (b, mx)

getAffine :: Num a => Expr a -> Maybe (Map (Maybe (RefNodeToT a)) a)
getAffine (Const x) = Just (M.singleton Nothing x)
getAffine (Var i) = Just (M.singleton (Just i) 1)
getAffine (Plus x y) = M.unionWith (+) <$> getAffine x <*> getAffine y
getAffine (Times x y) = do
  mx <- getAffine x
  my <- getAffine y
  terms <- sequence [ mult x1 x2 | x1 <- M.toList mx, x2 <- M.toList my ]
  Just (M.fromList terms)
  where
  mult (Nothing, x) (Nothing, y) = Just (Nothing, x * y)
  mult (Nothing, x) (Just i, y) = Just (Just i, x * y)
  mult (Just i, x) (Nothing, y) = Just (Just i, x * y)
  mult (Just i, x) (Just j, y) = Nothing
getAffine (MVMul f x) = Nothing

getAffine1 :: Num a => Expr a -> Maybe (a, Maybe (RefNodeToT a, a))
getAffine1 e = getAffine e >>= toAffine

toAffine :: Num v => Map (Maybe k) v -> Maybe (v, Maybe (k, v))
toAffine mp = let (mconst, mvar) = M.partition Data.Maybe.isNothing mp in do
  let b = case mconst of
            M.Map [] -> 0
            M.Map [(Nothing, v)] -> v
            _ -> error "impossible"
  mx <- case mvar of
    M.Map [] -> Just Nothing
    M.Map [(Just k, v)] -> Just (Just (k, v))
    _ -> Nothing
  pure (b, mx)

realizedToConst' :: forall var f a. Applicative f => (forall x. var x -> f (Maybe x)) -> Expr' var a -> f (Expr' var a)
realizedToConst' varMap = f where
  f :: forall a. Expr' var a -> f (Expr' var a)
  f (Const x) = pure $ Const x
  f (Var i) = flip fmap (varMap i) $ \mk -> case mk of
      Nothing -> Var i
      Just k -> Const k
  f (Plus x y) = Plus <$> f x <*> f y
  f (Times x y) = Times <$> f x <*> f y
  f (MVMul a x) = MVMul <$> f a <*> f x

realizedToConst :: MonadState Heap m => Expr a -> m (Expr a)
realizedToConst = realizedToConst' f where
  f :: MonadState Heap m => RefNodeToT b -> m (Maybe b)
  f (RefNodeToT nref) = do
    n <- readRef nref
    pure $ case n of
      RealizedNode x -> Just x
      _ -> Nothing

allVars :: Expr a -> Set (SomeRefNode)
allVars (Const x) = S.empty
allVars (Var (RefNodeToT i)) = S.singleton (SomeRefNode i)
allVars (Plus x y) = allVars x `S.union` allVars y
allVars (Times x y) = allVars x `S.union` allVars y
allVars (MVMul f x) = allVars f `S.union` allVars x


initializedMarginal :: MonadState Heap m => Ref (Node a b) -> m (MDistr b)
initializedMarginal nref = do
  n <- readRef nref
  case distr n of
    UDistr d -> pure d
    CDistr par cd -> do
      Just pard <- marginalRef par
      pure (makeMarginal pard cd)

marginalRef :: MonadState Heap m => Ref (Node a b) -> m (Maybe (MDistr b))
marginalRef nref = do
  n <- readRef nref
  case n of
    RealizedNode x -> error "shouldn't occur"
    _ -> case state n of
      Marginalized d -> do
        isStale <- stale nref
        pure $ if isStale then Nothing else Just d
      Initialized -> Just <$> initializedMarginal nref

-- This is currently unsound
-- Consider
-- x <- normal(0, 1)
-- pure (x + (- x))
marginal :: MonadState Heap m => Expr a -> m (Maybe (Result a))
marginal (Const x) = pure (Just (RConst x))
marginal (Var (RefNodeToT nref)) = do
  n <- readRef nref
  case n of
    RealizedNode x -> pure $ Just (RConst x)
    _ -> case state n of
      Marginalized d -> do
        isStale <- stale nref
        pure $ if isStale then Nothing else Just (RMarginal d)
      Initialized -> Just . RMarginal <$> initializedMarginal nref
marginal (Plus x y) = pure Nothing -- could do independence ananlysis, etc.
marginal (Times x y) = pure Nothing -- could do independence ananlysis, etc.
marginal (MVMul f x) = pure Nothing

marginal' :: MonadState Heap m => Expr a -> m (Result a)
marginal' = fmap fromJust . marginal

data NodeTo a where
  NodeTo :: Node z a -> NodeTo (MType a)

force :: MonadSample m => MonadState Heap m => Expr a -> m a
force (Const x) = pure x
force (Var (RefNodeToT nref)) = getValue nref
force (Plus x y) = (+) <$> force x <*> force y
force (Times x y) = (*) <$> force x <*> force y
force (MVMul a x) = (#>) <$> force a <*> force x

typeOfRefNodeToT :: MonadState Heap m => Ref (Node a b) -> m (SMarginalT b)
typeOfRefNodeToT nref = typeOfDSDistr . distr <$> readRef nref

forgetE :: MonadState Heap m => Expr a -> m ()
forgetE e = forM_ (allVars e) forgetSomeRefNode


forgetSomeRefNode :: MonadState Heap m => SomeRefNode -> m ()
forgetSomeRefNode (SomeRefNode nref) = forget nref

justRun :: Show p => MonadState Heap m => State.MonadIO m => MStream m (Expr p) (Expr a) -> m (Maybe (Result a))
justRun s = do
  res <- MS.runStream (\x -> marginal x >>= liftIO . print) s
  res' <- marginal res
  pure res'

runParticle :: Int -> MStream M (Maybe (Result Double)) (Maybe (Result Double)) -> StateT Heap SamplerIO Double
runParticle numParticles =
  fmap avg . MS.runStream (liftIO . print . avg)
  . Inference.particles numParticles
  where
  avg = average . map (meanResult . fromJust)
  fromJust (Just x) = x
  fromJust Nothing = RConst $ Prelude.read "NaN"

zdeepForce :: DeepForce b => MonadSample m => ZS.ZStream (StateT Heap m) a b -> ZS.ZStream m a (Forced b)
zdeepForce model = zunheap $ ZS.zconstM deepForce ZS.<<< model

zdeepForce' :: DeepForce b => MonadSample m => ZS.ZStream (StateT Heap m) a b -> ZS.ZStream m a b
zdeepForce' model = fmap deepConst (zdeepForce model)