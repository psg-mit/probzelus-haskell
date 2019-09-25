{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module DelayedSampling where

import Prelude hiding ((<>))

import Control.Exception.Base (assert)
import Control.Monad (forM_, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Bayes.Class (MonadSample, MonadCond, MonadInfer)
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler

import Data.Aeson
import Data.List (delete)
import Data.Maybe (isJust)
import Data.Proxy
import Data.Random hiding (normal)

import Debug.Trace

import GHC.TypeLits

import Numeric.AD
import Numeric.AD.Rank1.Tower (Tower)

import Numeric.LinearAlgebra.Static hiding (M)

import Unsafe.Coerce (unsafeCoerce)

import Distributions
import MVDistributions
import Inference
import Util.Ref

data MarginalT = MGaussianT | MBetaT | MBernoulliT | forall (n :: Nat). MMVGaussianT (Proxy n)

type DelayedSampling = MonadState Heap
type DelayedInfer m = (MonadState Heap m, MonadInfer m)

data SMarginalT (m :: MarginalT) where
  SMGaussianT :: SMarginalT MGaussianT
  SMBetaT :: SMarginalT MBetaT
  SMBernoulliT :: SMarginalT MBernoulliT
  SMMVGaussianT :: forall (n :: Nat) (p :: Proxy n). SMarginalT (MMVGaussianT p)


type family MType (m :: MarginalT) where
  MType MGaussianT = Double
  MType MBetaT = Double
  MType MBernoulliT = Bool
  MType (MMVGaussianT (p :: Proxy n)) = R n


data MDistr (m :: MarginalT) where
  MGaussian :: !Double -> !Double -> MDistr MGaussianT
  MBeta :: !Double -> !Double -> MDistr MBetaT
  MBernoulli :: !Double -> MDistr MBernoulliT
  MMVGaussian :: forall (n :: Nat) (p :: Proxy n). KnownNat n => !(R n) -> !(Sym n) -> MDistr (MMVGaussianT p)

--deriving instance Eq (MDistr m)
deriving instance Show (MDistr m)

instance ToJSON (MDistr a) where
  toJSON (MMVGaussian mu cov) = object ["mean" .= mu, "cov" .= cov]
  toJSON (MGaussian mu cov) = object ["mean" .= mu, "cov" .= cov]
  toJSON (MBeta a b) = object ["alpha" .= a, "beta" .= b]
  toJSON (MBernoulli p) = object ["p" .= p]


data CDistr (m :: MarginalT) (m' :: MarginalT) where
  AffineMeanGaussian :: !Double -> !Double -> !Double -> CDistr MGaussianT MGaussianT
  CBernoulli :: CDistr MBetaT MBernoulliT
  MVAffineMeanGaussian :: forall (n :: Nat) (m :: Nat) (pn :: Proxy n) (pm :: Proxy m).
    (KnownNat n, KnownNat m) => (L m n) -> (R m) -> (Sym m) -> CDistr (MMVGaussianT pn) (MMVGaussianT pm)

gaussianConditioning :: Double -> Double -> Double -> Double -> (Double, Double)
gaussianConditioning mu var obs obsvar = (mu', var')
  where
  ivar = recip var
  iobsvar = recip obsvar
  inf = ivar + iobsvar
  var' = recip inf
  mu' = (ivar * mu + iobsvar * obs) / inf

gaussianMeanGaussian :: Double -> CDistr MGaussianT MGaussianT
gaussianMeanGaussian = AffineMeanGaussian 1 0

ascend1 :: Floating d => d -> (d -> d) -> Int -> d -> d
ascend1 lr diff = go where
  go 0 x = x
  go n x = let d = diff x in go (n - 1) (x + (lr * d))

-- XXX: not sure that the factor amount is good
solveLaplaceGaussian :: Double -> Double
  -> (forall s. AD s (Tower Double) -> AD s (Tower Double))
  -> Double
  -> Int
  -> (Double, Double, Double)
solveLaplaceGaussian mu var likelihood learningRate numIters =
  (mu', var', factorAmt - originalFactorAmt)
  where
  fmu' : _ : f''mu' : _ = diffs0 f mu'
  originalFactorAmt = log $ sqrt (pi / var) * exp (gaussian_ll' mu var mu)
  factorAmt = log $ sqrt (- 2 * pi / f''mu') * exp fmu'
  var' :: Double
  var' = -2 / f''mu'
  mu' ::  Double
  mu' = ascend1 learningRate ((!! 1) . diffs0 f) numIters mu
  f :: forall s. AD s (Tower Double) -> AD s (Tower Double)
  f x = gaussian_ll' (auto mu) (auto var) x + likelihood x

mdistrToDistr :: MDistr a -> Distr (MType a)
mdistrToDistr (MGaussian mu var) = normal mu var
mdistrToDistr (MMVGaussian mu var) = mvNormal mu var
mdistrToDistr (MBeta a b) = beta a b
mdistrToDistr (MBernoulli p) = bernoulli p

cdistrToDistr :: CDistr m m' -> MType m -> MDistr m'
cdistrToDistr (AffineMeanGaussian m b obsvar) mu = MGaussian (m * mu + b) obsvar
cdistrToDistr (MVAffineMeanGaussian f b obsvar) mu = MMVGaussian (f #> mu + b) obsvar
cdistrToDistr CBernoulli p = MBernoulli p

makeMarginal :: MDistr a -> CDistr a b -> MDistr b
makeMarginal (MGaussian mu var) (AffineMeanGaussian m b obsvar) =
  MGaussian (m * mu + b) (m^2 * var + obsvar)
makeMarginal (MMVGaussian mu var) (MVAffineMeanGaussian f b obsvar) =
  MMVGaussian (f #> mu + b) (conjugate f var + obsvar)
makeMarginal (MBeta a b) CBernoulli = MBernoulli (a / (a + b))
makeMarginal (MBernoulli _) _ = error "impossible"

makeConditional :: MDistr a -> CDistr a b -> MType b -> MDistr a
makeConditional (MGaussian mu var) (AffineMeanGaussian m b obsvar) obs =
  MGaussian mu' var'
  where (mu', var') = gaussianConditioning mu var ((obs - b) / m) (obsvar / m^2)
makeConditional (MMVGaussian mu var) (MVAffineMeanGaussian f b obsvar) obs =
  MMVGaussian mu' var'
  where (mu', var') = fst (kalmanUpdate obs f obsvar (mu, var))
makeConditional (MBeta a b) CBernoulli tf =
  if tf then MBeta (a + 1) b else MBeta a (b + 1)
makeConditional (MBernoulli _) _ _ = error "impossible"

data State b where
  Initialized :: State b
  Marginalized :: MDistr b -> State b

deriving instance Show (State MGaussianT)
deriving instance Show (State MBetaT)

isMarginalized :: State b -> Bool
isMarginalized (Marginalized _) = True
isMarginalized _ = False

data DSDistr a b where
   UDistr :: !(MDistr b) -> DSDistr a b
   CDistr :: !(Ref (Node z a)) -> !(CDistr a b) -> DSDistr a b

typeOfMDistr :: MDistr b -> SMarginalT b
typeOfMDistr (MGaussian _ _) = SMGaussianT
typeOfMDistr (MBeta _ _) = SMBetaT
typeOfMDistr (MBernoulli _) = SMBernoulliT
typeOfMDistr (MMVGaussian _ _) = SMMVGaussianT

typeOfCDistr :: CDistr m m' -> SMarginalT m'
typeOfCDistr (AffineMeanGaussian _ _ _) = SMGaussianT
typeOfCDistr CBernoulli = SMBernoulliT
typeOfCDistr (MVAffineMeanGaussian _ _ _) = SMMVGaussianT

typeOfDSDistr :: DSDistr a b -> SMarginalT b
typeOfDSDistr (UDistr d) = typeOfMDistr d
typeOfDSDistr (CDistr _ c) = typeOfCDistr c


data Node a b = Node
  { name :: !String
  , children :: [RefNodeFrom b]
  , state :: !(State b)
  , distr :: !(DSDistr a b)
  }
  | RealizedNode (MType b)

data RefNode a b where
  RefNode :: !(Ref (Node a b)) -> RefNode a b

data SomeRefNode where
  SomeRefNode :: !(Ref (Node a b)) -> SomeRefNode

instance Eq SomeRefNode where
  SomeRefNode x == SomeRefNode y = x == unsafeCoerce y

data Path e (a :: MarginalT) (b :: MarginalT) where
  PNil :: Path e a a
  PSnoc :: e b c -> Path e a b -> Path e a c

data PathTo e b where
  PathTo :: !(Path e a b) -> PathTo e b

data RefNodeTo b where
  RefNodeTo :: !(Ref (Node a b)) -> RefNodeTo b

data RefNodeFrom a where
  RefNodeFrom :: !(Ref (Node a b)) -> RefNodeFrom a

instance Eq (RefNodeFrom a) where
  RefNodeFrom x == RefNodeFrom y = x == unsafeCoerce y

instance Eq (RefNodeTo a) where
  RefNodeTo x == RefNodeTo y = x == unsafeCoerce y

ancestry :: DelayedSampling m => Node a b -> m (PathTo RefNode a)
ancestry n = case distr n of
  UDistr _ -> pure $ PathTo PNil
  CDistr par cdistr -> do
    PathTo ancestors <- ancestry =<< readRef par
    pure (PathTo (PSnoc (RefNode par) ancestors))

path_all :: Applicative f => (forall a b. e a b -> f Bool) -> Path e a b -> f Bool
path_all f PNil = pure True
path_all f (PSnoc e es) = (&&) <$> f e <*> path_all f es

isTerminal :: DelayedSampling m =>Node a b -> m Bool
isTerminal n = do
  PathTo ancestors <- ancestry n
  ancestors_all_marginalized <- path_all (\(RefNode n) -> isMarginalized . state <$> readRef n) ancestors
  pure $ isMarginalized (state n) && ancestors_all_marginalized

-- initialize without parent node
constant' :: String -> MDistr a -> Node z a
constant' n d = Node
  { name = n
  , children = []
  , state = Initialized
  , distr = UDistr d
  }

assumeConstant :: DelayedSampling m => String -> MDistr a -> m (Ref (Node z a))
assumeConstant n d = newRef (constant' n d)

-- initialize with parent node
newConditional' :: DelayedSampling m => String -> Ref (Node a b) -> CDistr b c -> m (Ref (Node b c))
newConditional' str par cdistr = do
  childRef <- newRef child
  modifyRef' par $ \n -> n { children = (RefNodeFrom childRef) : children n }
  --childRef' <- mkWeakRef childRef $ putStrLn ("deleted " ++ str)
  pure childRef
  where
  child = Node
    { name = str
    , children = []
    , state = Initialized
    , distr = CDistr par cdistr }

assumeConditional :: DelayedSampling m => String -> Ref (Node a b) -> CDistr b c -> m (Ref (Node b c))
assumeConditional str par cdistr = newConditional' str par cdistr

updater :: DelayedSampling m => (a -> m a) -> Ref a -> m ()
updater f ref = do
  x <- readRef ref
  y <- f x
  writeRef ref y


marginalize :: DelayedSampling m => MDistr a -> Ref (Node a b) -> m (MDistr b)
marginalize parMarginal nref = do
  n <- readRef nref
  writeLog ("marginalize " ++ name n)
  case (state n, distr n) of
    (Initialized, CDistr par cdistr) -> do
      let marginal' = makeMarginal parMarginal cdistr
      writeRef nref $ n { state = Marginalized marginal' }
      pure marginal'
    (state, _) -> error $  "marginalize': " ++ name n

marginalize2' :: MType a -> Node a b -> Node a b
marginalize2' value n =
  case (state n, distr n) of
    (Initialized, CDistr _ cdistr) ->
      let marg = cdistrToDistr cdistr value in
      n { state = Initialized, distr = UDistr marg }
    _ -> error "marginalize2'"

marginalize2 :: DelayedSampling m => MType a -> Ref (Node a b) -> m ()
marginalize2 a = updater (pure . marginalize2' a)

sample :: DelayedSampling m => MonadSample m => Ref (Node a b) -> m ()
sample nref = do
  n <- readRef nref
  ioAssert (isTerminal n)
  writeLog ("sample " ++ name n)
  case state n of
    Marginalized m -> do
      x <- Distributions.sample (mdistrToDistr m)
      realize x nref
    _ -> error "sample"

updateParent :: DelayedSampling m => Node b c -> (forall a. CDistr b c -> Node a b -> m (Node a b)) -> m ()
updateParent n f = case distr n of
  UDistr _ -> pure ()
  CDistr p cdistr -> updater (f cdistr) p

firstSatisfying :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstSatisfying p [] = pure Nothing
firstSatisfying p (x : xs) = do
  px <- p x
  if px then pure (Just x) else firstSatisfying p xs

-- Invariant 2: A node always has at most one marginal Child
marginalChild :: DelayedSampling m => Node a b -> m (Maybe (RefNodeFrom b))
marginalChild n = firstSatisfying (\(RefNodeFrom x) -> isMarginalized . state <$> readRef x) (children n)

ioAssert :: Applicative m => m Bool -> m ()
ioAssert x = pure () -- do {b <- x; assert b (pure ())}

writeLog :: Applicative m => String -> m ()
writeLog = const $ pure () -- putStrLn

realize :: DelayedSampling m => MType b -> Ref (Node a b) -> m ()
realize val nref = do
  n <- readRef nref
  writeLog ("realize " ++ name n)
  ioAssert (isTerminal n)
  updateParent n $ \cdistr p -> do
    let Marginalized marg = state p
    let marg' = makeConditional marg cdistr val
    pure $ p { state = Marginalized marg', children = delete (RefNodeFrom nref) (children p) }
  forM_ (children n) $ \(RefNodeFrom c) -> do
    marginalize2 val c
  writeRef nref $ RealizedNode val

score' :: DelayedSampling m => MonadCond m => MType b -> Ref (Node a b) -> m Double
score' x nref = do
  n <- readRef nref
  ioAssert (isTerminal n)
  writeLog ("observe " ++ name n)
  pure $ case state n of
    Marginalized marg -> Distributions.score (mdistrToDistr marg) x
    _ -> error "observe'"

observe :: DelayedSampling m => MonadCond m => MType b -> Ref (Node a b) -> m ()
observe x nref = do
  ll <- score' x nref
  factor ll
  realize x nref

prune :: DelayedSampling m => MonadSample m => Ref (Node a b) -> m ()
prune nref = do
  n <- readRef nref
  writeLog ("prune " ++ name n)
  assert (isMarginalized (state n)) $ do
    maybeChild <- marginalChild n
    forM_ maybeChild $ \(RefNodeFrom c) ->
      prune c
    DelayedSampling.sample nref

graft :: DelayedSampling m => MonadSample m => Ref (Node a b) -> m (MDistr b)
graft nref = do
  n <- readRef nref
  writeLog ("graft " ++ name n)
  case state n of
    Marginalized _ -> do
      maybeChild <- marginalChild n
      forM_ maybeChild $ \(RefNodeFrom c) -> prune c
      n'  <- readRef nref
      pure $ case state n' of
        Marginalized m -> m
        _ -> error "graft"
    Initialized -> case distr n of
      UDistr d -> do
        writeRef nref $ n { state = Marginalized d }
        pure d
      CDistr par cdistr -> do
        parMarg <- graft par
        marginalize parMarg nref

obs :: DelayedSampling m => MonadInfer m => MType b -> Ref (Node a b) -> m ()
obs x n = do
  graft n
  DelayedSampling.observe x n

score :: DelayedSampling m => MonadInfer m => MType b -> Ref (Node a b) -> m Double
score x n = do
  graft n
  score' x n


getValue :: DelayedSampling m => MonadSample m => Ref (Node a b) -> m (MType b)
getValue nref = do
  n <- readRef nref
  case n of
    RealizedNode x -> pure x
    _ -> do
      graft nref
      DelayedSampling.sample nref
      getValue nref

printValue :: Show (MType b) => DelayedSampling m => MonadSample m => MonadIO m => Ref (Node a b) -> m ()
printValue n = do
  x <- getValue n
  liftIO $ print x

printState :: Show (State b) => DelayedSampling m => MonadIO m => Ref (Node a b) -> m ()
printState nref = do
  n <- readRef nref
  liftIO $ print (state n)

forget :: DelayedSampling m => Ref (Node a b) -> m ()
forget nref = do
  n <- readRef nref
  case n of
    RealizedNode _ -> pure ()
    _ -> case state n of
      Initialized -> error "forget"
      Marginalized marg -> do
        forM_ (children n) $ \(RefNodeFrom cref) -> do
          modifyRef' cref $ \c ->
            c { distr = case distr c of
              UDistr d -> UDistr d
              CDistr cdistr par -> case state c of
                Marginalized marg -> UDistr marg
                _ -> error "forget" }
        case distr n of
          UDistr d -> pure ()
          CDistr cdistr par -> error "forget: Shouldn't have parents?"
        writeRef nref $ n { distr = UDistr marg }
  modifyRef' nref $ \n' -> n' { children = [] }
  freeRef nref

-- sweep :: [SomeRefNode] -> M ()
-- sweep liveVars = mapM_ (\(SomeRefNode n) -> forgetRelated n) liveVars where
--   forgetRelated :: Ref (Node a b) -> M ()
--   forgetRelated nref = undefined
--   forgettable :: Ref (Node a b) -> M Bool
--   forgettable nref =
--     if SomeRefNode nref `elem` liveVars
--       then pure False
--       else case state n of

stale :: DelayedSampling m => Ref (Node a b) -> m Bool
stale nref = do
  n <- readRef nref
  case n of
    RealizedNode _ -> pure False
    _ -> case state n of
      Initialized -> case distr n of
        UDistr _ -> pure False
        CDistr par _ -> stale par
      Marginalized d -> isJust <$> marginalChild n


-- Examples

delay_triplet :: DelayedInfer m => MonadIO m => Double -> m ()
delay_triplet zobs = do
  x <- assumeConstant "x" (MGaussian 0 1)
  writeLog "x created"
  y <- assumeConditional "y"  x (gaussianMeanGaussian 1)
  writeLog "y created"
  z <- assumeConditional "z" y (gaussianMeanGaussian 1)
  writeLog "z created"
  obs zobs z
  writeLog "z observed"
  printValue z
  printValue x
  printValue y

observeConditional :: DelayedInfer m => String -> Ref (Node a b) -> CDistr b c -> MType c -> m ()
observeConditional str nref cdistr observation = do
  y <- assumeConditional str nref cdistr
  obs observation y
  freeRef y

scoreConditional :: DelayedInfer m => String -> Ref (Node a b) -> CDistr b c -> MType c -> m Double
scoreConditional str nref cdistr observation = do
  y <- assumeConditional str nref cdistr
  ll <- DelayedSampling.score observation y
  modifyRef' nref $ \n -> n { children = tail (children n) }
  freeRef y
  pure ll

-- For some reason, likelihoods aren't exactly  the same
delay_iidADF :: DelayedInfer m => MonadIO m => Bool -> [Double] -> m ()
delay_iidADF useADF yobs = do
  x <- assumeConstant "x" (MGaussian 0 1)
  forM_ (zip [1..] yobs) $ \(t, obsyt) -> do
    if useADF
      then observeGaussianMeanADFLaplace x (\mu -> gaussian_ll' mu 1 (auto obsyt)) 0.05 10000
      else observeConditional ("y" ++ show t) x (gaussianMeanGaussian 1) obsyt  -- normal(x, 1)
    printState x
  printValue x

observeGaussianMeanADFLaplace :: DelayedInfer m =>
      Ref (Node a MGaussianT)
  -> (forall s. AD s (Tower Double) -> AD s (Tower Double))
  -> Double
  -> Int
  -> m ()
observeGaussianMeanADFLaplace nref likelihood learningRate numIters = do
   graft nref
   n <- readRef nref
   case n of
    RealizedNode x -> factor (diffs likelihood x !! 0)
    _ -> case state n of
      Marginalized (MGaussian mu var) -> do
        let (mu', var', factorAmt) = solveLaplaceGaussian mu var likelihood learningRate numIters
        factor factorAmt
        writeRef nref $ n { state = Marginalized (MGaussian mu' var') }
      _ -> error "observeGaussianMeanADFLaplace"


delay_kalman :: DelayedInfer m => MonadIO m =>
  Bool -> Double -> Double -> [Double] -> m ()
delay_kalman shouldForget m b yobs = do
  x1 <- assumeConstant "x1" (MGaussian 0 1)
  observeConditional "y1" x1 (gaussianMeanGaussian 1) (head yobs)
  loop 2 x1 (tail yobs)
  where
  loop :: DelayedInfer m => MonadIO m =>
    Integer -> Ref (Node a MGaussianT) -> [Double] -> m ()
  loop t xpredt [] = pure ()
  loop t xpredt (y : ys) = do
    xt <- assumeConditional ("x" ++ show t) xpredt (AffineMeanGaussian m b 1)
    observeConditional ("y" ++ show t) xt (gaussianMeanGaussian 1) y
    when (t `mod` 100 == 0) $ do
      liftIO $ putStrLn ("t = " ++ show t)
      printState xt
    when shouldForget $ forget xpredt
    loop (t + 1) xt ys
