{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DelayedSamplingPure where

import Prelude hiding ((<>))

import Control.Exception.Base (assert)
import Control.Monad (forM_, when)

import Data.List (delete)
import Data.Maybe (isJust)
import Data.Proxy
import Data.IORef

import Debug.Trace

import GHC.TypeLits

import Numeric.AD
import Numeric.AD.Rank1.Tower (Tower)

import Numeric.LinearAlgebra.Static hiding (M)

import Unsafe.Coerce (unsafeCoerce)

import Distributions
import MVDistributions
import Inference

data Node' b where
  NMarginalized :: Maybe (CDistr b c, Node' c) -> [InitTree b] -> Node' b

data RootNode a where
  RootNode :: MDistr a -> Node' a -> RootNode a

data InitTree b where
  InitTree :: CDistr b c -> [InitTree c] -> InitTree b

data MPath where
  Root :: MPath
  Child :: MPath -> MPath

data IPath = IPath [Int]

data AnyPath where
  AMPath :: MPath -> AnyPath
  AIPath :: IPath -> AnyPath

data MarginalT = MGaussianT | MBetaT | MBernoulliT | forall (n :: Nat). MMVGaussianT (Proxy n)

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
   CDistr :: !(IORef (Node z a)) -> !(CDistr a b) -> DSDistr a b

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

data Path e (a :: MarginalT) (b :: MarginalT) where
  PNil :: Path e a a
  PSnoc :: e b c -> Path e a b -> Path e a c

data PathTo e b where
  PathTo :: !(Path e a b) -> PathTo e b

data RefNodeTo b where
  RefNodeTo :: !(IORef (Node a b)) -> RefNodeTo b

data RefNodeFrom a where
  RefNodeFrom :: !(IORef (Node a b)) -> RefNodeFrom a

instance Eq (RefNodeFrom a) where
  RefNodeFrom x == RefNodeFrom y = x == unsafeCoerce y

instance Eq (RefNodeTo a) where
  RefNodeTo x == RefNodeTo y = x == unsafeCoerce y

-- initialize without parent node
constant' :: MDistr a -> RootNode a
constant' d = RootNode d (NMarginalized Nothing [])

-- mModifyChildren :: ([InitTree a] -> [InitTree a]) -> MPath -> Node' x -> Node' x
-- mModifyChildren f Root (NMarginalized c t) = NMarginalized c (f t)
-- mModifyChildren f (Child p) (NMarginalized (Just (cdistr, c)) t) = NMarginalized (Just (cdistr, mModifyChildren f p c)) t

-- modifyChildren :: ([InitTree a] -> [InitTree a]) -> AnyPath -> Node' x -> Node' x
-- modifyChildren f (AMPath p) = mModifyChildren f p

-- -- initialize with parent node
-- newConditional' :: AnyPath -> CDistr a b -> Node' x -> (Node' x, AnyPath)
-- newConditional' par cdistr = do
--   childRef <- newIORef child
--   modifyIORef' par $ \n -> n { children = (RefNodeFrom childRef) : children n }
--   --childRef' <- mkWeakIORef childRef $ putStrLn ("deleted " ++ str)
--   pure childRef
--   where
--   child = Node
--     { name = str
--     , children = []
--     , state = Initialized
--     , distr = CDistr par cdistr }
