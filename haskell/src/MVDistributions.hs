{-# LANGUAGE ExistentialQuantification,
             DataKinds #-}
module MVDistributions
where

import Prelude hiding ((<>))

import Control.Arrow (first)
import Control.Monad (replicateM, zipWithM)
import Control.Monad.Bayes.Class (MonadSample, normal, shuffle)
import Data.Aeson (ToJSON (toJSON))
import Data.List (permutations, sortBy, maximumBy)
import Data.Ord (comparing, Down (Down))
import qualified Data.Vector as V
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra.Data as LAD
import Numeric.SpecFunctions (logGamma)

import Distributions (Distr (..))

instance KnownNat n => ToJSON (R n) where
  toJSON x = toJSON (extract x :: LAD.Vector Double)

instance (KnownNat m, KnownNat n) => ToJSON (L m n) where
  toJSON x = toJSON (LAD.toLists (extract x :: LAD.Matrix Double))

instance KnownNat n => ToJSON (Sym n) where
  toJSON x = toJSON (unSym x)

eyeSym :: KnownNat n => Sym n
eyeSym = sym eye

sampleNormalDist :: forall n m. MonadSample m => KnownNat n => R n -> Sym n -> m (R n)
sampleNormalDist mu sig = do
  xs <- replicateM (fromInteger (natVal (Proxy :: Proxy n))) (normal 0 1)
  let x = vector xs
  return (mu + chol sig #> x)

log2pi :: Double
log2pi = log (2 * pi)

mvNormal_ll0 :: forall n. KnownNat n => Sym n -> R n -> Double
mvNormal_ll0 s x = -1/2 * (x <.> (sinv #> x) + lndets + fromInteger n * log2pi)
  where
  (sinv, (lndets, _)) = invlndet (unSym s)
  sinv' :: Sym n
  sinv' = sym sinv
  n = natVal (Proxy :: Proxy n)

mvNormal_ll :: KnownNat n => R n -> Sym n -> R n -> Double
mvNormal_ll mu s x = mvNormal_ll0 s (x - mu)

mvNormal :: KnownNat n => R n -> Sym n -> Distr (R n)
mvNormal mu s = Distr (sampleNormalDist mu s) (mvNormal_ll mu s)

uniformNSphere :: forall n. KnownNat n => Distr (R n)
uniformNSphere = Distr (do { x <- sampleNormalDist 0 (sym eye); pure (konst (1 / (norm_2 x)) * x) })
  (\_ -> - logNSphereVolume (natVal (Proxy :: Proxy n)))

-- https://en.wikipedia.org/wiki/N-sphere#Closed_forms
logNSphereVolume :: Integer -> Double
logNSphereVolume n = log (fromIntegral n) + fromIntegral n / 2 * log pi
  - logGamma (fromIntegral n / 2 + 1)


conjugate :: (KnownNat m, KnownNat n) => L m n -> Sym n -> Sym m
conjugate f p = sym (f <> unSym p <> tr f)

conjugate' :: (KnownNat m, KnownNat n) => L n m -> Sym n -> Sym m
conjugate' f p = sym (tr f <> unSym p <> f)

kalmanPredict :: KnownNat n => Sq n -> Sym n -> (R n, Sym n) -> (R n, Sym n)
kalmanPredict f q (x, p) = (f #> x, conjugate f p + q)

symEachSide :: KnownNat n => Sym n -> Sym n -> Sym n
symEachSide p x = sym (p' <> unSym x <> p')
  where p' = unSym p

-- The below is not ideal numerically
kalmanUpdate :: forall m n. (KnownNat m, KnownNat n) => R m -> L m n -> Sym m
  -> (R n, Sym n) -> ((R n, Sym n), Double)
kalmanUpdate z h r (x, p) =
  ((x + k #> ytilde, p - symEachSide p (conjugate' h sinv'))
   , loglike)
  where
  ytilde :: R m
  ytilde = z - h #> x
  trh :: L n m
  trh = tr h
  s :: Sym m
  s = r + conjugate h p
  (sinv, (lndets, _)) = invlndet (unSym s)
  sinv' :: Sym m
  sinv' = sym sinv
  k = unSym p <> trh <> sinv
  n = natVal (Proxy :: Proxy n)
  loglike = -1/2 * (ytilde <.> (sinv #> ytilde) + lndets + fromInteger n * log2pi)

shuffleList :: MonadSample m => [a] ->  m [a]
shuffleList xs = V.toList <$> shuffle (V.fromList xs)

multiAssocBySampling :: MonadSample m => (st -> obs -> m (Double, st)) -> [st] -> [obs] -> m (Double, [st])
multiAssocBySampling upd = f where
  f st obs = do
    obs' <- shuffleList obs
    st' <- zipWithM upd st obs'
    let (weights, sts) = unzip st'
    return (sum weights, sts)

printVector :: KnownNat n => R n -> IO ()
printVector x = print (extract x)

scaleSym :: KnownNat n => Double -> Sym n -> Sym n
scaleSym c m = sym $ konst c * unSym m