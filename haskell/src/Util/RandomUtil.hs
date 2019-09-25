module Util.RandomUtil where

import Control.Monad.State (StateT)
import Control.Monad.Writer.CPS (WriterT, lift)
import Control.Monad.Bayes.Class
import Data.Random (RVarT, stdUniformT, normalT, uniformT)
import Data.Random.List (shuffleT)
import Data.Random.Distribution.Bernoulli (bernoulliT)
import Data.Random.Distribution.Beta (betaT)
import Data.Random.Distribution.Categorical (categoricalT)
import Data.Random.Distribution.Gamma (gammaT)

import Data.Random.Internal.Source (MonadRandom, getRandomPrim)

import qualified Data.Vector.Generic as VG

instance MonadRandom m => MonadRandom (StateT s m) where
  getRandomPrim p = lift (getRandomPrim p)

instance MonadRandom m => MonadRandom (WriterT w m) where
  getRandomPrim p = lift (getRandomPrim p)

instance MonadSample (RVarT m) where
  random = stdUniformT
  shuffle = fmap VG.fromList . shuffleT . VG.toList

  uniform a b = uniformT a b
  normal m s = normalT m s
  gamma shape scale = gammaT shape scale
  beta a b = betaT a b

  bernoulli p = bernoulliT p
  categorical ps = categoricalT (zip (VG.toList ps) [0..])
  -- geometric p = ?