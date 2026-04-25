{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module CMG where

import Prelude hiding (replicate)
import Control.Arrow (second)
import qualified Control.Category as C
-- import Control.Monad ((<=<))
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

import Data.Dynamic
import Data.Functor.Identity (Identity (..))
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Distributions (Distr)
import qualified Distributions as D

import Numeric.Log (Log (Exp, ln))

newtype Weight = Weight (Log Double)
  deriving (Eq, Show, Ord)

deriving instance Num Weight

instance Semigroup Weight where
  (<>) = (*)

instance Monoid Weight where
  mempty = 1

type Full t = t Identity
type Partial t = t Maybe

data (:*) a b f = f (a f) :* f (b f)
data (:+) a b f = a f :+ b f
data Atom t (f :: * -> *) = Atom t
type Unit = Atom ()

data (:==) a b = Bij
  { to :: forall f. Monad f => a f -> b f
  , from :: forall f. Monad f => b f -> a f
  }

instance C.Category (:==) where
  id = Bij { to = id, from = id }
  g . f = Bij {to = to g . to f, from = from f . from g }

pattern (:*.) :: Full a -> Full b -> Full (a :* b)
pattern (:*.) x y = Identity x :* Identity y


data Gen m g a = Gen
  { simulate :: Full g -> m (Full a)
  , pdf :: Full g -> Full a -> Log Double
  , propose :: forall d. Maybe (Partial a) -> Gen m d g -> Gen m d (g :* a)
  , extend :: forall d. Maybe (Partial a) -> Gen m d g -> Gen m d (g :* a)
  }

-- gid :: Applicative m => Gen m a a
-- gid = Gen
--   { simulate = pure
--   , pdf = \_ _ -> 1
--   , propose = undefined
--   , extend = undefined
--   }

bij_in :: (g :== g') -> Gen m g a -> Gen m g' a
bij_in b gen = Gen
  { simulate = \g' -> simulate gen (from b g')
  , pdf = \g' a -> pdf gen (from b g') a
  , propose = undefined
  , extend = undefined
  }

gcomp :: Monad m => Gen m g a -> Gen m (d :* a) b -> Gen m (g :* d) (a :* b)
gcomp gx gf = Gen
  { simulate = \(g :*. d) -> do
     a <- simulate gx g
     b <- simulate gf (d :*. a)
     pure (a :*. b)
  , pdf = \(g :*. d) (a :*. b) ->
     pdf gx g a * pdf gf (d :*. a) b
  , propose = undefined -- \t -> case t of
    --  Nothing -> simulate (gcomp gx gf)
    --  gcomp (propose gx ta)
    --        (propose gf tb)
  , extend = undefined
  }