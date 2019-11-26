{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module MetaGen where

import Prelude hiding (replicate)
import Control.Arrow (second)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

import Data.Dynamic
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Distributions (Distr)
import qualified Distributions as D

import Numeric.Log (Log (Exp, ln))

type Trace = Map String Dynamic

newtype Weight = Weight (Log Double)
  deriving (Eq, Show, Ord)

deriving instance Num Weight

instance Semigroup Weight where
  (<>) = (*)

instance Monoid Weight where
  mempty = 1

-- logpdf_retval_and_trace, propose, and extend all ignore
-- extra/irrelevant trace key-value pairs.
data Gen m a = Gen
  { simulate :: m (a, Trace)
  , logpdf_retval_and_trace :: Trace -> (a, Trace, Log Double)
  , propose :: Trace -> Gen m a
  , extend :: Trace -> Gen m a
  }

logpdf :: Gen m a -> Trace -> Log Double
logpdf model obs = let (_, _, w) = logpdf_retval_and_trace model obs in w

getValue :: Gen m a -> Trace -> a
getValue g t = let (x, _, _) = logpdf_retval_and_trace g t in x

traced :: Monad m => Gen m a -> Gen m Trace
traced m = Gen
  { simulate = (\(x, t) -> (t, t)) <$> simulate m
  , logpdf_retval_and_trace = \obs -> let (x, t, w) = logpdf_retval_and_trace m obs in
      (t, t, w)
  , propose = \obs -> do
      proposedt <- traced (propose m obs)
      let (proposedtproposed, proposedtaux) = split_proposed_and_aux m obs proposedt
      return (M.union proposedtproposed obs)
  , extend = \obs -> do
      proposedt <- traced (extend m obs)
      let (proposedtproposed, proposedtaux) = split_proposed_and_aux m obs proposedt
      return (M.union proposedtproposed obs)
  }

deriving instance Functor m => Functor (Gen m)

instance Monad m => Applicative (Gen m) where
  pure x = Gen
    { simulate = pure (x, M.empty)
    , logpdf_retval_and_trace = \t -> (x, M.empty, 1)
    , propose = \t -> pure x
    , extend = \t -> pure x
    }
  f <*> x = Gen
    { simulate = (\(f', t) (x', t') -> (f' x', M.union t t')) <$> simulate f <*> simulate x
    , logpdf_retval_and_trace = \t ->
        let (f', t1, w1) = logpdf_retval_and_trace f t in
        let (x', t2, w2) = logpdf_retval_and_trace x t in
        (f' x', M.union t1 t2, w1 * w2)
    , propose = \t -> propose f t <*> propose x t
    , extend = \t -> extend f t <*> extend x t
    }

instance Monad m => Monad (Gen m) where
  x >>= f = Gen
    { simulate = do
       (x', t) <- simulate x
       (y, t') <- simulate (f x')
       pure (y, M.union t t')
    , logpdf_retval_and_trace = \t ->
      let (x', tx, wx) = logpdf_retval_and_trace x t in
      let (y, tfx, wfx) = logpdf_retval_and_trace (f x') t in
      (y, M.union tx tfx, wx * wfx)
    , propose = \t -> do
        x' <- propose x t
        propose (f x') t
    , extend = \t -> do
        x' <- extend x t
        extend (f x') t
    }

fromSamplerAndScorer :: Typeable a => Monad m => m a -> (a -> Double) -> Gen m a
fromSamplerAndScorer sampler scorer = g where
  traceFor x = M.singleton "" (toDyn x)
  g = Gen
    { simulate = fmap (\x -> (x, traceFor x)) sampler
    , logpdf_retval_and_trace = \t -> case M.lookup "" t >>= fromDynamic of
        Nothing -> error "primitive not observed"
        Just x -> (x, traceFor x, Exp (scorer x))
    , propose = proposeextend
    , extend = proposeextend
    }
  proposeextend t = case M.lookup "" t >>= fromDynamic of
    Nothing -> g
    Just x -> pure x

prim :: MonadSample m => Typeable a => Distr a -> Gen m a
prim d = fromSamplerAndScorer (D.sample d) (D.score d)

-- instance MonadTrans Gen where
--   lift x = Gen x (lift (lift x))

modifyingKeys :: [(String, String)] -> (Trace -> Trace, Trace -> Trace)
modifyingKeys keys = (to keys M.empty, from keys M.empty) where
  to [] t' t = if M.null t then t' else error "Didn't incorporate all keys"
  to ((match, subtr) : ks) t' t =
    let (matches, dontmatch) = M.partitionWithKey (\k' _ -> and (zipWith (==) match k')) t in
    to ks (M.union t' (M.mapKeys ((subtr ++) . drop (length match)) matches)) dontmatch
  from ks = to (map (\(x, y) -> (y, x)) ks)

at' :: Functor m => (Trace -> Trace, Trace -> Trace) -> Gen m a -> Gen m a
at' maps@(to, from) g = Gen
  { simulate = second to <$> simulate g
  , logpdf_retval_and_trace = \t -> let (x, t', w) = logpdf_retval_and_trace g (to t) in
                      (x, from t', w)
  , propose = \t -> at' maps (propose g (to t))
  , extend = \t -> at' maps (extend g (to t))
  }

(~~) :: Functor m => [(String, String)] -> Gen m a -> Gen m a
ks ~~ g = at' (modifyingKeys ks) g

at :: String -> [(String, String)]
at k = [("", k)]

myGen :: MonadSample m => Gen m (Double, Double, Double)
myGen = do
  x <- at "x" ~~ prim (D.normal 0 5)
  y <- at "y" ~~ prim (D.normal 0 2)
  z <- at "z" ~~ prim (D.normal (x^2 + y^2) 0.1)
  pure (x, y, z)

driftProposal :: MonadSample m => Trace -> Gen m ()
driftProposal t = do
  at "x" ~~ prim (D.normal (t ! "x") 0.2)
  at "y" ~~ prim (D.normal (t ! "y") 0.2)
  return ()

tr :: Typeable a => String -> a -> Trace
tr k v = M.singleton k (toDyn v)

replicate :: Monad m => Int -> Gen m a -> Gen m [a]
replicate n g =
  sequence [ at (show i ++ ".") ~~ g | i <- [0..n-1] ]

trList :: Typeable a => String -> [a] -> Trace
trList k xs = mconcat [ tr (k ++ "." ++ show i) x | (i, x) <- zip [0..] xs]

weighted_sample :: Monad m => Gen m a -> Trace -> Gen m (Trace, Log Double)
weighted_sample model obs = let q = propose model obs in do
  (t, qEstimate) <- propose_with_density_estimate model obs q
  let pPdf = logpdf model (M.union obs t)
  return (t, pPdf / qEstimate)

propose_with_density_estimate :: Monad m => Gen m a -> Trace -> Gen m a -> Gen m (Trace, Log Double)
propose_with_density_estimate p obs q = do
  q_trace <- at "q" ~~ traced q
  let (proposed_trace, aux_trace) = split_proposed_and_aux p obs q_trace
  hEstimate <- if M.null aux_trace
    then return 1
    else let h = extend q (M.mapKeys ("proposed." ++ ) proposed_trace)
         in snd <$> at "h" ~~ weighted_sample h (M.mapKeys ("proposed." ++) aux_trace)
  return (proposed_trace, logpdf q q_trace / hEstimate)

split_proposed_and_aux :: Gen m a -> Trace -> Trace -> (Trace, Trace)
split_proposed_and_aux model obst proposedt =
  let (_, t, _) = logpdf_retval_and_trace model (obst `M.union` proposedt) in
  let onlyProposedt = t `traceMinus` obst in
  (onlyProposedt, proposedt `traceMinus` t)

traceMinus :: Trace -> Trace -> Trace
traceMinus t toRemove = M.withoutKeys t (M.keysSet toRemove)

(!) :: Typeable a => Trace -> String -> a
t ! k = fromMaybe (error "trace value of incorrect type") $ fromDynamic (t M.! k)

rejection :: MonadSample m => Gen m a -> (Trace -> Bool) -> Gen m Trace
rejection p condition = go 0 where
  go i = do
    t <- at ("attempt." ++ show i ++ ".") ~~ traced p
    if condition t
      then at "accepted." ~~ replay t
      else go (i + 1)

exactly :: MonadSample m => Eq a => Typeable a => a -> Gen m a
exactly x = prim (D.dirac x)

exactly' :: MonadSample m => Typeable a => a -> Gen m a
exactly' x = fromSamplerAndScorer (pure x) (error "Can't score")

replay :: MonadSample m => Trace -> Gen m Trace
replay = M.traverseWithKey $ \k v -> at k ~~ exactly' v

importance_resampling :: MonadSample m => Gen m a -> Trace -> Int -> Gen m Trace
importance_resampling model obs n = do
  particles <- at "particles." ~~ replicate n (weighted_sample model obs)
  idx <- at "idx" ~~ prim (D.categorical (map (exp . ln . snd) particles))
  at "inferred." ~~ replay (fst (particles !! idx))


observing :: MonadCond m => Trace -> Gen m a -> m a
observing t g = do
  ((t', ll), _) <- simulate (weighted_sample g t)
  factor ll
  pure (getValue g t')

observing' :: Monad m => Trace -> Gen m a -> Weighted m a
observing' t g = withWeight $ do
  ((t', ll), _) <- simulate (weighted_sample g t)
  pure (getValue g t', ll)