

{- Assumed density filtering using symbolic arithmetic
-}
module ADF where

import Control.Monad (replicateM, forM, void, (>=>))
import Control.Monad.Bayes.Class hiding (factor)
import Control.Monad.Bayes.Sampler
import Control.Monad.State
import Control.Monad.Trans (liftIO)

import Data.Maybe (catMaybes, maybeToList, fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Debug.Trace

import Numeric.Log (Log (Exp))

import Distributions hiding (factor)
import Util.Numeric
import Util.MStream hiding (Ret)
import qualified Util.MStream as M
import SymbolicArithmetic

data PProg a where
  Ret :: a -> PProg a
  FactorThen :: Exp Int Double -> PProg a -> PProg a
  -- YieldThen :: Exp Int Double -> PProg p a -> PProg p a
  SampleThen :: ExpFam a -> [Double] -> (Exp Int a -> PProg b) -> PProg b

factor :: Exp Int Double -> PProg ()
factor w = FactorThen w $ Ret ()

sample' :: ExpFam a -> [Double] -> PProg (Exp Int a)
sample' ef naturalParams = SampleThen ef naturalParams Ret

instance Functor PProg where
  fmap f (Ret x) = Ret (f x)
  fmap f (FactorThen w k) = FactorThen w (fmap f k)
  fmap f (SampleThen ef naturalParams k) = SampleThen ef naturalParams (fmap f . k)

instance Applicative PProg where
  pure = Ret
  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')

instance Monad PProg where
  Ret x >>= f = f x
  FactorThen w k >>= f = FactorThen w (k >>= f)
  SampleThen ef naturalParams k >>= f = SampleThen ef naturalParams (\x -> k x >>= f)

getDist :: Seq (Some ExpFam, [Double]) -> Exp Int Double -> Maybe String
getDist env p = do
  i <- getVar p
  (Some ef, naturalParams) <- Seq.lookup i env
  return (efDescribe ef naturalParams)

-- Compute a "variable representation":
-- Decompose a product, represent as a list of expressions, into
-- Maybe a constant and a Map from each variable in the expression
-- to a piece of the product involving only that variable.
-- This will fail for expressions that cannot be decomposed in this
-- way, such as
-- log (Var i + Var j)
variableRepresentation :: (Ord env, Num a, Eq a, Floating a) => [Exp env a] -> Maybe (Maybe a, Map env (Exp () a))
variableRepresentation = fmap (foldr combine (Nothing, M.empty)) . sequence . map f
  where
  combine (mx, map1) (my, map2) =
    (foldMaybe (*) mx my, M.unionWith (*) map1 map2)
  f e = case Set.toList (allVars e) of
    [] -> Just (getConstant e, M.empty)
    [i] -> Just (Nothing, M.singleton i (mapVars (\_ -> ()) e))
    _ -> Nothing

fromVariableRepresentation :: (Maybe a, Map env (Exp () a)) -> [Exp env a]
fromVariableRepresentation (mx, mp) =
  maybeToList (Const <$> mx) ++ [ mapVars (\() -> i) e | (i, e) <- M.toList mp]

-- Given a variable representation of a log-likelihood to fold in to the posterior,
-- compute the updates needed to the environment of natural parameters for
-- exponential families.
-- This can fail if the variable representation has more than one variable in it
-- (e.g., `Var i * Var j`), or if the sufficient statistics implied by the
-- expression are not captured by the exponential family for the variable,
-- e.g., `Var i ^ 3` where `i` is from a Normal exponential family.
analyticUpdate :: (Maybe Double, Map Int (Exp () Double)) -> Seq (Some ExpFam, [Double]) -> Maybe (Seq (Some ExpFam, [Double]))
analyticUpdate (mx, mp) = case M.toList mp of
  [] -> Just . id
  [(i, e)] -> \env -> do
    (Some ef, naturalParams) <- Seq.lookup i env
    naturalParams' <- update e (+ fromMaybe 1 mx) $ zip (suffStats ef) naturalParams
    return $ Seq.update i (Some ef, naturalParams') env
  (i : j : xs) -> \_ -> Nothing
  where
  update :: Eq a => a -> (b -> b) -> [(a, b)] -> Maybe [b]
  update x f [] = Nothing
  update x f ((a, b) : ys) = if x == a
    then Just (f b : map snd ys)
    else (b :) <$> update x f ys

-- If an analytic update is possible, do that (see `analyticUpdate`).
-- Otherwise, we compute the closest member of the exponential families
-- to the posterior, in terms of KL divergence (see `mcUpdate`).
update :: MonadSample m  => Seq (Some ExpFam, [Double]) -> Exp Int Double -> m (Seq (Some ExpFam, [Double]))
update env e = case analytic of
  Just env' -> return env'
  Nothing -> mcUpdate env e
  where
  analytic = do
    exps <- sequence . map variableRepresentation $ getSumOfProd e
    foldr (>=>) return (map analyticUpdate exps) env

-- We compute the closest member of the exponential families
-- to the posterior, in terms of KL divergence. This corresponds to
-- a maximum likelihood estimate. We perform it by
-- estimating the mean of the sufficient statistics using
-- naive importance sampling, and then moment-matching to find the
-- natural parameters that produce those expected sufficient statistics.
mcUpdate :: MonadSample m => Seq (Some ExpFam, [Double]) -> Exp Int Double -> m (Seq (Some ExpFam, [Double]))
mcUpdate env e = do
  allSuffStats <- replicateM mcIterations suffStatsComp
  let suffStats' = weightedAvg $ exponentiateWeights [ (x, Numeric.Log.Exp w) | (w, x) <- allSuffStats ]
  --traceShow allSuffStats $
  return $ foldr (.) id [ Seq.update i (Some ef, mle ef stats) | ((i, (Some ef, _)), stats) <- zip efs suffStats' ] env
  where
  weightedAvg = weightedAverageGeneric (\k -> map (map (* k))) (repeat (repeat 0)) (zipWith (zipWith (+)))
  vars = allVars e
  mcIterations = 100
  efs = catMaybes [ (,) i <$> Seq.lookup i env | i <- Set.toList vars ]
  suffStatsComp = do
    vals <- forM efs $ \(i, (Some ef, naturalParams)) -> do
      val <- efSample ef naturalParams
      let val' = efRepr ef val
      return (val', map (eval (\_ -> val')) (suffStats ef))
    let (values, suffStatsvs) = unzip vals
    return $ (eval ((values !!)) e, suffStatsvs)


run' :: MonadSample m => PProg a -> StateT (Seq (Some ExpFam, [Double])) m a
run' (Ret x) = return x
run' (SampleThen ef naturalParams f) = do
  env <- get
  let env' = env Seq.:|> (Some ef, naturalParams)
  put env'
  run' (f (Var (length env)))
-- run' env (YieldThen p x) = do
--   yield (getDist env p)
--   run' env x
run' (FactorThen ll x) = do
  env <- get
  env' <- update env ll
  put env'
  run' x

gaussianGaussianModel :: MStream PProg (Exp Int Double) a
gaussianGaussianModel = do
  mu <- M.lift $ sample' normalEF (normalNatParams 0 100)
  step mu
  where
  step mu = do
    yield mu
    M.lift $ factor (gaussian_ll (2 * mu) 9 3.5)
    step mu

betaBernoulliModel :: MStream PProg (Exp Int Double) a
betaBernoulliModel = do
  p <- M.lift $ sample' betaEF [1,1]
  step True p
  where
  step b p = do
    yield p
    M.lift $ factor $ bernoulli_ll p (if b then 1 else 0)
    step (not b) p

-- onlineLogisticRegression ::

run :: MonadSample m => MStream PProg (Exp Int Double) a -> MStream m (Maybe String) a
run = undefined -- run' Seq.empty

runModel :: MStream PProg (Exp Int Double) a -> IO ()
runModel = void . sampleIO . M.runStream (liftIO . putStrLn . fromMaybe "Nothing") . run