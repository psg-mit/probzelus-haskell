{-# LANGUAGE LambdaCase #-}

module SymbolicLL where

import Prelude
import Data.Foldable (asum)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.IORef

import Control.Arrow (second)
import Control.Monad ((>=>), void)
import Control.Monad.State

import Debug.Trace (traceShow, trace)

import Distributions hiding (factor)
import Util.MStream hiding (Ret)
import qualified Util.MStream as M
import Util.ZStream (ZStream)
import qualified Util.ZStream as Z
import SymbolicArithmetic

data PProg v a where
  Ret :: a -> PProg v a
  FactorThen :: Exp v Double -> PProg v a -> PProg v a
  SampleThen :: (Exp v a -> Exp v Double) -> (Exp v a -> PProg v b) -> PProg v b

instance Functor (PProg v) where
  fmap f (Ret x) = Ret (f x)
  fmap f (FactorThen w k) = FactorThen w (fmap f k)
  fmap f (SampleThen d k) = SampleThen d (fmap f . k)

instance Applicative (PProg v) where
  pure = Ret
  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')

instance Monad (PProg v) where
  Ret x >>= f = f x
  FactorThen w k >>= f = FactorThen w (k >>= f)
  SampleThen d k >>= f = SampleThen d (\x -> k x >>= f)

factor :: Exp v Double -> PProg v ()
factor w = FactorThen w $ Ret ()

sample' :: (Exp v a -> Exp v Double) -> PProg v (Exp v a)
sample' s = SampleThen s (\x -> Ret x)

getVarFromProduct' :: (Num a, Eq env) => env -> [Exp env a] -> Maybe (Exp env a, [Exp env a])
getVarFromProduct' i es = case Data.List.partition (varIn i) es of
  ([], _) -> Nothing
  (is, es') -> Just (prod1 is, es')

getVarFromProduct :: (Eq env, Eq a, Num a, Show env, Show a) => env -> [Exp env a] -> Either Bool (Exp env a, [Exp env a])
--getVarFromProduct i es| traceShow ("getVarFromProduct", i, es) False = undefined
getVarFromProduct i es = case getVarFromProduct' i es of
  Just (e, es') -> if all (Var i `notIn`) es'
    then Right (e, es')
    else Left True
  Nothing -> Left False

getFromList :: Eq a => a -> [(a, k)] -> Maybe (k, [(a, k)])
getFromList i [] = Nothing
getFromList i ((j, x) : xs) = if i == j
  then Just (x, xs)
  else fmap (second ((j, x) :)) (getFromList i xs)

matchPattern :: (Eq a, Show a, Show k) => [(a, k)] -> [a] -> Maybe [[k]]
--matchPattern es xs | traceShow (es, xs) False = undefined
matchPattern [] [] = Just []
matchPattern (_ : _) [] = Nothing
matchPattern es (x : xs) =
  let (esx, esnotx) = Data.List.partition (\(k, v) -> k == x) es in
  (map snd esx :) <$> matchPattern esnotx xs

getDist1 :: Int -> [(Exp Int Double, [Exp Int Double])] -> Maybe String
--getDist1 i xs | traceShow ("getDist1", i, xs) False = undefined
getDist1 i xs = asum [ f' . map (sum1 . map prod1) =<< (matchPattern xs (map (mapVars (\_ -> i)) f)) | (f, f') <- fs] where
  fs = map efFunc exponentialFamilies
  efFunc (Some ef) = (suffStats ef, fmap (efDescribe ef) . mapM getConstant)

-- at :: Int -> [a] -> Maybe a
-- at _ []       = Nothing
-- at 0 (x : _)  = Just x
-- at i (_ : xs) = lookup (i - 1) xs

getDist :: Exp Int Double -> Exp Int Double -> Maybe String
--getDist e i | traceShow ("getDist", e, i) False = undefined
getDist e i = do
  i' <- getVar i
  let e' = getSumOfProd e
  xs <- getRelevants (map (getVarFromProduct i') e')
  getDist1 i' xs
  where
  getRelevants [] = Just []
  getRelevants (x : xs) = case x of
    Left False -> getRelevants xs
    Left True -> Nothing
    Right y -> fmap (y :) (getRelevants xs)

run'' :: Monad m => PProg Int a -> StateT (Int, Exp Int Double) m a
run'' (Ret x) = pure x
run'' (SampleThen d f) = do
  (nvars, e) <- get
  put (nvars + 1, e + d (Var nvars))
  run'' (f (Var nvars))
run'' (FactorThen ll x) = do
  modify (second (+ ll))
  run'' x

run :: Monad m => MStream (PProg Int) (Exp Int Double) a -> MStream m (Maybe String) a
run = apState . M.mapyieldM evalDist . M.liftM run'' where
  apState :: Monad m => MStream (StateT (Int, Exp Int Double) m) (Maybe String) a -> MStream m (Maybe String) a
  apState = fmap snd . M.runState (0, 0)
  evalDist :: Monad m => Exp Int Double -> StateT (Int, Exp Int Double) m (Maybe String)
  evalDist y = do
    (_, e) <- get
    pure (getDist e y)

runZ :: Monad m => ZStream (PProg Int) a b -> ZStream m a b
runZ = Z.runState (0, 0) . Z.liftM run''

betaBernoulliModel :: MStream (PProg v) (Exp v Double) a
betaBernoulliModel = do
  p <- M.lift $ sample' (beta_ll 1 1)
  step True p
  where
  step b p = do
    yield p
    M.lift $ factor (bernoulli_ll p (if b then 1 else 0))
    step (not b) p

gaussianGaussianModel :: MStream (PProg v) (Exp v Double) a
gaussianGaussianModel = do
  mu <- M.lift $ sample' (gaussian_ll 0 100)
  step mu
  where
  step mu = do
    yield mu
    M.lift $ factor (gaussian_ll (2 * mu) 1 3.5)
    step mu


runModel :: MStream (PProg Int) (Exp Int Double) a -> IO ()
runModel = void . runStream (putStrLn . fromMaybe "Nothing") . run