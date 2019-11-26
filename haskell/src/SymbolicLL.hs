

module SymbolicLL where

import Data.Foldable (asum)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.IORef

import Control.Arrow (second)
import Control.Monad ((>=>), void)

import Debug.Trace (traceShow, trace)

import Distributions
import Util.Stream
import SymbolicArithmetic

data PProg a where
  Ret :: PProg a
  FactorThen :: Exp Int Double -> PProg a -> PProg a
  YieldThen :: Exp Int Double -> PProg a -> PProg a
  SampleThen :: (Exp Int a -> Exp Int Double) -> (Exp Int Double -> PProg a) -> PProg a

data PProg' a where
  Ret' :: PProg' a
  FactorThen' :: Exp Int Double -> PProg' a -> PProg' a
  SampleThen' :: (Exp Int a -> Exp Int Double) -> (Exp Int Double -> PProg' a) -> PProg' a

newtype SPProg a b = SPProg (a -> PProg' b)

factor :: Exp Int Double -> PProg ()
factor w = FactorThen w Ret

pyield :: Exp Int Double -> PProg ()
pyield p = YieldThen p Ret

sample' :: (Exp Int a -> Exp Int Double) -> PProg a
sample' s = SampleThen s (\_ -> Ret)

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

run' :: Monad m => Int -> Exp Int Double -> PProg a -> MStream m (Maybe String) (Exp Int Double)
run' nvars e Ret = return e
run' nvars e (SampleThen d f) = run' (nvars + 1) (e + d (Var nvars)) (f (Var nvars))
run' nvars e (YieldThen p x) = do
  yield (getDist e p)
  run' nvars e x
run' nvars e (FactorThen ll x) = run' nvars (e + ll) x

run :: Monad m => PProg a -> MStream m (Maybe String) (Exp Int Double)
run = run' 0 0

betaBernoulliModel :: PProg Double
betaBernoulliModel =
  SampleThen (beta_ll 1 1) $ step True
  where
  step b p =
    YieldThen p $
    FactorThen (bernoulli_ll p (if b then 1 else 0)) $
    step (not b) p

gaussianGaussianModel :: PProg Double
gaussianGaussianModel =
  SampleThen (gaussian_ll 0 100) $ step
  where
  step mu =
    YieldThen mu $
    FactorThen (gaussian_ll (2 * mu) 1 3.5) $
    step mu

runModel :: PProg Double -> IO ()
runModel = void . runMStream (putStrLn . fromMaybe "Nothing") . run