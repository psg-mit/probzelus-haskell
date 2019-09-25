{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.DelayedSamplingAbstract where

import GHC.TypeNats (KnownNat)
import Numeric.LinearAlgebra.Static (L, R, Sym, konst, sym, eye)

import Control.Monad.Bayes.Sampler (SamplerIO)
import Util.MStream (MStream)
import qualified Util.MStream as MS

import qualified Distributions as D
import DSProg
import SymbolicDistr

kalman :: Bool -> Double -> Double -> [Double] -> MStream M (Expr Double) (Expr Double)
kalman forgetb m b yobs = do
  x1 <- MS.lift $ sample (normal 0 1)
  MS.lift $ observe (normal x1 1) (head yobs)
  MS.yield x1
  loop 2 x1 (tail yobs)
  where
  forgett x = if forgetb then MS.lift (forgetE x) else pure ()
  loop :: Integer -> Expr Double -> [Double] -> MStream M (Expr Double) (Expr Double)
  loop t xpredt [] = pure xpredt
  loop t xpredt (y : ys) = do
    xt <- MS.lift $ sample (normal (Const m * xpredt + Const b) 1)
    MS.lift $ observe (normal xt 1) y
    MS.yield xt
    forgett xpredt
    loop (t + 1) xt ys

exampleKalman :: SamplerIO (Double, Maybe (Result Double))
exampleKalman = evalM (justRun (kalman True 1 1 [1..]))

kalmanMV :: forall n. KnownNat n => Bool -> L n n -> R n -> [R n] -> MStream M (Expr (R n)) (Expr (R n))
kalmanMV forgetb m b yobs = do
  x1 <- MS.lift $ sample (mvNormal (Const (konst 0)) (sym eye))
  MS.lift $ observe (mvNormal x1 (sym eye)) (head yobs)
  MS.yield x1
  loop 2 x1 (tail yobs)
  where
  forgett x = if forgetb then MS.lift (forgetE x) else pure ()
  loop :: Integer -> Expr (R n) -> [R n] -> MStream M (Expr (R n)) (Expr (R n))
  loop t xpredt [] = pure xpredt
  loop t xpredt (y : ys) = do
    xt <- MS.lift $ sample (mvNormal (MVMul (Const m) xpredt + Const b) (sym eye))
    MS.lift $ observe (mvNormal xt (sym eye)) y
    MS.yield xt
    forgett xpredt
    loop (t + 1) xt ys

exampleKalmanMV :: SamplerIO (Double, Maybe (Result (R 3)))
exampleKalmanMV = evalM (justRun (kalmanMV True eye (konst 1 :: R 3) (map konst [1..])))

betaBernoulli :: Double -> Double -> [Bool] -> MStream M (Expr Double) (Expr Double)
betaBernoulli a b xs = do
  p <- MS.lift $ sample (betaSD a b)
  loop p xs
  where
  loop :: Expr Double -> [Bool] -> MStream M (Expr Double) (Expr Double)
  loop p [] = pure p
  loop p (y : ys) = do
    MS.lift $ observe (bernoulliSD p) y
    MS.yield p
    loop p ys

exampleBetaBernoulli :: SamplerIO (Double, Maybe (Result Double))
exampleBetaBernoulli = evalM (justRun (betaBernoulli 1 1 (cycle [True,  True, False])))

yieldValue :: Expr p -> MStream M (Expr p) ()
yieldValue e = do
  x <- MS.lift $ force e
  MS.yield (Const x)

delay_triplet :: Double -> MStream M (Expr Double) (Expr Double)
delay_triplet zobs = do
  x <- MS.lift $ sample (normal 0 1)
  y <- MS.lift $ sample (normal x 1)
  MS.lift $ observe (normal y 1) zobs
  MS.yield y
  MS.yield x -- XXX: UNSOUND! NEED TO FIX GET_MARGINAL to force here!!!
  yieldValue y
  yieldValue x
  pure x

delay_iid :: [Double] -> MStream M (Expr Double) (Expr Double)
delay_iid yobs = do
  x <- MS.lift $ sample (normal 0 1)
  loop x yobs
  where
  loop x [] = yieldValue x >> pure x
  loop x (y : ys) = do
    MS.lift $ observe (normal x 1) y
    MS.yield x
    loop x ys

louis :: M ()
louis = do
  b <- sample $ bernoulliSD (Const 0.2)
  o <- sample $ normal (Const 0.2) 1
  b' <- force b
  if b' then D.factor (-0.1) else pure ()