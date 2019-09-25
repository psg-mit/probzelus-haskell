module AD where

import Control.Monad (void)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Numeric.AD

import SymbolicArithmetic (Exp (Var), eval)
import SymbolicLL
import Util.MStream hiding (Ret)

deriv :: d ~ Double => Exp Int d -> Seq d -> Seq d
deriv e = grad (\xs -> eval (xs `Seq.index`) (fmap auto e))


ascend :: Floating d => d -> (Seq d -> Seq d) -> Int -> Seq d -> Seq d
ascend lr diff = go where
  go 0 x = x
  go n x = let d = diff x in go (n - 1) (Seq.zipWith (+) x (fmap (lr *) d))

-- find a *local* MAP via gradient descent
runMAP' :: Monad m => Double -> Int -> Int -> Exp Int Double -> Seq Double -> PProg p a -> MStream m Double ()
runMAP' lr numIters = go where
  go nvars e x Ret = pure ()
  go nvars e x (SampleThen d f) =
    -- In reality, we should make sure that we start with a value that's in the support,
    -- rather than blindly picking 0.2 here. I just picked that because it works for
    -- both Gaussian and Beta.
    go (nvars + 1) (e + d (Var nvars)) (x Seq.:|> 0.2) (f (Var nvars))
  go nvars e x (YieldThen p p') =
    let x' = ascend lr (deriv e) numIters x in do
    yield (eval (x' `Seq.index`) p)
    go nvars e x' p'
  go nvars e x (FactorThen ll p') = go nvars (e + ll) x p'

runMAP :: Monad m => Double -> Int -> PProg p a -> MStream m Double ()
runMAP lr numIters = runMAP' lr numIters 0 0 Seq.empty

runModel :: PProg p a -> IO ()
runModel = void . runStream print . runMAP 1e-4 100