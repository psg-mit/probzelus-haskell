module AD where

import Control.Monad (void)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Control.Monad.State

import Numeric.AD

import SymbolicArithmetic (Exp (Var), eval)
import SymbolicLL hiding (run2)
import Util.MStream hiding (Ret)
import qualified Util.MStream as M

deriv :: d ~ Double => Exp Int d -> Seq d -> Seq d
deriv e = grad (\xs -> eval (xs `Seq.index`) (fmap auto e))


ascend :: Floating d => d -> (Seq d -> Seq d) -> Int -> Seq d -> Seq d
ascend lr diff = go where
  go 0 x = x
  go n x = let d = diff x in go (n - 1) (Seq.zipWith (+) x (fmap (lr *) d))

-- find a *local* MAP via gradient descent
runMAP' :: Monad m => Double -> Int -> Int -> Exp Int Double -> Seq Double -> PProg a -> MStream m Double a
runMAP' lr numIters = go where
  go nvars e x (Ret y) = pure y
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

-- find a *local* MAP via gradient descent
runMAP1 :: Monad m => PProg' a -> StateT (Int, Exp Int Double, Seq Double) m a
runMAP1 = go where
  go :: Monad m => PProg' a -> StateT (Int, Exp Int Double, Seq Double) m a
  go (Ret' y) = pure y
  go (SampleThen' d f) = do
    (nvars, e, x) <- get
    -- In reality, we should make sure that we start with a value that's in the support,
    -- rather than blindly picking 0.2 here. I just picked that because it works for
    -- both Gaussian and Beta.
    put (nvars + 1, e + d (Var nvars), (x Seq.:|> 0.2))
    go (f (Var nvars))
  go (FactorThen' ll p') = do
    modify (\(nvars, e, x) -> (nvars, e + ll, x))
    go p'

run2 :: Monad m => Double -> Int -> MStream PProg' (Exp Int Double) a -> MStream m Double a
run2 lr numIters = apState . M.mapyieldM evalDist . M.liftM runMAP1 where
  apState :: Monad m => MStream (StateT (Int, Exp Int Double, Seq Double) m) y a -> MStream m y a
  apState = fmap snd . M.runState (0, 0, Seq.empty)
  evalDist :: Monad m => Exp Int Double -> StateT (Int, Exp Int Double, Seq Double) m Double
  evalDist y = state $ \(nvars, e, x) ->
    let x' = ascend lr (deriv e) numIters x in
    (eval (x' `Seq.index`) y, (nvars, e, x'))

runMAP :: Monad m => Double -> Int -> PProg a -> MStream m Double a
runMAP lr numIters = runMAP' lr numIters 0 0 Seq.empty

runModel :: MStream PProg' (Exp Int Double) a -> IO ()
runModel = void . runStream print . run2 1e-4 100