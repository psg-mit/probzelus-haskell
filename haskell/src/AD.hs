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
import Util.ZStream (ZStream)
import qualified Util.ZStream as Z

deriv :: d ~ Double => Exp Int d -> Seq d -> Seq d
deriv e = grad (\xs -> eval (xs `Seq.index`) (fmap auto e))


ascend :: Floating d => d -> (Seq d -> Seq d) -> Int -> Seq d -> Seq d
ascend lr diff = go where
  go 0 x = x
  go n x = let d = diff x in go (n - 1) (Seq.zipWith (+) x (fmap (lr *) d))

-- find a *local* MAP via gradient descent
runMAP1 :: Monad m => PProg Int a -> StateT (Int, Exp Int Double, Seq Double) m a
runMAP1 = go where
  go :: Monad m => PProg Int a -> StateT (Int, Exp Int Double, Seq Double) m a
  go (Ret y) = pure y
  go (SampleThen d f) = do
    (nvars, e, x) <- get
    -- In reality, we should make sure that we start with a value that's in the support,
    -- rather than blindly picking 0.2 here. I just picked that because it works for
    -- both Gaussian and Beta.
    put (nvars + 1, e + d (Var nvars), (x Seq.:|> 0.2))
    go (f (Var nvars))
  go (FactorThen ll p') = do
    modify (\(nvars, e, x) -> (nvars, e + ll, x))
    go p'

ascendStep :: Monad m => Double -> Int -> StateT (Int, Exp Int Double, Seq Double) m ()
ascendStep lr numIters = modify $ \(nvars, e, x) ->
  let x' = ascend lr (deriv e) numIters x in (nvars, e, x')

evalDist :: Monad m => Exp Int Double -> StateT (Int, Exp Int Double, Seq Double) m Double
evalDist y = do
  (_, _, x) <- get
  pure $ eval (x `Seq.index`) y

runMAP :: Monad m => Double -> Int -> MStream (PProg Int) (Exp Int Double) a -> MStream m Double a
runMAP lr numIters = apState . M.mapyieldM f . M.liftM runMAP1 where
  apState :: Monad m => MStream (StateT (Int, Exp Int Double, Seq Double) m) y a -> MStream m y a
  apState = fmap snd . M.runState (0, 0, Seq.empty)
  f :: Monad m => Exp Int Double -> StateT (Int, Exp Int Double, Seq Double) m Double
  f y = do
    ascendStep lr numIters
    evalDist y

runZ :: Monad m => Double -> Int -> ZStream (PProg Int) a b -> ZStream m a b
runZ lr numIters = Z.runState (0, 0, Seq.empty) . Z.liftM f where
  f :: Monad m => PProg Int a -> StateT (Int, Exp Int Double, Seq Double) m a
  f x = do
    y <- runMAP1 x
    ascendStep lr numIters
    pure y


runModel :: MStream (PProg Int) (Exp Int Double) a -> IO ()
runModel = void . runStream print . runMAP 1e-4 100