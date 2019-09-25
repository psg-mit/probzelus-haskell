import Control.Monad (void)
import Control.Monad.Bayes.Sampler (sampleIO)
import System.Environment (getArgs)
import DSProg
import Examples.DelayedSamplingAbstract (kalman)
import DelayedSampling (delay_kalman)
import qualified Examples.MultiTargetTracking as MTT
import qualified Examples.SingleTargetTracking as STT

nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth i (_ : xs) = nth (i - 1) xs

main :: IO ()
main = do
  args <- getArgs
  case nth 0 args of
    Just "mtt" -> MTT.runExample (maybe True Prelude.read (nth 1 args)) (maybe 100 Prelude.read (nth 2 args))
    Just "stt" -> STT.runExample (maybe True Prelude.read (nth 1 args)) (maybe 100 Prelude.read (nth 2 args))
    _ -> void $ do
      let highLevel = maybe True Prelude.read (nth 0 args)
      let forgetb = maybe True Prelude.read (nth 1 args)
      let kalmanf = if highLevel
            then \forg m b xs -> void $ justRun (kalman forg m b xs)
            else delay_kalman
      sampleIO $ evalM (kalmanf forgetb 1 1 [1..])