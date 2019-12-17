import Control.Monad (void)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Trans (liftIO)
import System.Environment (getArgs)
import DSProg
import Examples.DelayedSamplingAbstract (kalman)
import DelayedSampling (delay_kalman)
import qualified Examples.MultiTargetTracking as MTT
import qualified Examples.SingleTargetTracking as STT
import qualified ADF as ADF
import qualified Examples.Walker as Walker

import Data.Aeson (ToJSON, encode)
import qualified Util.ZStream as ZS
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)


nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth i (_ : xs) = nth (i - 1) xs

runMTTExample :: Int -> IO ()
runMTTExample n = sampleIO $ ZS.runStream (liftIO . BS.putStrLn . encode) (MTT.runMTTPF n)

main :: IO ()
main = do
  args <- getArgs
  case nth 0 args of
    Just "mtt" -> runMTTExample (maybe 100 Prelude.read (nth 1 args))
    Just "stt" -> STT.runExample (maybe True Prelude.read (nth 1 args)) (maybe 100 Prelude.read (nth 2 args))
    Just "gaussiangaussian" -> ADF.runModel ADF.gaussianGaussianModel
    Just "walker" -> Walker.run
    _ -> void $ do
      let highLevel = maybe True Prelude.read (nth 0 args)
      let forgetb = maybe True Prelude.read (nth 1 args)
      let kalmanf = if highLevel
            then \forg m b xs -> void $ justRun (kalman forg m b xs)
            else delay_kalman
      sampleIO $ evalM (kalmanf forgetb 1 1 [1..])