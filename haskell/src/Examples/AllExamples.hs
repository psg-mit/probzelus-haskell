module Examples.AllExamples where

import Control.Monad.Bayes.Sampler (sampleIO)

import AD
import ADF
import PProg
import SymbolicLL

allExamples :: [IO ()]
allExamples =
  [ sampleIO (algorithm model)
    | algorithm <- [ PProg.runOneWeightedSample
                   , PProg.runParticleFilter
                   , PProg.runImportanceSampling]
    , model <- [ PProg.betaBernoulliModel
               , PProg.gaussianGaussianModel ]
  ]
  ++
  [ algorithm model
    | algorithm <- [ SymbolicLL.runModel
                   , AD.runModel]
    , model <- [ SymbolicLL.betaBernoulliModel
               , SymbolicLL.gaussianGaussianModel ]
  ]
  ++
  [ ADF.runModel ADF.betaBernoulliModel
  , ADF.runModel ADF.gaussianGaussianModel ]