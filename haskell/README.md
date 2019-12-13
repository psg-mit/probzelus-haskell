# rppl

## Running

To play with the Haskell reactive PPL library,
in a terminal from this directory, run
```
stack repl
```

Then, at the GHCi prompt, you can run a program such as
```
AD.runModel SymbolicLL.gaussianGaussianModel
```

A full list of simple runnable examples can be found in the definition
`allExamples` in the file `src/AllExamples.hs`.

## Inference algorithms

Four modules implement several different inference algorithms,
where the notions of what exactly is a probabilistic program may vary
slightly for each different inference algorithm.
Except where noted, each file defines its own notion of a
probabilistic program (`PProg`).

- `PProg.hs` - the "standard" black-box setting.
  Inference algorithms: particle filter (`runParticleFilter`),
  importance sampler for computing Monte Carlo estimates
  of running expectations (`runImportanceSampling`),
  run a single weighted sample (`runOneWeightedSample`)
- `SymbolicLL.hs` - compute a running log PDF of all sampled variables.
  If we return an expression that is simply a sampled variable, we try to
  see if the log PDF factors to tell us that our posterior on that variable
  is an exponential family, and then give the parameters of the exponential
  family in that case (`runModel`)
- `ADF.hs` - Assumed Density Filtering (ADF) - All sampled variables must
  come from exponential families. If a factor statement would yield an
  analytic update to the exponential families that it depends on, then that
  analytic update is performed. Otherwise, we use importance sampling to
  compute a Monte Carlo estimate of the means of the sufficient statistics
  for the relevant exponential families, and then update the natural parameters
  for those exponential families by moment-matching (`runModel`).
- `AD.hs` - Use automatic differentiation to compute the local
  maximum a posteriori (MAP) estimate of the result. As new sample/factor
  statements are added to the model, we update the old result, hoping that
  the local MAP "doesn't change much" with small changes to the program
  (`runModel`).
  Uses the `PProg` definition from `SymbolicLL`, currently.

## Multi-target tracking example

Need to make this portable.

```
stack exec examples mtt | ../java/run.sh App
```
