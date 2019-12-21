ProbZelus-haskell
=================

A Haskell implementation of the ProbZelus probabilistic programming
language for streaming inference.

# Building

Dependencies:
- Haskell with Stack package manager
- (For target-tracker visualization:) Java with Maven package manager

All commands are relative to this base directory.

Build ProbZelus-haskell:
```
cd haskell/ ; stack build
```

## Target-tracking example

To build the visualization for the multi-target tracker:
```
cd java/ ; mvn compile
```

To view streaming results from the multi-target tracker:

```
(cd haskell/ ; stack exec examples mtt) | (cd java/ ; mvn exec:java -Dexec.mainClass=glimpsetracks.App)
```
