module Examples.SLAM where

import Prelude hiding (Left, Right)
import Util.Stream
import Data.Random
import Data.Random.Distribution.Bernoulli

data Dir = Left | Right deriving Show

type Pos = Integer
max_pos :: Pos
max_pos = 10

type Particle = ((Pos, Dir), (Pos -> (Float, Float)))
type Obs = Bool

mt_prior = log 0.5
mf_prior = log 0.5

macc_likelihood = log 0.9
minacc_likelihood = log 0.1

logsumexp :: Float -> Float -> Float
logsumexp a b =
    let maxexp = max a b in
    let sum = exp (a - maxexp) + exp (b - maxexp) in
    log sum + maxexp

samplex :: (Pos, Dir) -> RVar (Pos, Dir)
samplex (x_prev, dir_prev) =
    let new_dir = if x_prev == max_pos then
            Left
        else
            if x_prev == 0 then
                Right
            else
                dir_prev
    in
    let new_x = case new_dir of
                Right -> (x_prev + 1)
                Left -> (x_prev - 1)
    in do
    wheel_slip <- bernoulli (0.1 :: Float)
    return (if wheel_slip then x_prev else new_x, new_dir)


update :: Particle -> Obs -> Pos -> (Pos -> (Float, Float))
update (a, map) obs x = \x_q -> if x_q == x
    then let (ntrue, nfalse) = map x_q in
         if obs then (ntrue + 1, nfalse) else (ntrue, nfalse + 1)
    else map x_q

update_weight :: Float -> Particle -> Pos -> Obs -> Float
update_weight w_prev (x, map) x_new obs_new =
    let (filtersum_true, filtersum_false) = map x_new in

    --Pr(m[x[t0]], y{t: t < t0 && x[t] == x[t0]} |  x{t: t < t0})
    let mjoint_true = mt_prior + macc_likelihood * filtersum_true + minacc_likelihood * filtersum_false in
    let mjoint_false =  mf_prior + macc_likelihood * filtersum_false + minacc_likelihood * filtersum_true in

    --Pr(y{t: t < t0 && x[t] == x[t0]} |  x{t: t < t0})
    let mmarg = logsumexp mjoint_true mjoint_false in

    --Pr(m[x[t0]] | y{t: t < t0 && x[t] == x[t0]}, x{t: t < t0})
    let mpost_true = mjoint_true - mmarg in
    let mpost_false = mjoint_false - mmarg in

    --Pr(y[t0] | y{t: t < t0 && x[t] == x[t0]}, x{t: t < t0})
    let ypred_true = logsumexp (mpost_true + macc_likelihood) (mpost_false + minacc_likelihood) in
    let ypred_false = logsumexp (mpost_true + minacc_likelihood) (mpost_false + macc_likelihood) in

    if obs_new then
        ypred_true
    else
        ypred_false

initp :: Particle
initp = ((0, Right), \_ -> (0.0, 0.0))

slam_step :: (Particle, Float) -> Obs -> IO (Particle, Float)
slam_step state a = do
    let (particle, w) = state
    let (x, map) = particle
    (x_new, dir_new) <- runRVar (samplex x) StdRandom

    let w' = w + update_weight w particle x_new a
    let new_map = update particle a x_new
    let particle' = ((x_new, dir_new), new_map)


    return (particle', w')

slam :: (Particle, Float) -> MStream IO Obs () -> MStream IO (Particle, Float) (Particle, Float)
slam state = mstreamIterate state slam_step

slamobs :: Monad m => MStream m Obs ()
slamobs = listToMStream [True, False, False]

slamtest :: MStream IO (Particle, Float) (Particle, Float)
slamtest = slam (initp, 0.0) slamobs

run_slamtest :: IO (Particle, Float)
run_slamtest = runMStream printer slamtest
  where
  printer ((is, f), s) = do
    putStrLn $ "Position history: " ++ show is
    putStrLn $ "Map: " ++ show (map f [0 .. max_pos])
    putStrLn $ "Score: " ++ show s
