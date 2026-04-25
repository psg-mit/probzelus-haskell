module Util.Trace where

import Data.Dynamic
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)

type Trace = Map String Dynamic

isNullTrace :: Trace -> Bool
isNullTrace = M.null

infixr 7 |->
(|->) :: String -> Trace -> Trace
k |-> v = M.singleton k (toDyn v)

atom :: Typeable a => a -> Trace
atom v = M.singleton "" (toDyn v)


list :: Typeable a => [a] -> Trace
list xs = atom (map atom xs)

minus :: Trace -> Trace -> Trace
minus t toRemove = M.withoutKeys t (M.keysSet toRemove)

(!) :: Typeable a => Trace -> String -> a
t ! k = fromMaybe (error "(!): trace value of incorrect type") $ fromDynamic (t M.! k)