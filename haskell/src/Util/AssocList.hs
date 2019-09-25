module Util.AssocList where

import Data.List (nubBy, groupBy, partition)
import Data.Function (on)

import Util.AssocListSet (Set (Set))
import qualified Util.AssocListSet as S

newtype Map k v = Map [(k, v)]

empty :: Map k v
empty = Map []

singleton :: k -> v -> Map k v
singleton k v = Map [(k, v)]

fromList :: Eq k => [(k, v)] -> Map k v
fromList = Map . nubBy ((==) `on` fst)

fromListWith :: Eq k => (v -> v -> v) -> [(k, v)] -> Map k v
fromListWith f = Map . map (\xs -> (fst (head xs), foldr1 f (map snd xs))) . groupBy ((==) `on` fst)

fromSet :: (k -> v) -> Set k -> Map k v
fromSet f (Set ks) = Map [ (k, f k) | k <- ks ]

insertWith' :: Eq k => (v -> v -> v) -> k -> v -> [(k, v)] -> [(k, v)]
insertWith' f k v [] = [(k, v)]
insertWith' f k v ((k', v') : xs) =
  if k == k' then (k, f v v') : xs
             else (k', v') : insertWith' f k v xs

insertWith :: Eq k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map xs) = Map (insertWith' f k v xs)

unionWith :: Eq k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (Map xs) m = foldr (uncurry (insertWith f)) m xs

partition :: (k -> Bool) -> Map k v -> (Map k v, Map k v)
partition f (Map xs) = (Map ys, Map zs)
  where (ys, zs) = Data.List.partition (f . fst) xs

toList :: Map k v -> [(k, v)]
toList (Map xs) = xs

size :: Map k v -> Int
size (Map xs) = length xs