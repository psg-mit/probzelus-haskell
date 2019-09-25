{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.AssocListSet where

import Data.List (nub)

newtype Set k = Set [k]

deriving instance Foldable Set

empty :: Set k
empty = Set []

singleton :: k -> Set k
singleton k = Set [k]

fromList :: Eq k =>  [k] -> Set k
fromList = Set . nub

insert' :: Eq k => k -> [k] -> [k]
insert' k [] = [k]
insert' k xs@(k' : ks) =
  if k == k' then xs
             else k' : insert' k ks

insert :: Eq k => k -> Set k -> Set k
insert k (Set xs) = Set (insert' k xs)

union :: Eq k => Set k -> Set k -> Set k
union (Set xs) s = foldr insert s xs