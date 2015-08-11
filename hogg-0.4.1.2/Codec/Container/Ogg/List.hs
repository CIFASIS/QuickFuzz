--
-- Module      : Codec.Container.Ogg.List
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.List (
  listMerge,
  classify
) where

import Data.List

------------------------------------------------------------
-- listMerge
--

-- | Merge the contents of a list of lists into one sorted list
listMerge :: Ord a => [[a]] -> [a]
listMerge ll = listMerge' $ listSort ll

listMerge' :: Ord a => [[a]] -> [a]
listMerge' [] = []
listMerge' (l:ls) = case l of
  [] -> listMerge' ls
  (x:xs) -> x : listMerge (xs:ls)

listSort :: Ord a => [[a]] -> [[a]]
listSort xs = sortBy listOrd xs

listOrd :: Ord a => [a] -> [a] -> Ordering
listOrd [] _ = LT
listOrd _ [] = GT
listOrd (x:_) (y:_) = compare x y

------------------------------------------------------------
-- classify
--

-- | Separate the items in a list into lists of similar items
classify :: (a -> a -> Bool) -> [a] -> [[a]]
classify f xs = classify' f xs []

classify' :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
classify' _ [] ss = ss
classify' f (x:xs) [] = classify' f xs [[x]]

classify' f (x:xs) (s:ss) = case f x (head s) of
  True  -> classify' f xs ((s++[x]):ss)
  False -> classify' f xs (s : classify' f [x] ss)

