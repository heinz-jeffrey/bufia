module Reduce
  (Order (..),
   orderOfStr,
   toFactors
  ) where

import Data.Set as Set
import Data.List as List

data Order = Succ | Prec

orderOfStr :: String -> Order
orderOfStr "prec" = Prec
orderOfStr "succ" = Succ
orderOfStr _ = error "Reduce.hs: orderOfStr"

extract :: Order -> ([a] -> [[a]])
extract Prec = List.subsequences
extract Succ = concat . List.map List.tails . List.inits

factors' :: Ord a => Order -> Int -> [a] -> Set [a]
factors' order k = Set.filter (\w -> length w == k) . Set.fromList . extract order

factors :: Ord a => Order -> Int -> [[a]] -> Set [a]
factors order k = List.foldl' Set.union Set.empty . List.map (factors' order k)

toFactors :: Ord a => Order -> Int -> [[a]] -> [[a]]
toFactors order k = Set.toList . factors order k

