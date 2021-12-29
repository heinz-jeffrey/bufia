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
factors' order k = Set.filter bylength . Set.fromList . extract order
    where bylength w = len <= k && len > 0
                      where len = length w
            
factors :: Ord a => Order -> Int -> [[a]] -> Set [a]
factors order k = List.foldl' Set.union Set.empty . List.map (factors' order k)

toFactors :: Ord a => Order -> Int -> [[a]] -> [[a]]
toFactors order k = Set.toList . factors order k

wordlist :: [[String]]
wordlist = [ ["K"],
             ["R"],
             ["D"],
             ["S"],
             ["M"],
             ["P"],
             ["B"],
             ["L"],
             ["F"],
             ["HH"],
             ["T"],
             ["P","R"],
             ["W"],
             ["N"],
             ["V"],
             ["G"],
             ["JH"],
             ["S","T"],
             ["T","R"],
             ["K","R"],
             ["SH"],
             ["G","R"],
             ["CH"],
             ["B","R"],
             ["S","P"],
             ["F","L"],
             ["K","L"],
             ["S","K"],
             ["Y"],
             ["F","R"],
             ["P","L"],
             ["B","L"],
             ["S","L"],
             ["D","R"],
             ["K","W"],
             ["S","T","R"],
             ["TH"],
             ["S","W"],
             ["G","L"],
             ["HH","W"],
             ["S","N"],
             ["S","K","R"],
             ["Z"],
             ["S","M"],
             ["TH","R"],
             ["S","K","W"],
             ["T","W"],
             ["S","P","R"],
             ["SH","R"],
             ["S","P","L"],
             ["DH"],
             ["D","W"],
             ["G","W"],
             ["TH","W"],
             ["S","K","L"] ]
