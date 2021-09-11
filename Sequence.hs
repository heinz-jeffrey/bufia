module Sequence
  ( pointwiseApply
  ) where

import qualified Data.Set as Set
import qualified Data.List as List

type Set = Set.Set

-- combine xs ys z returns the list xs ++ [z] ++ ys
combine :: [a] -> [a] -> a -> [a]
combine left right x = left ++ x:right

-- combineAll xs ys zs returns a set of lists which combines each z in zs with the lists xs and ys
combineAll :: Ord a => [a] -> [a] -> Set a -> Set [a]
combineAll left right xs = Set.foldl' (\ys x -> Set.insert (combine left right x) ys) Set.empty xs

-- indexWord is self-explantory.
indexWord :: [a] -> [(a,Int)]
indexWord word = List.zip word [0 .. List.length word - 1]

-- dosomething f x n word splits the word at index n into left and
-- right words. The element at position n is lost (but will be
-- supplied as x in pointwiseApply below). Function f is applied to x
-- resulting in a set of xs. Then combineAll left right xs is
-- computed.
dosomething :: Ord a => (a -> Set a) -> a -> Int -> [a] -> Set [a]
dosomething f x n word = combineAll left right xs where
  (left,headright) = List.splitAt n word
  right = List.tail headright
  xs = f x 

-- pointwiseApply f word applies the function f pointwise within the
-- word and collects the results as a set. Function f takes each
-- element of the list to a set of elements. Each element in (f xs) is
-- then used to produce a new list which is added to the set.
pointwiseApply :: Ord a => (a -> Set a) -> [a] -> Set [a]
pointwiseApply f word = List.foldl'
                        (\ys (x,n) -> Set.union (dosomething f x n word) ys)
                        Set.empty
                        (indexWord word)
