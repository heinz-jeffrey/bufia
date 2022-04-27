> {-|
> Module:    PriorityQueue
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module provides a priority queue along
> the lines of page 385 of Simon Thompson's
> Haskell Craft of Functional Programming.
> -}

> module PriorityQueue
>   ( PriorityQueue,
>     empty,    -- Queue a
>     isEmpty,  -- Queue a -> Bool
>     push,     -- a -> Queue a -> Queue a
>     pushMany, -- [a] -> Queue a -> Queue a
>     pop,      -- Queue a -> (a, Queue a)
>   ) where

> import qualified Data.Set as Set
> type Set = Set.Set

> data PriorityQueue a = PriorityQueue (Set a)

> empty :: PriorityQueue a
> empty = PriorityQueue Set.empty

> isEmpty :: PriorityQueue a -> Bool
> isEmpty (PriorityQueue s) = Set.null s

> push :: Ord a => a -> PriorityQueue a -> PriorityQueue a
> push x (PriorityQueue xs) = PriorityQueue (Set.insert x xs)

> pushMany :: Ord a => Set a -> PriorityQueue a -> PriorityQueue a
> pushMany xs (PriorityQueue ys) = PriorityQueue (Set.union xs ys)

> pop :: Ord a => PriorityQueue a -> (a, PriorityQueue a)
> pop (PriorityQueue ys) = (x, PriorityQueue xs) where (x,xs) = Set.deleteFindMin ys

Notes:
Taking the head of a list is O(1) time but finding the minimum in a set is O(log n) time.
FromList in haskell always returns the items in the set in ascending order beginning with the minimum.
So this priority queue prioritizes the minimal elements first.
