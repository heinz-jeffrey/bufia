module Learn
  ( learn
  ) where

import Base
import qualified PriorityQueue as Queue

import qualified Data.Set as Set
import qualified Data.List as List

type Set = Set.Set
type Queue = Queue.PriorityQueue

nextQueue :: (Ord a, DiscretePartialOrderWithMinimum a) => a -> Set a -> Queue a -> Queue a
nextQueue struc visited queue = Queue.pushMany (Set.toList nextStrucs) queue
  where nextStrucs = Set.difference (nextGreaterThan struc) visited
  
learn' :: (Ord a,
           DiscretePartialOrderWithMinimum a,
           Grammar a
          ) =>
          Queue a ->        -- queue
          Int ->            -- maxSize (k value)
          (Set a) ->        -- positive data sample
          (Set [String]) -> -- negative data that is unaccounted for 
          a ->              -- current structure
          (Set a) ->        -- visited structures
          (Set a) ->        -- absent structures (constraints)
          (Set a)           -- output grammar
  
learn' queue k posData negData struc visited constraints
  -- If the Queue is empty we stop.
  | Queue.isEmpty queue = constraints           

  -- We also stop if there is no more negative data to account for.
  | Set.null negData = constraints

  -- We also stop if the struc is larger than k (since all
  -- subsequent strucs will also be larger than k
  | size struc > k = constraints

  -- if any constraint is contained within the struc we leave it and move
  -- on
  | struc >::> constraints = learn' qs k posData negData hd visited constraints

  -- if the struc is contained in the positive data then we add it to
  -- the visited structures (= not in the grammar) and move on
  | struc <::< posData = learn' nextQ k posData negData hd (Set.insert struc visited) constraints

  -- Now the struc is not in the positive data and not already
  -- subsumed by an existing constraint so next we check

  -- (I) whether it is contained in any remaining unaccounted negData. If so,
  -- we add it to the constraints and subtract its extension from the
  -- negData
    
     | nonZeroIntersect strucExt negData =
       learn' qs k posData (Set.difference negData strucExt) hd visited (Set.insert struc constraints)
    
  -- (II) whether it overlaps with existing constraints. If not, we add it to
  -- the constraints and and subtract its extension from the
  -- negData
  {-
  | noOverlap struc constraints =
    learn' qs k posData (Set.difference negData strucExt) hd visited (Set.insert struc constraints)
  -}
  -- Otherwise its extension is either already subsumed by the current
  -- grammar (I) or its extension overlaps with the current grammar
  -- (II) and will be skipped. Therefore we add the struc to the set
  -- of visited structures and move on
  | otherwise =
      learn' qs k posData negData hd (Set.insert struc visited) constraints
      --learn' qs k posData (Set.difference negData strucExt) hd visited (Set.insert struc constraints)
  where
    (hd,qs) = Queue.pop queue
    nextQ = nextQueue struc visited qs
    strucExt = extension k struc
    nonZeroIntersect xs ys = (not . Set.null) (Set.intersection xs ys)  -- only used with (I) above
    noOverlap x cs = Set.null    -- only used with (II) above
                   (Set.intersection (extension k x) (extension' k cs))
      
learn :: (Ord a,
          DiscretePartialOrderWithMinimum a,
          Grammar a
         ) =>
         Int            -> -- maxSize (k value)
         (Set a)        -> -- positive sample
         (Set [String]) -> -- negative data that is unaccounted for 
         a              -> -- current structure
         (Set a)           -- output grammar

learn k posData negData struc =
  learn' (Queue.push struc Queue.empty) k posData negData struc Set.empty Set.empty
