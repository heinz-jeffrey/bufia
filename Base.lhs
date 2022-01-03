> {-|
> Module:    Base
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module provides general functions for a variety of purposes.
> -}

> module Base where

> import Data.Set as Set
> import Data.Map.Strict as Map
> import Data.List as List
> import Data.Maybe 

The class DiscretePartialOrderWithMinimum contains the functions needed for the BUFIA learning algorithm.
Functions `minimum` and `size` are self-explantory.
x <:< y is True iff x is less than y (structure x is contained within structure y)                    
`nextGreaterThan` returns the set of structures that are greater than 'a' by one step in the partial order.             
x <:< ys is True iff x is less than any element of y (structure x is contained within any structure y)
x >:> ys is True iff any y is less than x (any y is contained in x)

-- > class (Eq a) => DiscretePartialOrderWithMinimum a where
-- >   minimum :: a
-- >   size :: a -> Int
-- >   (<:<) :: a -> a -> Bool            
-- >   nextGreaterThan :: a -> Set a      
-- >   (<::<) :: a -> Set a -> Bool       
-- >   (>::>) :: a -> Set a -> Bool       
-- >   (<::<) x ys = Set.foldr' foldfunction False ys
-- >     where
-- >       foldfunction y bool = bool || (x <:< y) 
-- >   (>::>) x ys = Set.foldr' foldfunction False ys
-- >     where
-- >       foldfunction y bool = bool || (y <:< x)

`close f xs` returns the closure of xs under f.
It assumes f is monotonically increasing
and that there is a ceiling.

> closeSet :: Eq a => (Set a -> Set a) -> Set a -> Set a
> closeSet f xs 
>   | xs == nxs = xs
>   | otherwise = closeSet f nxs
>   where nxs = f xs

> closeMap :: (Eq k, Ord k, Eq d) => (Map k d -> Map k d) -> [Map k d] -> Map k d
> closeMap f maplist 
>   | Map.null nextMap = List.foldl' Map.union Map.empty maplist 
>   | otherwise = closeMap f (nextMap:maplist)
>   where nextMap = f (head maplist)

> zeroIntersect :: Ord a => Set a -> Set a -> Bool
> zeroIntersect xs ys = Set.null (Set.intersection xs ys)

`findUpdate f key map` looks up the data associated with key in map
and returns the pair (data,map). If Nothing is found, it computes
the data with `f key` and returns the pair (data,updated map).

> findUpdate :: (Eq d, Ord k) => (k -> d) -> k -> Map k d -> (d, Map k d)
> findUpdate f key map =
>   if result == Nothing
>   then (newValue, Map.insert key newValue map)
>   else (fromJust result, map)
>   where result = Map.lookup key map
>         newValue = f key

`exciseNothings xs` returns a set without the Nothing values

> insertExceptNothing :: Ord a => Set a -> Maybe a -> Set a
> insertExceptNothing xs Nothing = xs
> insertExceptNothing xs (Just x) = Set.insert x xs

> exciseNothings :: Ord a => Set (Maybe a) -> Set a
> exciseNothings = Set.foldl' insertExceptNothing Set.empty 


`setDoubleFold` is a product construction for sets.

> setDoubleFold :: (Ord a, Ord b) => (a -> a -> b) -> Set a -> Set a -> Set b
> setDoubleFold f xs ys = Set.foldl
>                         (\ys' y -> Set.union
>                                    ys'
>                                    (Set.foldl
>                                      (\xs' x -> Set.insert (f x y) xs')
>                                      Set.empty
>                                      xs)
>                         )
>                         Set.empty
>                         ys

`setZipWith` does a product construction over sets of lists.

> setZipWith' :: Ord a => ([a] -> [a] -> [a]) -> [a] -> Set [a] -> Set [a]
> setZipWith' f x = Set.foldl' (\zs y -> Set.insert (f x y) zs) Set.empty

> setZipWith :: Ord a => ([a] -> [a] -> [a]) -> Set [a] -> Set [a] -> Set [a]
> setZipWith f xs ys = Set.foldl' (\zs x -> Set.union zs (setZipWith' f x ys)) Set.empty xs


Concatenates two finite languages
(finite sets of finitely long sequences)

> (+++) :: Ord a => Set [a] -> Set [a] -> Set [a]
> (+++) = setDoubleFold (++) 

Returns the concatenation of a list of languages.

> (++++) :: Ord a => [(Set [a])] -> Set [a]
> (++++) [] = Set.empty
> (++++) xs = List.foldl' (+++) (Set.singleton []) xs

Returns the set of all sequences of length k from a given list of symbols

> kStrings :: Ord a => Int -> [a] -> Set [a]
> kStrings k alph = (++++) (List.map Set.fromList (replicate k alph'))
>   where alph' = List.map (:[]) alph


> pairsThatSumTo :: Int -> [(Int,Int)]
> pairsThatSumTo k = [ (x,y) | x <- [0..k], y <- [0..k], x + y == k ]

> padByPair :: a -> [a] -> (Int,Int) -> [a]
> padByPair pad xs (left,right) = (replicate left pad) ++ xs ++ (replicate right pad)

> infixate :: a -> [a] -> Int -> [[a]]
> infixate pad xs k = List.map (padByPair pad xs) (pairsThatSumTo (k - length xs))

infixate "x" ["a","b","c"] 5
[["a","b","c","x","x"],["x","a","b","c","x"],["x","x","a","b","c"]]

{-
*Feature> map (map (bundleExtension sys)) $ infixate ((head . unStruc) s0') (unStruc s1) 3
[[["i","u"],["o","u"],["a","e","i","o","u"]],[["a","e","i","o","u"],["i","u"],["o","u"]]]

*Feature> map (map (Set.fromList . (bundleExtension sys))) $ infixate ((head . unStruc) s0') (unStruc s1) 3
[[fromList ["i","u"],fromList ["o","u"],fromList ["a","e","i","o","u"]],[fromList ["a","e","i","o","u"],fromList ["i","u"],fromList ["o","u"]]]

*Feature> map (++++) $ map (map (Set.fromList . (bundleExtension sys))) $ infixate ((head . unStruc) s0') (unStruc s1) 3
[fromList ["ioa","ioe","ioi","ioo","iou","iua","iue","iui","iuo","iuu","uoa","uoe","uoi","uoo","uou","uua","uue","uui","uuo","uuu"],fromList ["aio","aiu","auo","auu","eio","eiu","euo","euu","iio","iiu","iuo","iuu","oio","oiu","ouo","ouu","uio","uiu","uuo","uuu"]]

*Feature> List.foldl' Set.union Set.empty $ map (++++) $ map (map (Set.fromList . (bundleExtension sys))) $ infixate ((head . unStruc) s0') (unStruc s1) 3
fromList ["aio","aiu","auo","auu","eio","eiu","euo","euu","iio","iiu","ioa","ioe","ioi","ioo","iou","iua","iue","iui","iuo","iuu","oio","oiu","ouo","ouu","uio","uiu","uoa","uoe","uoi","uoo","uou","uua","uue","uui","uuo","uuu"]

-}


combine xs ys z returns the list xs ++ [z] ++ ys

> combine :: [a] -> [a] -> a -> [a]
> combine left right x = left ++ x:right

combineAll xs ys zs returns a set of lists which combines each z in zs with the lists xs and ys

> combineAll :: Ord a => [a] -> [a] -> Set a -> Set [a]
> combineAll left right xs = Set.foldl' (\ys x -> Set.insert (combine left right x) ys) Set.empty xs

indexWord is self-explantory.

> indexWord :: [a] -> [(a,Int)]
> indexWord word = List.zip word [0 .. List.length word - 1]

dosomething f x n word splits the word at index n into left and
right words. The element at position n is lost (but will be
supplied as x in pointwiseApply below). Function f is applied to x
resulting in a set of xs. Then combineAll left right xs is
computed.

> dosomething :: Ord a => (a -> Set a) -> a -> Int -> [a] -> Set [a]
> dosomething f x n word = combineAll left right xs where
>   (left,headright) = List.splitAt n word
>   right = List.tail headright
>   xs = f x 

pointwiseApply f word applies the function f pointwise within the
word and collects the results as a set. Function f takes each
element of the list to a set of elements. Each element in (f xs) is
then used to produce a new list which is added to the set.

> pointwiseApply :: Ord a => (a -> Set a) -> [a] -> Set [a]
> pointwiseApply f word = List.foldl'
>                         (\ys (x,n) -> Set.union (dosomething f x n word) ys)
>                         Set.empty
>                         (indexWord word)


addWBs :: [String] -> [String]
addWBs xs = ["#"] ++ xs ++ ["#"]

> data Order = Succ | Prec

> orderOfStr :: String -> Order
> orderOfStr "prec" = Prec
> orderOfStr "succ" = Succ
> orderOfStr "Prec" = Prec
> orderOfStr "Succ" = Succ
> orderOfStr "sp" = Prec
> orderOfStr "sl" = Succ
> orderOfStr _ = error "Reduce.hs: orderOfStr"

> extract :: Order -> ([a] -> [[a]])
> extract Prec = List.subsequences
> extract Succ = concat . List.map List.tails . List.inits

> factors' :: Ord a => Order -> Int -> (Int -> Int -> Bool) -> [a] -> Set [a]
> factors' order k f = Set.filter (\w -> length w `f` k) . Set.fromList . extract order

> factors :: Ord a => Order -> Int -> (Int -> Int -> Bool) -> [[a]] -> Set [a]
> factors order k f = List.foldl' Set.union Set.empty . List.map (factors' order k f)

> toFactors :: Ord a => Order -> Int -> (Int -> Int -> Bool) -> [[a]] -> [[a]]
> toFactors order k f = Set.toList . factors order k f

`toFactors Succ k (==)` returns all subfactors (successor model) of size k
`toFactors Succ k (<=)` returns all subfactors (successor model) up to size k inclusive

> reduceWords :: Ord a => Order -> Int -> [[a]] -> [[a]]
> reduceWords order k ws = short ++ toFactors order k (==) long
>                          where (short,long) = List.partition (\w -> length w < k) ws

compareByIndex assumes x1 and x2 are elements of xs

> compareByIndex :: Eq a => [a] -> a -> a -> Ordering
> compareByIndex xs x1 x2 = compare i1 i2
>   where i1 = fromJust (List.elemIndex x1 xs)
>         i2 = fromJust (List.elemIndex x2 xs)

