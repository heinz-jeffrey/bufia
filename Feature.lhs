> {-|
> Module:    Feature
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module provides a means to construct
> a phonological feature system and query it.
> -}

> module Feature where

The Feature System is the main object here. 

> import Base
> import qualified Table as Table
> import qualified Data.List as List
> import qualified Data.Set as Set
> import qualified Data.IntSet as IntSet
> import qualified Data.Map.Strict as Map

> type IntSet = IntSet.IntSet
> type Set = Set.Set
> type Map = Map.Map
> type Table = Table.Table

> type Symbol = String

Elements are feature-value pairs.

> data Elt = Elt { feature :: String,
>                  value   :: String
>                } deriving (Eq, Ord, Show, Read)

> makeElt :: String -> String -> Elt
> makeElt f v = Elt {feature = f, value = v}

> eltFromPair :: (String,String) -> Elt
> eltFromPair = uncurry makeElt

> eltIsValue :: String -> Elt -> Bool
> eltIsValue s e = value e == s

> eltIsFeature :: String -> Elt -> Bool
> eltIsFeature f e = feature e == f

> hshowElt :: Elt -> String
> hshowElt elt = (value elt) ++ (feature elt)

> hreadElt :: String -> Elt
> hreadElt [] = error "hreadElt: Unreadable Feature Element"
> hreadElt (x:[]) = error "hreadElt: Feature Name is Empty String"
> hreadElt (x:xs) = eltFromPair (xs,[x])

We will encode all elements as integers internally in the feature system.

> indexOfElt :: [Elt] -> Elt -> Int
> indexOfElt xs x = maybe (-1) id $ findIndex (==x) es

> eltOfIndex :: [Elt] -> Int -> Elt
> eltOfIndex = (!!)


A feature system is basically a collection of
compiled information from a feature table
(symbol columns, feature rows, value cells)

> data Sys = Sys { symbols  :: [Symbol],
>                  features :: [String],
>                  values   :: [String],
>                  elements :: [Elt],

                   elements should be listed in order of
                   decreasing priority
 
>                  symMap   :: Map Symbol (IntSet),

                    maps each symbol to a set of elements 
                    (feature-value pairs; int)

>                  classMap :: Map Int (Set Symbol),

                    maps each element (feature-value pair; int)
                    to a set of symbols

>                  ncMap    :: Map IntSet (Set Symbol),

                    maps a set of elements (feature-value pairs; ints)
                    to a set of symbols

>                  unifyMap :: Map Int IntSet

                    maps an element (int) to a set of
                    compatible/unifiable elements (int)

>                } deriving (Eq, Show, Read)


> ofTable :: Table -> Sys
> ofTable t =
>   Sys
>   { symbols  = syms,
>     features = feats,
>     values   = vals,
>     elements = elts,
>     symMap   = makeSymMap elts t,
>     classMap = clmap,
>     ncMap    = makeNCMap elts clmap (Set.fromList syms),
>     unifyMap = makeUnifyMap elts clmap elts
>   }
>   where syms  = Table.colNames t
>         feats = Table.rowNames t
>         vals  = Table.values t
>         elts  = makeElts feats vals
>         clmap = makeClassMap t

> hread :: String -> Sys
> hread = ofTable . Table.hread

> makeElts :: [String] -> [String] -> [Elt]
> makeElts fs vs = map eltFromPair [ (f,v) | f <- fs, v <- vs]

> addSymElt :: Map Symbol IntSet -> (String,String,String) -> Map String IntSet
> addSymElt m (s,f,v) = 
>   Map.insertWith Set.union s (Set.singleton (makeElt f v)) m

> makeSymMap :: Table -> Map String IntSet
> makeSymMap t = List.foldl' addSymElt Map.empty t
> 
> addEltSym ::  Map Elt (Set String) -> (String,String,String) -> Map Elt (Set String)
> addEltSym m (s,f,v) =
>   Map.insertWith Set.union (makeElt f v) (Set.singleton s) m
  
> makeClassMap :: Table -> Map Elt (Set String)
> makeClassMap t = List.foldl' addEltSym Map.empty t

> lookupElt :: Sys -> Elt -> Set String
> lookupElt sys elt = maybe Set.empty id $ Map.lookup elt (classMap sys)

> matchingSymbols :: Sys -> IntSet -> [String]
> matchingSymbols sys xs = Set.elems $ Set.foldl' (\ys x -> Set.intersection ys (lookupElt sys x)) (Set.fromList (symbols sys)) xs

baseNCMap only contains the empty bundle (size 0)
which maps to all symbols

> baseNCMap :: Set String -> Map IntSet (Set String)
> baseNCMap symbols = Map.singleton Set.empty symbols

> insertMapMaybe :: IntSet
>                -> Set String
>                -> Elt
>                -> Set String
>                -> Map IntSet (Set String)
>                -> Map IntSet (Set String)

if the newBundle is the same as the oldBundle don't do anything

> insertMapMaybe k d k' d' m'
>   | k == newBundle = m'

if the intersection is empty then don't do anything

>   | Set.null intersection = m'

otherwise

>   | otherwise = Map.insert newBundle intersection m'
>   where intersection = Set.intersection d d'
>         newBundle = Set.insert k' k
>        

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b

> addToBundles :: Map Elt (Set String) -> Map IntSet (Set String) -> Map IntSet (Set String)
> addToBundles classMap map = Map.foldrWithKey
>                             (\k d m -> Map.foldrWithKey (insertMapMaybe k d) m classMap)
>                             Map.empty
>                             map 
>

> makeNCMap :: Map Elt (Set String) -> Set String -> Map IntSet (Set String) 
> makeNCMap classMap symbols = closeMap (addToBundles classMap) [baseNCMap symbols]

the unifyMap ignores zero-valued elements because they are unifiable
with everything.  Consequently if a unifyMap is searched for a
zero-valued element Nothing will be returned.

Care should be taken on how to interpret Nothing : either all elements
or the Set.empty could be appropriate depending on the situation.

> makeUnifyMap :: Map Elt (Set String) -> [Elt] -> Map Elt IntSet
> makeUnifyMap clmp elts = List.foldl' (addUnifiableElts clmp elts) Map.empty nonNilEltPairs
>  where
>     nonNilEltPairs =
>       filter
>       (\(e1,e2) -> value e1 /= "0" && value e2 /= "0" && e1 /= e2)
>       [ (x,y) | x <- elts, y <- elts ]

if e1 is unifiable with e2 we add e2 to the set of elements compatible with e1.

> addUnifiableElts :: Map Elt (Set String) -> [Elt] -> Map Elt IntSet -> (Elt,Elt) -> Map Elt IntSet
> addUnifiableElts clmp elts m (e1,e2) =
>   Map.insertWith
>   Set.union
>   e1 -- this is the key
>   (coExistInSomeSymbol e2 (Map.lookup e1 clmp) (Map.lookup e2 clmp))
>   m

> coExistInSomeSymbol :: Elt -> Maybe (Set String) -> Maybe (Set String) -> IntSet
> coExistInSomeSymbol elt (Just xs) (Just ys) =
>   if Set.null (Set.intersection xs ys)
>   then Set.empty
>   else Set.singleton elt
> coExistInSomeSymbol _ _ _ = Set.empty

> removeZeroElts :: IntSet -> IntSet
> removeZeroElts = Set.filter (\x -> not (eltIsValue "0" x))

> nonZeroElts :: Sys -> IntSet
> nonZeroElts sys = removeZeroElts $ Set.fromList (elements sys)


Ordering Features

> compareFeatures :: Sys -> String -> String -> Ordering
> compareFeatures sys = compareByIndex (features sys)

> compareValues :: Sys -> String -> String -> Ordering
> compareValues sys = compareByIndex (values sys)

> compareElt :: Sys -> Elt -> Elt -> Ordering
> compareElt sys e1 e2 =
>   if featureComparison == EQ
>   then compareValues sys (value e1) (value e2)
>   else featureComparison
>   where featureComparison =
>           compareFeatures sys (feature e1) (feature e2)

> maxElt :: Sys -> Elt -> Elt -> Elt
> maxElt sys e1 e2 =
>   if compareElt sys e1 e2 == LT
>   then e2 else e1

> greaterElts :: Sys -> Elt -> [Elt]
> greaterElts sys elt = tail $ List.dropWhile (/= elt) (elements sys)
