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
> import qualified Data.Map.Strict as Map

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

A feature system is basically a collection of
compiled information from a feature table
(symbol columns, feature rows, value cells)

> data Sys = Sys { symbols  :: [Symbol],
>                  features :: [String],
>                  values   :: [String],
>                  elements :: [Elt],

                   elements should be listed in order of
                   decreasing priority
 
>                  symMap   :: Map Symbol (Set Elt),

                    maps each symbol to a set of elements
                    (feature-value pairs)

>                  classMap :: Map Elt (Set Symbol),

                    maps each element (feature-value pair)
                    to a set of symbols

>                  ncMap    :: Map (Set Elt) (Set Symbol),

                    maps a set of elements (feature-value pairs)
                    to a set of symbols

>                  unifyMap :: Map Elt (Set Elt)

                    maps an element to a set of
                    compatible/unifiable elements

>                } deriving (Eq, Show, Read)


> ofTable :: Table -> Sys
> ofTable t =
>   Sys
>   { symbols  = syms,
>     features = feats,
>     values   = vals,
>     elements = elts,
>     symMap   = makeSymMap t,
>     classMap = clmap,
>     ncMap    = makeNCMap clmap (Set.fromList syms),
>     unifyMap = makeUnifyMap clmap elts
>   }
>   where syms  = Table.colNames t
>         feats = Table.rowNames t
>         vals  = Table.values t
>         elts  = makeElts t feats vals
>         clmap = makeClassMap t

> hread :: String -> Sys
> hread = ofTable . Table.hread

> makeElts :: Table -> [String] -> [String] -> [Elt]
> makeElts t fs vs = map eltFromPair [ (f,v) | f <- fs, v <- vs]

> addSymElt :: Map String (Set Elt) -> (String,String,String) -> Map String (Set Elt)
> addSymElt m (s,f,v) = 
>   Map.insertWith Set.union s (Set.singleton (makeElt f v)) m

> makeSymMap :: Table -> Map String (Set Elt)
> makeSymMap t = List.foldl' addSymElt Map.empty t
> 
> addEltSym ::  Map Elt (Set String) -> (String,String,String) -> Map Elt (Set String)
> addEltSym m (s,f,v) =
>   Map.insertWith Set.union (makeElt f v) (Set.singleton s) m
  
> makeClassMap :: Table -> Map Elt (Set String)
> makeClassMap t = List.foldl' addEltSym Map.empty t

> lookupElt :: Sys -> Elt -> Set String
> lookupElt sys elt = maybe Set.empty id $ Map.lookup elt (classMap sys)

> matchingSymbols :: Sys -> Set Elt -> [String]
> matchingSymbols sys xs = Set.elems $ Set.foldl' (\ys x -> Set.intersection ys (lookupElt sys x)) (Set.fromList (symbols sys)) xs

baseNCMap only contains the empty bundle (size 0)
which maps to all symbols

> baseNCMap :: Set String -> Map (Set Elt) (Set String)
> baseNCMap symbols = Map.singleton Set.empty symbols

> insertMapMaybe :: (Set Elt)
>                -> Set String
>                -> Elt
>                -> Set String
>                -> Map (Set Elt) (Set String)
>                -> Map (Set Elt) (Set String)

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

> addToBundles :: Map Elt (Set String) -> Map (Set Elt) (Set String) -> Map (Set Elt) (Set String)
> addToBundles classMap map = Map.foldrWithKey
>                             (\k d m -> Map.foldrWithKey (insertMapMaybe k d) m classMap)
>                             Map.empty
>                             map 
>

> makeNCMap :: Map Elt (Set String) -> Set String -> Map (Set Elt) (Set String) 
> makeNCMap classMap symbols = closeMap (addToBundles classMap) [baseNCMap symbols]

the unifyMap ignores zero-valued elements because they are unifiable
with everything.  Consequently if a unifyMap is searched for a
zero-valued element Nothing will be returned.

Care should be taken on how to interpret Nothing : either all elements
or the Set.empty could be appropriate depending on the situation.

> makeUnifyMap :: Map Elt (Set String) -> [Elt] -> Map Elt (Set Elt)
> makeUnifyMap clmp elts = List.foldl' (addUnifiableElts clmp elts) Map.empty nonNilEltPairs
>  where
>     nonNilEltPairs =
>       filter
>       (\(e1,e2) -> value e1 /= "0" && value e2 /= "0" && e1 /= e2)
>       [ (x,y) | x <- elts, y <- elts ]

if e1 is unifiable with e2 we add e2 to the set of elements compatible with e1.

> addUnifiableElts :: Map Elt (Set String) -> [Elt] -> Map Elt (Set Elt) -> (Elt,Elt) -> Map Elt (Set Elt)
> addUnifiableElts clmp elts m (e1,e2) =
>   Map.insertWith
>   Set.union
>   e1 -- this is the key
>   (coExistInSomeSymbol e2 (Map.lookup e1 clmp) (Map.lookup e2 clmp))
>   m

> coExistInSomeSymbol :: Elt -> Maybe (Set String) -> Maybe (Set String) -> Set Elt
> coExistInSomeSymbol elt (Just xs) (Just ys) =
>   if Set.null (Set.intersection xs ys)
>   then Set.empty
>   else Set.singleton elt
> coExistInSomeSymbol _ _ _ = Set.empty

> removeZeroElts :: Set Elt -> Set Elt
> removeZeroElts = Set.filter (\x -> not (eltIsValue "0" x))

> nonZeroElts :: Sys -> Set Elt
> nonZeroElts sys = removeZeroElts $ Set.fromList (elements sys)
