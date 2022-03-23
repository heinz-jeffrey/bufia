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
> import qualified Data.IntSet as IntSet
> import qualified Data.IntMap as IntMap

> type IntSet = IntSet.IntSet
> type IntMap = IntMap.IntMap
> type Set = Set.Set
> type Map = Map.Map
> type Table = Table.Table


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

> hshowElt :: [Elt] -> Int -> String
> hshowElt es n = (value elt) ++ (feature elt)
>   where elt = decode es n

> hreadElt :: String -> Elt
> hreadElt [] = error "hreadElt: Unreadable Feature Element"
> hreadElt (x:[]) = error "hreadElt: Feature Name is Empty String"
> hreadElt (x:xs) = eltFromPair (xs,[x])

We will encode all elements as integers internally in the feature
system.

A feature system is basically a collection of
compiled information from a feature table
(symbol columns, feature rows, value cells)

> data Sys = Sys { symbols  :: [Symbol],
>                  features :: [String],
>                  values   :: [String],
>                  elements :: [Elt],

                    elements should be listed in
                    order of decreasing priority

>                  symMap   :: IntMap IntSet,

                    maps each symbol to a set of elements
                    symbol → set elt

>                  classMap :: IntMap IntSet,

                    maps each element to a set of symbols
                    elt → set symbol

>                  unifyMap :: IntMap IntSet

                    maps each elt to set of unifiable elemets
                    so from Elt to Set Elt (elt → set elt)
                    i.e. from Int to IntSet
                    i.e. IntMap IntSet

>                } deriving (Eq, Show, Read)


> getfeats :: Maybe [String] -- keeplist
>          -> Maybe [String] -- droplist
>          -> [String] -- featurelist
>          -> [String]

> getfeats Nothing Nothing flist = flist
> getfeats (Just xs) Nothing flist = filter (`elem` xs) flist
> getfeats Nothing (Just xs) flist = filter (\x -> not (elem x xs)) flist
> getfeats _ _ _ = error "ofTable: only one of keeplist/droplist to be used"


> ofTable :: Maybe [String]
>         -> Maybe [String]
>         -> Table
>         -> Sys

If the keeplist is Nothing AND if the droplist is Nothing
Then all features in the featuretable are used.
If both are Justs then an error is given
Otherwise only the ones in Just are used.

> ofTable keeplist droplist t =
>   Sys
>   { symbols  = syms,
>     features = feats,
>     values   = vals,
>     elements = elts,
>     symMap   = makeSymMap syms elts t,
>     classMap = clmap,
>     unifyMap = makeUnifyMap elts clmap
>   }
>   where syms  = Table.colNames t
>         feats = getfeats keeplist droplist (Table.rowNames t)
>         vals  = Table.values t
>         elts  = makeElts compare feats vals
>         clmap = makeClassMap syms elts t

The first argument of hread is a list of features to drop from the
full tables of features.

> hread :: Maybe [String] -> Maybe [String] -> String -> Sys
> hread keeplist droplist = ofTable keeplist droplist . Table.hread

> makeElts :: (Elt -> Elt -> Ordering)
>          -> [String] -> [String] -> [Elt]

> makeElts compare fs vs = List.sortBy compare
>                          $ map eltFromPair
>                          [ (f,v) | f <- fs, v <- vs ]

> addSymElt :: [Symbol] -> [Elt]
>           -> IntMap IntSet
>           -> (String,String,String)
>           -> IntMap IntSet

> addSymElt ss es m (s,f,v) =
>   IntMap.insertWith IntSet.union s_i (IntSet.singleton e_i) m
>   where e_i = encode es $ makeElt f v
>         s_i = encode ss s

> makeSymMap :: [Symbol] -> [Elt] -> Table -> IntMap IntSet
> makeSymMap ss es t = List.foldl' (addSymElt ss es) IntMap.empty t


> addEltSym :: [Symbol] -> [Elt]
>           -> IntMap IntSet
>           -> (String,String,String)
>           -> IntMap IntSet

> addEltSym ss es m (s,f,v) =
>   IntMap.insertWith IntSet.union e_i (IntSet.singleton s_i) m
>   where e_i = encode es (makeElt f v)
>         s_i = encode ss s

> makeClassMap :: [Symbol] -> [Elt]
>              -> Table
>              -> IntMap IntSet

> makeClassMap ss es t = List.foldl' (addEltSym ss es) IntMap.empty t

> lookupElt :: Sys -> Int -> IntSet
> lookupElt sys e_i = maybe IntSet.empty id $ IntMap.lookup e_i (classMap sys)

> matchingSymbols :: Sys -> IntSet -> IntSet -- Sys -> Set Elt -> Set Symbol
> matchingSymbols sys es = IntSet.foldl' (\ys elt -> IntSet.intersection ys (lookupElt sys elt)) all_s_i es
>   where all_s_i = IntSet.fromList . indices $ symbols sys


Next we build a map from feature bundles to sets of matching symbols. 

ncMap :: Set Elt → Set Symbols which is IntSet  → IntSet

baseNCMap only contains the empty bundle (size 0)
which maps to all symbols

> baseNCMap :: [Symbol] -> Map IntSet IntSet
> baseNCMap ss = Map.singleton IntSet.empty all_s_i
>   where all_s_i = IntSet.fromList (indices ss)

> insertMapMaybe :: IntSet  -- k
>                -> IntSet  -- d
>                -> Int     -- k'
>                -> IntSet  -- d'
>                -> Map IntSet IntSet -- m', oldmap
>                -> Map IntSet IntSet -- newmap

if the newBundle is the same as the oldBundle don't do anything

> insertMapMaybe k d k' d' m'
>   | k == newBundle = m'

if the intersection is empty then don't do anything

>   | IntSet.null intersection = m'

otherwise

>   | otherwise = Map.insert newBundle intersection m'
>   where intersection = IntSet.intersection d d'
>         newBundle = IntSet.insert k' k
>

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b

> addToBundles :: IntMap IntSet -> Map IntSet IntSet -> Map IntSet IntSet
> addToBundles classMap map = Map.foldrWithKey
>                             (\k d m -> IntMap.foldrWithKey (insertMapMaybe k d) m classMap)
>                             Map.empty
>                             map
>

> makeNCMap :: Sys -> Map IntSet IntSet
> makeNCMap sys = closeMap
>                 (addToBundles (classMap sys))
>                 [baseNCMap (symbols sys)]



the unifyMap ignores zero-valued elements because they are unifiable
with everything.  Consequently if a unifyMap is searched for a
zero-valued element Nothing will be returned.

Care should be taken on how to interpret Nothing : either all elements
or the Set.empty could be appropriate depending on the situation.


> makeUnifyMap :: [Elt] -> IntMap IntSet -> IntMap IntSet
> makeUnifyMap elts clmap = List.foldl' (addUnifiableElts clmap) IntMap.empty nonNilEltPairs
>   where
>     nonNilEltPairs =
>       map (\(x,y) -> (encode elts x, encode elts y))
>       $ filter
>       (\(e1,e2) -> value e1 /= "0" && value e2 /= "0" && e1 /= e2)
>       [ (x,y) | x <- elts, y <- elts ]

if e1 is unifiable with e2 we add e2 to the set of elements compatible with e1.

> addUnifiableElts :: IntMap IntSet -> IntMap IntSet -> (Int,Int) -> IntMap IntSet
> addUnifiableElts clmap m (e1,e2) =
>   IntMap.insertWith
>   IntSet.union
>   e1 -- this is the key
>   (coExistInSomeSymbol e2 (IntMap.lookup e1 clmap) (IntMap.lookup e2 clmap))
>   m

> coExistInSomeSymbol :: Int -> Maybe IntSet -> Maybe IntSet -> IntSet
> coExistInSomeSymbol e_i (Just xs) (Just ys) =
>   if IntSet.null (IntSet.intersection xs ys)
>   then IntSet.empty
>   else IntSet.singleton e_i
> coExistInSomeSymbol _ _ _ = IntSet.empty


> removeZeroElts :: [Elt] -> IntSet -> IntSet
> removeZeroElts es = IntSet.filter (\x -> not (eltIsValue "0" (decode es x)))


Ordering Features

> compareElt :: [Elt] -> Elt -> Elt -> Ordering
> compareElt es e1 e2 =
>   compare (encode es e1) (encode es e2)

> maxElt :: [Elt] -> Elt -> Elt -> Elt
> maxElt es e1 e2 =
>   if (encode es e1) < (encode es e2)
>   then e2
>   else e1

> greaterElts :: Sys -> Elt -> [Elt]
> greaterElts sys elt = tail $ List.dropWhile (/= elt) (elements sys)


> compareByFeatureValue :: Sys -> Elt -> Elt -> Ordering
> compareByFeatureValue sys e1 e2 = compare (f1,v1) (f2,v2)
>   where fs = features sys
>         vs = values sys
>         f1 = encode fs (feature e1)
>         f2 = encode fs (feature e2)
>         v1 = encode vs (value e1)
>         v2 = encode vs (value e2)

> compareByExtSize :: Sys -> Elt -> Elt -> Ordering
> compareByExtSize sys e1 e2
>   | size s1 == size s2 = compareByFeatureValue sys e1 e2
>   | size s1  < size s2 = GT
>   | otherwise          = LT
>   where es = elements sys
>         s1 = lookupElt sys (encode es e1)
>         s2 = lookupElt sys (encode es e2)
>         size = IntSet.size

> makeMapi2i :: [Elt] -> [Elt] -> IntMap Int
> makeMapi2i oldElts newElts = List.foldl' f IntMap.empty newElts
>   where f m e = IntMap.insert (encode oldElts e) (encode newElts e) m

> i2if :: IntMap Int -> Int -> Int
> i2if m n = maybe (-1) id (IntMap.lookup n m)

> convertIntSet :: IntMap Int -> IntSet -> IntSet
> convertIntSet m = IntSet.map (i2if m)

> convertKeysIntMap :: IntMap Int -> IntMap a -> IntMap a
> convertKeysIntMap m = IntMap.foldlWithKey' f IntMap.empty
>   where f newmap k d = IntMap.insert (i2if m k) d newmap

> sysByCompareElts :: (Elt -> Elt -> Ordering) -> Sys -> Sys
> sysByCompareElts fcompare sys =
>   Sys
>   { symbols  = symbols sys,
>     features = features sys,
>     values   = values sys,
>     elements = newElts,
>     symMap   = IntMap.foldlWithKey' f IntMap.empty (symMap sys),
>     classMap = convertKeysIntMap i2imap (classMap sys),
>     unifyMap = IntMap.foldlWithKey' f IntMap.empty updatedKeyMap
>   }
>   where oldElts       = elements sys
>         newElts       = List.sortBy fcompare oldElts
>         i2imap        = makeMapi2i oldElts newElts
>         updatedKeyMap = convertKeysIntMap i2imap (unifyMap sys)
>         f newmap k d  = IntMap.insert k (convertIntSet i2imap d) newmap


> sysByExtSize :: Sys -> Sys
> sysByExtSize sys = sysByCompareElts (compareByExtSize sys) sys

> sysByFV :: Sys -> Sys
> sysByFV sys = sysByCompareElts (compareByFeatureValue sys) sys

> adjustSys :: Int -> (Sys -> Sys)
> adjustSys 0 = Feature.sysByExtSize
> adjustSys 1 = Feature.sysByFV
> adjustSys 2 = id

",i,u,e,o,a,\nback,-,+,-,+,-\nlow,-,-,-,-,+\nhigh,+,+,-,-,-\n"
