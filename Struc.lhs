> {-|
> Module:    Struc
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module constructs models of words as structures
> which are sequences of feature bundles.
> -}

> module Struc where

> import Base
> import Split
> import Feature
> import qualified Data.List as List
> import qualified Data.Set as Set
> import qualified Data.Map as Map

We use `data` instead of `type` because we want to create our own instance of Ord. 

> data Bundle = Bundle (Set Elt) deriving (Eq, Show, Read)

> unBundle :: Bundle -> Set Elt
> unBundle (Bundle xs) = xs

> bundleSep = ','

> hshowBundle :: Bundle -> String
> hshowBundle (Bundle xs) = "[" ++ List.intercalate [bundleSep] contents ++ "]"
>   where contents = map hshowElt (Set.toList xs)

> hreadBundle :: String -> Bundle
> hreadBundle xs
>   | length xs < 2
>   || head xs /= '['
>   || last xs /= ']' = error "hreadBundle: Unreadable Bundle; bundles should be enclosed in square brackets."
>   | otherwise = Bundle . List.foldl' f Set.empty $ Split.bySep bundleSep ys
>   where ys = (reverse . tail . reverse . tail) xs -- this removes the square brackets
>         f b str = Set.insert (hreadElt str) b

> minBundle :: Bundle
> minBundle = Bundle Set.empty

maybe this should be called bundleMatchingSymbols?

> bundleExtension :: Sys -> Bundle -> Set Symbol
> bundleExtension sys (Bundle xs) = Set.fromList (matchingSymbols sys xs)

> instance Ord Bundle where
>   compare (Bundle xs) (Bundle ys)
>     | Set.null xs && Set.null ys = EQ
>     | (Set.size xs) < (Set.size ys) = LT
>     | (Set.size xs) > (Set.size ys) = GT
>     | minx < miny = LT
>     | minx > miny = GT
>     | otherwise = compare (Set.delete minx xs) (Set.delete miny ys)
>     where minx = Set.findMin xs 
>           miny = Set.findMin ys 

> lookupUnifiable :: Sys -> Elt -> Set Elt
> lookupUnifiable sys elt = maybe Set.empty id $ Map.lookup elt (Feature.unifyMap sys)

> findUnifiable :: Sys -> Set Elt -> Elt -> Set Elt
> findUnifiable sys elts elt = Set.intersection elts (lookupUnifiable sys elt)

Note we remove the zero-valued features because lookupUnifiable
will return Set.emtpty for elements not in unifyMap and we don't
care about the zero-valued features

> unifiableElts :: Sys -> Bundle -> Set Elt
> unifiableElts sys (Bundle elts) = Set.foldl' (findUnifiable sys) (nonZeroElts sys) (removeZeroElts elts)

> insertBundleMaybe :: Int -> Elt -> Bundle -> Maybe Bundle
> insertBundleMaybe maxBundleSize x (Bundle xs)
>   | 1 + (Set.size xs) > maxBundleSize = Nothing
>   | otherwise = Just (Bundle (Set.insert x xs))

> bundleNextGreater' :: Sys -> Int -> Bundle -> Set Bundle
> bundleNextGreater' sys maxBundleSize b = exciseNothings (Set.map f (unifiableElts sys b))
>   where f elt = insertBundleMaybe maxBundleSize elt b

> bundleNextGreater :: Sys -> Bundle -> Set Bundle
> bundleNextGreater sys b@(Bundle xs) = Set.map f (unifiableElts sys b)
>   where f elt = Bundle $ Set.insert elt xs

> bundleIsLessThan :: Bundle -> Bundle -> Bool
> bundleIsLessThan (Bundle xs) (Bundle ys) = xs `Set.isSubsetOf` ys

> bundleIsLessThanExt :: Sys -> Bundle -> Bundle -> Bool
> bundleIsLessThanExt sys xs ys = (bundleExtension sys xs) `Set.isSubsetOf` (bundleExtension sys ys)


Structures


> data Struc = Struc [Bundle] deriving (Eq, Show, Read)

> unStruc :: Struc -> [Bundle]
> unStruc (Struc bs) = bs

> hshow :: Struc -> String
> hshow (Struc xs) = concat (map hshowBundle xs)

> listHshow :: [Struc] -> String
> listHshow = List.intercalate ";" . List.map hshow 

> setHshow :: Set Struc -> String
> setHshow = List.intercalate "\n" . List.map hshow . Set.toList 

> hread :: String -> Struc
> hread = Struc . map hreadBundle . map f . Split.bySep ']'
>   where f x = x ++ "]"

> setHread :: String -> Set Struc 
> setHread = Set.fromList . map Struc.hread . lines 

> minStruc :: Struc
> minStruc = Struc []

> instance Ord Struc where
>   compare (Struc xsl) (Struc ysl)
>     | List.null xsl && List.null ysl = EQ
>     | (List.length xsl) < (List.length ysl) = LT
>     | (List.length xsl) > (List.length ysl) = GT
>     | headxs < headys = LT
>     | headxs > headys = GT
>     | otherwise = compare (Struc (List.tail xsl)) (Struc (List.tail ysl))
>     where headxs = List.head xsl
>           headys = List.head ysl


maybe this should be called minMatchingStrings?

> minExtension :: Sys -> Struc -> Set [Symbol]
> minExtension sys (Struc []) = Set.empty
> minExtension sys (Struc (b:[])) = Set.map (:[]) (bundleExtension sys b)
> minExtension sys (Struc (b:bs)) =  minExtension sys (Struc [b]) +++ minExtension sys (Struc bs)

-- > kExtension :: Sys -> Order -> Int -> Struc -> Set [Symbol]
-- > kExtension sys ord k (Struc xs)
-- >   | length xs >  k = Set.empty
-- >   | length xs == k = minExt
-- >   | ord == Prec    = kPrecExtend sys k minExt
-- >   | ord == Succ    = kSuccExtend sys k minExt
-- >   where minExt = minExtension sys (Struc xs) 

-- > kSuccExtend :: Sys -> Int -> Set [Symbol] -> Set [Symbol]
-- > kSuccExtend sys k ws = infixate ...

-- > kPrecExtend :: Sys -> Int -> Set [Symbol] -> Set [Symbol]
-- > kPrecExtend sys k ws = subsequate ...

getElts maps a string (symbol) to the set of features it has
given by the feature system.

> getElts :: Sys -> Symbol -> Bundle
> getElts sys x = Bundle (maybe Set.empty id elts)
>   where elts = Map.lookup x (symMap sys)

The maybe function takes a default value, a function, and a Maybe
value. If the Maybe value is Nothing, the function returns the
default value. Otherwise, it applies the function to the value
inside the Just and returns the result.

We lift getElts to changing a list of symbols to a structure

> ofWord :: Sys -> [Symbol] -> Struc
> ofWord sys xs = Struc (map (getElts sys) xs)

We lift changeStructure to change a wordlist to a set of structures

> setOfWordSet :: Sys -> [[Symbol]] -> Set Struc
> setOfWordSet sys xss = Set.fromList (map (Struc.ofWord sys) xss)

> strucIsPrefixOf :: Struc -> Struc -> Bool
> strucIsPrefixOf (Struc []) _                  =  True
> strucIsPrefixOf _ (Struc [])                  =  False
> strucIsPrefixOf (Struc (x:xs)) (Struc (y:ys)) = x `bundleIsLessThan` y && strucIsPrefixOf (Struc xs) (Struc ys)

> strucIsInfixOf :: Struc -> Struc -> Bool
> strucIsInfixOf needle (Struc haystack) = any (strucIsPrefixOf needle) (List.map Struc (List.tails haystack))

> strucIsSubsequenceOf :: Struc -> Struc -> Bool
> strucIsSubsequenceOf (Struc []) _ =  True
> strucIsSubsequenceOf _ (Struc []) =  False
> strucIsSubsequenceOf s@(Struc (x:xs)) (Struc (y:ys)) =
>   if x `bundleIsLessThan` y then strucIsSubsequenceOf (Struc xs) (Struc ys)
>   else strucIsSubsequenceOf s (Struc ys)

> isLessThan :: Order -> Struc -> Struc -> Bool
> isLessThan Succ = strucIsInfixOf
> isLessThan Prec = strucIsSubsequenceOf

> nextGreater' :: Sys -> Int -> Struc -> Set Struc
> nextGreater' sys maxBundleSize (Struc bs) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise]
>   where adjoinLeft = Set.singleton $ Struc (minBundle:bs)
>         adjoinRight = Set.singleton $ Struc (bs ++ [minBundle])
>         addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply (bundleNextGreater' sys maxBundleSize) bs)

> nextGreater :: Sys -> Struc -> Set Struc
> nextGreater sys (Struc bs) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise]
>   where adjoinLeft = Set.singleton $ Struc (minBundle:bs)
>         adjoinRight = Set.singleton $ Struc (bs ++ [minBundle])
>         addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply (bundleNextGreater sys) bs)



For Testing

csv = ",i,u,e,o,a\nhigh,+,+,-,-,-\nback,-,+,-,+,-\nlow,-,-,-,-,+"

vsys = (Feature.makeSystem . Table.ofStr) csv
high = makeElt "high" "+"
nonhigh = makeElt "high" "-"
back = makeElt "back" "+"
nonback = makeElt "back" "-"
low = makeElt "low" "+"
nonlow = makeElt "low" "-"

s0  = Struc []
s0' = Struc [minBundle]
s1  = Struc [Bundle (Set.fromList [high]), Bundle (Set.fromList [back])]
