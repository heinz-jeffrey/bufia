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
> import qualified Data.IntSet as IntSet
> import qualified Data.Map as Map

> type Sys = Feature.Sys

We use `data` instead of `type` because we want to create our own instance of Ord. 
We can also use `newtype` and create our own instance and this uses less memory.
Use newtype when the constructor has only one argument.  

Check out Data.IntSet instead of type (Data.Set.Set Int).

> newtype Bundle = Bundle IntSet deriving (Eq, Show, Read)

> unBundle :: Bundle -> IntSet
> unBundle (Bundle xs) = xs

> bundleSep = ','

> hshowBundle :: Sys -> Bundle -> String
> hshowBundle sys (Bundle xs) = "[" ++ List.intercalate [bundleSep] contents ++ "]"
>   where contents = map (hshowElt sys) (IntSet.toList xs)

> hreadBundle :: Sys -> String -> Bundle
> hreadBundle xs
>   | length xs < 2
>   || head xs /= '['
>   || last xs /= ']' = error "hreadBundle: Unreadable Bundle; bundles should be enclosed in square brackets."
>   | otherwise = Bundle . List.foldl' f IntSet.empty $ Split.bySep bundleSep ys
>   where ys = (reverse . tail . reverse . tail) xs -- this removes the square brackets
>         f b str = IntSet.insert (hreadElt sys str) b

> minBundle :: Bundle
> minBundle = Bundle IntSet.empty

bundleMaxElt must be non-empty...

> bundleMaxElt :: Sys -> Bundle -> Elt
> bundleMaxElt sys b = (last . List.sortBy (compareElt sys) . IntSet.elems . unBundle) b

maybe this should be called bundleMatchingSymbols?

> bundleExtension :: Sys -> Bundle -> Set Symbol
> bundleExtension sys (Bundle xs) = IntSet.fromList (matchingSymbols sys xs)

> instance Ord Bundle where
>   compare (Bundle xs) (Bundle ys)
>     | IntSet.null xs && IntSet.null ys = EQ
>     | (IntSet.size xs) < (IntSet.size ys) = LT
>     | (IntSet.size xs) > (IntSet.size ys) = GT
>     | minx < miny = LT
>     | minx > miny = GT
>     | otherwise = compare (IntSet.delete minx xs) (IntSet.delete miny ys)
>     where minx = IntSet.findMin xs 
>           miny = IntSet.findMin ys 

> lookupUnifiable :: Sys -> Elt -> IntSet
> lookupUnifiable sys elt = maybe IntSet.empty id $ Map.lookup elt (Feature.unifyMap sys)

> findUnifiable :: Sys -> IntSet -> Elt -> IntSet
> findUnifiable sys elts elt = IntSet.intersection elts (lookupUnifiable sys elt)

Note we remove the zero-valued features because lookupUnifiable
will return IntSet.empty for elements not in unifyMap and we don't
care about the zero-valued features

> unifiableElts :: Sys -> Bundle -> IntSet
> unifiableElts sys (Bundle elts) =
>   IntSet.foldl' (findUnifiable sys) (nonZeroElts sys) (removeZeroElts elts)

> insertBundleMaybe :: Int -> Elt -> Bundle -> Maybe Bundle
> insertBundleMaybe maxBundleSize x (Bundle xs)
>   | 1 + (IntSet.size xs) > maxBundleSize = Nothing
>   | otherwise = Just (Bundle (IntSet.insert x xs))

bundleNextGreater' takes an Int and will not add features to a bundle
if it would then exceed the max bundle size.

 eltsToAdd

> bundleNextGreater' :: Sys -> Int -> Bundle -> Set Bundle
> bundleNextGreater' sys maxBundleSize b = exciseNothings $ IntSet.map f eltsToAdd
>   where f elt     = insertBundleMaybe maxBundleSize elt b
>         eltsToAdd = uElts

-- >         eltsToAdd = IntSet.intersection uElts (IntSet.fromList gElts)

>         uElts     = unifiableElts sys b

-- >         gElts     = if b == minBundle
-- >                     then elements sys
-- >                     else greaterElts sys $ bundleMaxElt sys b


-- bundleNextGreater also only adds elements that are greater than the
-- largest element in the bundle.

bundleNextGreater has no restriction on size.

> bundleNextGreater :: Sys -> Bundle -> Set Bundle
> bundleNextGreater sys b@(Bundle xs) = IntSet.map f eltsToAdd
>   where f elt     = Bundle $ IntSet.insert elt xs
>         eltsToAdd = uElts

-- >         eltsToAdd = IntSet.intersection uElts (IntSet.fromList gElts)

>         uElts     = unifiableElts sys b

-- >         gElts     = if b == minBundle
-- >                     then elements sys
-- >                     else greaterElts sys $ bundleMaxElt sys b


> bundleIsLessThan :: Bundle -> Bundle -> Bool
> bundleIsLessThan (Bundle xs) (Bundle ys) = xs `IntSet.isSubsetOf` ys

> bundleIsLessThanExt :: Sys -> Bundle -> Bundle -> Bool
> bundleIsLessThanExt sys xs ys = (bundleExtension sys xs) `IntSet.isSubsetOf` (bundleExtension sys ys)


==========
Structures
==========


> data Struc = Struc [Bundle] deriving (Eq, Show, Read)

> unStruc :: Struc -> [Bundle]
> unStruc (Struc bs) = bs

> hshow :: Struc -> String
> hshow (Struc xs) = concat (map hshowBundle xs)

> listHshow :: [Struc] -> String
> listHshow = List.intercalate ";" . List.map hshow 

> setHshow :: Set Struc -> String
> setHshow = List.intercalate "\n" . List.map hshow . IntSet.toList 

> hread :: String -> Struc
> hread = Struc . map hreadBundle . map f . Split.bySep ']'
>   where f x = x ++ "]"

> setHread :: String -> Set Struc 
> setHread = IntSet.fromList . map Struc.hread . lines 

> minStruc :: Struc
> minStruc = Struc [minBundle]

> size :: Struc -> Int
> size (Struc bs) = length bs

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
> minExtension sys (Struc []) = IntSet.empty
> minExtension sys (Struc (b:[])) = IntSet.map (:[]) (bundleExtension sys b)
> minExtension sys (Struc (b:bs)) =  minExtension sys (Struc [b]) +++ minExtension sys (Struc bs)

-- > kExtension :: Sys -> Order -> Int -> Struc -> Set [Symbol]
-- > kExtension sys ord k (Struc xs)
-- >   | length xs >  k = IntSet.empty
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
> getElts sys x = Bundle (maybe IntSet.empty id elts)
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
> setOfWordSet sys xss = IntSet.fromList (map (Struc.ofWord sys) xss)

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
> nextGreater' sys maxBundleSize (Struc bs) = List.foldl IntSet.union IntSet.empty [adjoinLeft,adjoinRight,addEltsPointwise]
>   where adjoinLeft = IntSet.singleton $ Struc (minBundle:bs)
>         adjoinRight = IntSet.singleton $ Struc (bs ++ [minBundle])
>         addEltsPointwise = IntSet.map (\xs -> Struc xs) (pointwiseApply (bundleNextGreater' sys maxBundleSize) bs)

> nextGreater :: Sys -> Struc -> Set Struc
> nextGreater sys (Struc bs) = List.foldl IntSet.union IntSet.empty [adjoinLeft,adjoinRight,addEltsPointwise]
>   where adjoinLeft = IntSet.singleton $ Struc (minBundle:bs)
>         adjoinRight = IntSet.singleton $ Struc (bs ++ [minBundle])
>         addEltsPointwise = IntSet.map (\xs -> Struc xs) (pointwiseApply (bundleNextGreater sys) bs)


`extension ord sys k x` gives the set of words of size k that match struc x. 

> extension :: Order -> Sys -> Int -> Struc -> Set [Symbol]
> extension ord sys k x
>   | k < Struc.size x  = minExtension sys x
>   | otherwise         = IntSet.fromList $ concat (f x)
>   where bs  = unStruc x
>         n   = k - length bs
>         f x = List.map IntSet.elems
>               . List.map (minExtension sys)    -- :: [Set [Symbol]]
>               . List.map Struc                 -- :: [Struc]
>               $ expandWith ord minBundle n bs  -- :: [[Symbols]]




