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
> import qualified Data.IntSet as IntSet
> import qualified Data.Map as Map
> import qualified Data.IntMap as IntMap

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
>   where contents = map (hshowElt (elements sys)) (IntSet.toList xs)

> hreadBundle :: Sys -> String -> Bundle
> hreadBundle sys xs
>   | length xs < 2
>   || head xs /= '['
>   || last xs /= ']' = error "hreadBundle: Unreadable Bundle; bundles should be enclosed in square brackets."
>   | otherwise = Bundle . List.foldl' f IntSet.empty $ Split.bySep bundleSep ys
>   where ys = (reverse . tail . reverse . tail) xs -- this removes the square brackets
>         f b str = IntSet.insert (encode (elements sys) (hreadElt str)) b

> minBundle :: Bundle
> minBundle = Bundle IntSet.empty

bundleMaxElt must be non-empty...

> bundleMaxElt :: Bundle -> Int
> bundleMaxElt = IntSet.findMax . unBundle


maybe this should be called bundleMatchingSymbols?

> bundleExtension :: Sys -> Bundle -> IntSet
> bundleExtension sys (Bundle es) = matchingSymbols sys es

> instance Ord Bundle where
>   compare (Bundle xs) (Bundle ys)
>     | IntSet.null xs && IntSet.null ys = EQ
>     | (IntSet.size xs) < (IntSet.size ys) = LT
>     | (IntSet.size xs) > (IntSet.size ys) = GT
>     | minx < miny = LT
>     | minx > miny = GT
>     | otherwise = compare (Bundle (IntSet.delete minx xs))
>                           (Bundle (IntSet.delete miny ys))
>     where minx = IntSet.findMin xs 
>           miny = IntSet.findMin ys 


> lookupUnifiable :: Sys -> Int -> IntSet
> lookupUnifiable sys elt = maybe IntSet.empty id $ IntMap.lookup elt (Feature.unifyMap sys)

> findUnifiable :: Sys -> IntSet -> Int -> IntSet
> findUnifiable sys elts elt = IntSet.intersection elts (lookupUnifiable sys elt)

Note we remove the zero-valued features because lookupUnifiable
will return IntSet.empty for elements not in unifyMap and we don't
care about the zero-valued features

> unifiableElts :: Sys -> Bundle -> IntSet
> unifiableElts sys (Bundle elts) =
>   IntSet.foldl' (findUnifiable sys) allNonZeroElts (removeZeroElts elemList elts)
>   where elemList = elements sys
>         allNonZeroElts = removeZeroElts elemList (IntSet.fromList (indices elemList))


> insertBundleMaybe :: Int -> Int -> Bundle -> Maybe Bundle
> insertBundleMaybe maxBundleSize e_i (Bundle elts)
>   | 1 + (IntSet.size elts) > maxBundleSize = Nothing
>   | otherwise = Just (Bundle (IntSet.insert e_i elts))


bundleNextGreater' takes an Int and will not add features to a bundle
if it would then exceed the max bundle size.

> bundleNextGreater' :: Sys -> Int -> Bundle -> Set Bundle
> bundleNextGreater' sys maxBundleSize b =
>   exciseNothings . Set.fromList . map f . IntSet.toList $ eltsToAdd
>   where f e_i     = insertBundleMaybe maxBundleSize e_i b

-- >         eltsToAdd = uElts

>         eltsToAdd = IntSet.intersection uElts (IntSet.fromList gElts)
>         all_e_i   = indices (elements sys)
>         maxelt    = bundleMaxElt b
>         uElts     = unifiableElts sys b            -- unifiable elements
>         gElts     = if b == minBundle              -- greater elements
>                     then all_e_i 
>                     else filter (\i -> i > maxelt) all_e_i


-- bundleNextGreater also only adds elements that are greater than the
-- largest element in the bundle.


bundleNextGreater has no restriction on size.

> bundleNextGreater :: Sys -> Bundle -> Set Bundle
> bundleNextGreater sys b@(Bundle xs) =
>   Set.fromList . map f . IntSet.toList $ eltsToAdd
>   where f e_i     = Bundle $ IntSet.insert e_i xs
>         eltsToAdd = IntSet.intersection uElts (IntSet.fromList gElts)
>         all_e_i   = indices (elements sys)
>         maxelt    = bundleMaxElt b
>         uElts     = unifiableElts sys b            -- unifiable elements
>         gElts     = if b == minBundle              -- greater elements
>                     then all_e_i 
>                     else filter (\i -> i > maxelt) all_e_i


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

> hshow :: Sys -> Struc -> String
> hshow sys (Struc xs) = concat (map (hshowBundle sys) xs)

> listHshow :: Sys -> [Struc] -> String
> listHshow sys = List.intercalate ";" . List.map (hshow sys)

> setHshow :: Sys -> Set Struc -> String
> setHshow sys = List.intercalate "\n" . List.map (hshow sys) . Set.toList 

> hread :: Sys -> String -> Struc
> hread sys = Struc . map (hreadBundle sys) . map f . Split.bySep ']'
>   where f x = x ++ "]"

> setHread :: Sys -> String -> Set Struc 
> setHread sys = Set.fromList . map (Struc.hread sys) . filter (\x -> head x /= '%') . lines 

> minStruc :: Struc
> minStruc = Struc [minBundle]

> kminStruc :: Int -> Struc
> kminStruc k = Struc $ take k $ repeat minBundle

> isBlank :: Struc -> Bool
> isBlank (Struc bs) = all (==minBundle) bs

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
> minExtension sys (Struc []) = Set.empty
> minExtension sys (Struc (b:[])) = Set.fromList . map f $ IntSet.toList (bundleExtension sys b)
>   where f s_i = [decode (symbols sys) s_i]
> minExtension sys (Struc (b:bs)) = minExtension sys (Struc [b]) +++ minExtension sys (Struc bs)


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
> getElts sys s = Bundle (maybe IntSet.empty id elts)
>   where elts = IntMap.lookup s_i (symMap sys)
>         s_i  = encode (symbols sys) s

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
> nextGreater' sys maxBundleSize (Struc bs) =
>   let nextStrucs = Set.map Struc (pointwiseApply (bundleNextGreater' sys maxBundleSize) bs)
>   in if all (== minBundle) bs
>      then Set.insert (Struc (minBundle:bs)) nextStrucs 
>      else nextStrucs

> nextGreater :: Sys -> Struc -> Set Struc
> nextGreater sys (Struc bs) =
>   let nextStrucs = Set.map Struc (pointwiseApply (bundleNextGreater sys) bs)
>   in if all (== minBundle) bs
>      then Set.insert (Struc (minBundle:bs)) nextStrucs
>      else nextStrucs

 
`extension ord sys k x` gives the set of words of size k that match struc x.

> extension :: Order -> Sys -> Int -> Struc -> Set [Symbol]
> extension ord sys k x
>   | k < Struc.size x  = minExtension sys x
>   | otherwise         = Set.fromList $ concat (f x)
>   where bs  = unStruc x
>         n   = k - length bs
>         f x = List.map Set.elems
>               . List.map (minExtension sys)    -- :: [Set [Symbol]]
>               . List.map Struc                 -- :: [Struc]
>               $ expandWith ord minBundle n bs  -- :: [[Symbols]]




