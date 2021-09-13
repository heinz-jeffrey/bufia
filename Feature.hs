module Feature
  ( Elt (..),
    Sys,
    Bundle (..),
    Struc (..),
    makeSystem,        -- Table -> Sys
    eltIsValue,        -- String -> Elt -> Bool
    eltIsFeature,      -- String -> Elt -> Bool
    eltToStr,          -- Elt -> String
    symbols,           -- Sys -> [String]
    features,          -- Sys -> [String]
    values,            -- Sys -> [String]
    elements,          -- Sys -> [Elt]
    symMap,            -- Sys -> Map String (Set Elt),      
    classMap,          -- Sys -> Map Elt (Set String),      
    ncMap,             -- Sys -> Map (Set Elt) (Set String),
    unifyMap,          -- Sys -> Map Elt (Set Elt)
    matchingSymbols,   -- Sys -> Set Elt -> [String]
    exciseNothings,    -- Ord a => Set (Maybe a) -> Set a
    minBundle,         -- Bundle
    unifiableElts,     -- Sys -> Bundle -> Set Elt
    minStruc,          -- Struc
    toStruc,           -- Sys -> [Symbol] -> Struc
    unStruc,           -- Struc -> [Bundle]
    strucSetToStr,     -- Set Struc -> String
    toStrucSet,    -- Sys -> [[Symbol]] -> Set Struc
    insertBundleMaybe, -- Int -> Elt -> Bundle -> Maybe Bundle
    bundleExtension,   -- Sys -> Bundle -> [Symbol]
    minExtension       -- Sys -> Struc -> Set [Symbol]
  ) where

import qualified Table as Table
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Base

type Set = Set.Set
type Map = Map.Map
type Table = Table.Table

type Symbol = String

--
-- Elements are feature-value pairs.
--

data Elt = Elt { feature :: String,
                 value :: String
               } deriving (Eq, Ord, Show, Read)

eltFromPair :: (String,String) -> Elt
eltFromPair (f,v) = Elt {feature = f, value = v}

makeElt :: String -> String -> Elt
makeElt f v = Elt {feature = f, value = v}

eltIsValue :: String -> Elt -> Bool
eltIsValue s e = value e == s

eltIsFeature :: String -> Elt -> Bool
eltIsFeature f e = feature e == f

eltToStr :: Elt -> String
eltToStr elt = (value elt) ++ (feature elt)

--
-- A feature system is basically a collection of compiled information
-- from a feature table (symbol columns, feature rows, value cells)
--

data Sys = Sys { symbols  :: [Symbol],
                 features :: [String],
                 values   :: [String],
                 elements :: [Elt],
                 symMap   :: Map Symbol (Set Elt),       -- maps each symbol to a set of elements (feature-value pairs)
                 classMap :: Map Elt (Set Symbol),       -- maps each element (feature-value pair) to a set of symbols
                 ncMap    :: Map (Set Elt) (Set Symbol), -- maps a set of elements (feature-value pairs) to a set of symbols
                 unifyMap :: Map Elt (Set Elt)           -- maps an element to a set of compatible/unifiable elements
               } deriving (Eq, Show, Read)

makeSystem :: Table -> Sys
makeSystem t =
  Sys
  { symbols  = syms,
    features = feats,
    values   = vals,
    elements = elts,
    symMap   = makeSymMap t,
    classMap = clmap,
    ncMap    = makeNCMap clmap (Set.fromList syms),
    unifyMap = makeUnifyMap clmap elts
  }
  where syms  = Table.colNames t
        feats = Table.rowNames t
        vals  = Table.values t
        elts  = makeElts t feats vals
        clmap = makeClassMap t
  
makeElts :: Table -> [String] -> [String] -> [Elt]
makeElts t fs vs = map eltFromPair [ (f,v) | f <- fs, v <- vs]

addSymElt :: Map String (Set Elt) -> (String,String,String) -> Map String (Set Elt)
addSymElt m (s,f,v) = 
  Map.insertWith Set.union s (Set.singleton (makeElt f v)) m

makeSymMap :: Table -> Map String (Set Elt)
makeSymMap t = List.foldl' addSymElt Map.empty t

addEltSym ::  Map Elt (Set String) -> (String,String,String) -> Map Elt (Set String)
addEltSym m (s,f,v) =
  Map.insertWith Set.union (makeElt f v) (Set.singleton s) m
  
makeClassMap :: Table -> Map Elt (Set String)
makeClassMap t = List.foldl' addEltSym Map.empty t

lookupElt :: Sys -> Elt -> Set String
lookupElt sys elt = maybe Set.empty id $ Map.lookup elt (classMap sys)

matchingSymbols :: Sys -> Set Elt -> [String]
matchingSymbols sys xs = Set.elems $ Set.foldl' (\ys x -> Set.intersection ys (lookupElt sys x)) (Set.fromList (symbols sys)) xs

-- baseNCMap only contains the empty bundle (size 0) which maps to all
-- symbols
baseNCMap :: Set String -> Map (Set Elt) (Set String)
baseNCMap symbols = Map.singleton Set.empty symbols


insertMapMaybe :: (Set Elt)
            -> Set String
            -> Elt
            -> Set String
            -> Map (Set Elt) (Set String)
            -> Map (Set Elt) (Set String)
insertMapMaybe k d k' d' m'
  | k == newBundle = m'         -- if the newBundle is the same as the oldBundle don't do anything
  | Set.null intersection = m'  -- if the intersection is empty then don't do anything
  | otherwise = Map.insert newBundle intersection m'
  where intersection = Set.intersection d d'
        newBundle = Set.insert k' k

        
-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
addToBundles :: Map Elt (Set String) -> Map (Set Elt) (Set String) -> Map (Set Elt) (Set String)
addToBundles classMap map = Map.foldrWithKey
                            (\k d m -> Map.foldrWithKey (insertMapMaybe k d) m classMap)
                            Map.empty
                            map 

makeNCMap :: Map Elt (Set String) -> Set String -> Map (Set Elt) (Set String) 
makeNCMap classMap symbols = closeMap (addToBundles classMap) [baseNCMap symbols]

-- the unifyMap ignores all zero-valued elements because they are
-- unifiable with everything.  Consequently if a unifyMap is searched
-- for a zero-valued element Nothing will be returned.  Care should be
-- taken on how to interpret Nothing : either all elements or the
-- Set.empty could be appropriate depending on the situation.
makeUnifyMap :: Map Elt (Set String) -> [Elt] -> Map Elt (Set Elt)
makeUnifyMap clmp elts = List.foldl' (addUnifiableElts clmp elts) Map.empty nonNilEltPairs
  where
    nonNilEltPairs =
      filter
      (\(e1,e2) -> value e1 /= "0" && value e2 /= "0" && e1 /= e2)
      [ (x,y) | x <- elts, y <- elts ]

-- if e1 is unifiable with e2 we add e2 to the set of elements compatible with e1.
addUnifiableElts :: Map Elt (Set String) -> [Elt] -> Map Elt (Set Elt) -> (Elt,Elt) -> Map Elt (Set Elt)
addUnifiableElts clmp elts m (e1,e2) =
  Map.insertWith
  Set.union
  e1 -- this is the key
  (coExistInSomeSymbol e2 (Map.lookup e1 clmp) (Map.lookup e2 clmp))
  m

coExistInSomeSymbol :: Elt -> Maybe (Set String) -> Maybe (Set String) -> Set Elt
coExistInSomeSymbol elt (Just xs) (Just ys) =
  if Set.null (Set.intersection xs ys)
  then Set.empty
  else Set.singleton elt
coExistInSomeSymbol _ _ _ = Set.empty

removeZeroElts :: Set Elt -> Set Elt
removeZeroElts = Set.filter (\x -> not (eltIsValue "0" x))

nonZeroElts :: Sys -> Set Elt
nonZeroElts sys = removeZeroElts $ Set.fromList (elements sys)

--
-- Bundles
--

data Bundle = Bundle (Set Elt) deriving (Eq, Show, Read)

unBundle :: Bundle -> Set Elt
unBundle (Bundle xs) = xs

bundleToStr :: Bundle -> String
bundleToStr (Bundle xs) = "[" ++ List.intercalate ", " contents ++ "]"
  where contents = map eltToStr (Set.toList xs)

minBundle :: Bundle
minBundle = Bundle Set.empty

-- maybe this should be called bundleMatchingSymbols?
bundleExtension :: Sys -> Bundle -> [Symbol]
bundleExtension sys (Bundle xs) = matchingSymbols sys xs

instance Ord Bundle where
  compare (Bundle xs) (Bundle ys)
    | Set.null xs && Set.null ys = EQ
    | (Set.size xs) < (Set.size ys) = LT
    | (Set.size xs) > (Set.size ys) = GT
    | minx < miny = LT
    | minx > miny = GT
    | otherwise = compare (Set.delete minx xs) (Set.delete miny ys)
    where minx = Set.findMin xs 
          miny = Set.findMin ys 

lookupUnifiable :: Sys -> Elt -> Set Elt
lookupUnifiable sys elt = maybe Set.empty id $ Map.lookup elt (Feature.unifyMap sys)

findUnifiable :: Sys -> Set Elt -> Elt -> Set Elt
findUnifiable sys elts elt = Set.intersection elts (lookupUnifiable sys elt)

-- Note we remove the zero-valued features because lookupUnifiable
-- will return Set.emtpty for elements not in unifyMap and we don't
-- care about the zero-valued features
unifiableElts :: Sys -> Bundle -> Set Elt
unifiableElts sys (Bundle elts) = Set.foldl' (findUnifiable sys) (nonZeroElts sys) (removeZeroElts elts)


insertBundleMaybe :: Int -> Elt -> Bundle -> Maybe Bundle
insertBundleMaybe maxBundleSize x (Bundle xs)
  | 1 + (Set.size xs) > maxBundleSize = Nothing
  | otherwise = Just (Bundle (Set.insert x xs))


-- nextBundles :: Bundle -> Set Bundle
-- nextBundles = nextGreaterThan

strucIsPrefixOf :: Struc -> Struc -> Bool
strucIsPrefixOf (Struc []) _                  =  True
strucIsPrefixOf _ (Struc [])                  =  False
strucIsPrefixOf (Struc (x:xs)) (Struc (y:ys)) = x <:< y && strucIsPrefixOf (Struc xs) (Struc ys)

strucIsInfixOf :: Struc -> Struc -> Bool
strucIsInfixOf needle (Struc haystack) = any (strucIsPrefixOf needle) (List.map (\x -> Struc x) (List.tails haystack))


instance DiscretePartialOrderWithMinimum Bundle where
  minimum = minBundle
  size (Bundle xs) = Set.size xs
  (<:<) (Bundle xs) (Bundle ys) = Set.isSubsetOf xs ys
  nextGreaterThan b@(Bundle xs) = exciseNothings (Set.map f (unifiableElts sys b))
    where f elt = insertBundleMaybe maxBundleSize elt b


--
-- Structures
--

data Struc = Struc [Bundle] deriving (Eq, Show, Read)

unStruc :: Struc -> [Bundle]
unStruc (Struc bs) = bs

strucToStr :: Struc -> String
strucToStr (Struc xs) = concat (map bundleToStr xs)

strucSetToStr :: Set Struc -> String
strucSetToStr xs = List.intercalate "\n" (List.map strucToStr (Set.toList xs))

minStruc :: Struc
minStruc = Struc []


instance Ord Struc where
  compare (Struc xsl) (Struc ysl)
    | List.null xsl && List.null ysl = EQ
    | (List.length xsl) < (List.length ysl) = LT
    | (List.length xsl) > (List.length ysl) = GT
    | headxs <  headys = LT
    | headxs >  headys = GT
    | otherwise = compare (Struc (List.tail xsl)) (Struc (List.tail ysl))
    where headxs = List.head xsl
          headys = List.head ysl


-- maybe this should be called minMatchingStrings?
minExtension :: Sys -> Struc -> Set [String]
minExtension sys (Struc []) = Set.empty
minExtension sys (Struc (b:[])) = Set.map (:[]) (Set.fromList (bundleExtension sys b))
minExtension sys (Struc (b:bs)) =  minExtension sys (Struc [b]) +++ minExtension sys (Struc bs)



-- getElts maps a string (symbol) to the set of features it has
-- given by the feature system.
getElts :: Sys -> Symbol -> Bundle
getElts sys x = Bundle (maybe Set.empty id elts)
  where elts = Map.lookup x (symMap sys)
-- The maybe function takes a default value, a function, and a Maybe
-- value. If the Maybe value is Nothing, the function returns the
-- default value. Otherwise, it applies the function to the value
-- inside the Just and returns the result.

-- We lift getElts to changing a list of symbols to a structure
toStruc :: Sys -> [Symbol] -> Struc
toStruc sys xs = Struc (map (getElts sys) xs)

-- We lift changeStructure to change a wordlist to a set of structures
toStrucSet :: Sys -> [[Symbol]] -> Set Struc
toStrucSet sys xss = Set.fromList (map (toStruc sys) xss)

-- memoize :: (Int -> a) -> (Int -> a)
-- memoize f = (map f [0 ..] !!)

-- memoize :: ([a] -> b) -> ([a] -> b)
-- memoize f = 

-- data MemoF a b = MF (a -> b, Map a b)

-- memoize :: a -> b -> MemoF a b
-- memoize f = MF (f, Map.empty)


-- For Testing
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
