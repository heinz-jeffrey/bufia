-- the main file which runs the learning program

import ClassExt
import qualified Table as Table
import qualified Reduce as Reduce
import qualified Sequence as Sequence
import qualified LearnExt as Learn
import Feature 

-- uncomment only the one below you want to run!

-- import qualified Test as Language
-- import qualified Quechua as Language
-- import qualified Polish as Language
import qualified English as Language

import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

type Map = Map.Map
type Set = Set.Set

-- type Elt = Feature.Elt
-- type Sys = Feature.Sys
-- type Struc = Struc
-- type Bundle = Bundle

-- We set up our basic parameters

k, maxBundleSize :: Int
k = 3
maxBundleSize = 3

-- Next we reduce the data to factors of size k Note we add word
-- boundaries only for successor the precData is only for Quechua. It
-- and other 'prec' statements should be removed when running the
-- other languages.

addWBs :: [String] -> [String]
addWBs xs = ["#"] ++ xs ++ ["#"]

-- succOrder = (Reduce.orderOfStr "succ") 
-- precOrder = (Reduce.orderOfStr "prec")

allKgrams :: Set [String]      -- gives all strings of length k
allKgrams = Set.fromList
            $ map (map (:[]))
            $ Set.toList 
            $ (++++) (List.map Set.fromList (replicate k (symbols sys))) 

succPosKgrams,precPosKgrams :: Set [String]
succPosKgrams = Set.fromList
                $ Reduce.toFactors Reduce.Succ k (map addWBs Language.wordlist)
precPosKgrams = Set.fromList
                $ Reduce.toFactors Reduce.Prec k Language.wordlist


succPosData,precPosData :: Set Struc
succPosData = Set.map (toStruc sys) succPosKgrams
precPosData = Set.map (toStruc sys) precPosKgrams

succNegData,precNegData :: Set [String]
succNegData = Set.difference allKgrams succPosKgrams
precNegData = Set.difference allKgrams precPosKgrams

-- These run the learner

precGrammar = Learn.learn k precPosData precNegData minStruc
succGrammar = Learn.learn k succPosData succNegData minStruc

-- our main function prints the results.

main = do
--  putStrLn "=====Forbidden Precedence Structures=====\n"
--  putStrLn (Feature.strucSetToStr precGrammar) -- for pretty printing
-- putStrLn (show precGrammar) -- for future usage if we later want to 'read' the grammar and use it
--  putStrLn "\n\n"

  putStrLn "=====Forbidden Successor Structures=====\n"
  putStrLn (Feature.strucSetToStr succGrammar) -- for pretty printing
  -- putStrLn (show succGrammar) -- for future usage if we later want to 'read' the grammar and use it

--
-- BETTER NOT TO MESS WITH ANYTHING BELOW HERE
--

sys :: Sys
sys = (Feature.makeSystem . Table.ofStr) Language.csv

instance DiscretePartialOrderWithMinimum Bundle where
  minimum = minBundle
  size (Bundle xs) = Set.size xs
  (<:<) (Bundle xs) (Bundle ys) = Set.isSubsetOf xs ys
  nextGreaterThan b@(Bundle xs) = exciseNothings (Set.map f (unifiableElts sys b))
    where f elt = insertBundleMaybe maxBundleSize elt b

nextBundles :: Bundle -> Set Bundle
nextBundles = nextGreaterThan

strucIsPrefixOf :: Struc -> Struc -> Bool
strucIsPrefixOf (Struc []) _                  =  True
strucIsPrefixOf _ (Struc [])                  =  False
strucIsPrefixOf (Struc (x:xs)) (Struc (y:ys)) = x <:< y && strucIsPrefixOf (Struc xs) (Struc ys)

strucIsInfixOf :: Struc -> Struc -> Bool
strucIsInfixOf needle (Struc haystack) = any (strucIsPrefixOf needle) (List.map (\x -> Struc x) (List.tails haystack))

instance DiscretePartialOrderWithMinimum Struc where
  minimum = minStruc
  -- size (Struc xs) = sum (map size xs)
  size (Struc xs) = List.length xs
  (<:<) = strucIsInfixOf
  nextGreaterThan (Struc xsl) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise] where
    adjoinLeft = Set.singleton $ Struc (minBundle:xsl)
    adjoinRight = Set.singleton $ Struc (xsl ++ [minBundle])
    addEltsPointwise = Set.map (\xs -> Struc xs) (Sequence.pointwiseApply nextBundles xsl)
  
nextStrucStrucs :: Struc -> Set Struc
nextStrucStrucs = nextGreaterThan

  

--
-- Our grammars will be of type Set Struc
--

-- for now we assume a connected successor structure

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
(>.>) f g = g . f

instance Grammar Struc where
  recognize struc word = struc <:< (toStruc sys word)
  extension k struc
    | length (unStruc struc) > k = Set.empty
    | otherwise = List.foldl' Set.union Set.empty                 -- I have got to clean this up. 
                  $ map (++++)                                    -- All this map (map (... ))
                  $ map (map Set.fromList)                        -- is ridiculous
                  $ map (map (map (:[])))
                  $ map (map (bundleExtension sys))
                  $ infixate minBundle (unStruc struc) k

    
