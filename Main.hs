-- the main file which runs the learning program

import Base
import qualified Table as Table
import qualified Reduce as Reduce
import qualified Learn as Learn
import Feature 


import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

type Map = Map.Map
type Set = Set.Set

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


instance DiscretePartialOrderWithMinimum Struc where
  minimum = minStruc
  -- size (Struc xs) = sum (map size xs)
  size (Struc xs) = List.length xs
  (<:<) = strucIsInfixOf
  nextGreaterThan (Struc xsl) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise] where
    adjoinLeft = Set.singleton $ Struc (minBundle:xsl)
    adjoinRight = Set.singleton $ Struc (xsl ++ [minBundle])
    addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply nextBundles xsl)
  
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

    
