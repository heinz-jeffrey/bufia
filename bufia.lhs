> {-|
> Program:    Bufia
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> ..description TBD..
> Right now this is a placeholder that we develop over time.
> 
> -}


> import System.Environment (getArgs)
> import Base
> import qualified PriorityQueue as Queue
> import qualified Feature as Feature
> import qualified Struc as Struc
> import qualified Data.List as List
> import qualified Data.Set as Set 

> type Queue = Queue.PriorityQueue
> type Set = Set.Set
> type Struc = Struc.Struc


> minFactor = Struc.minStruc

> main :: IO ()
> main = putStrLn =<< f =<< getArgs

wf = wordfile
ff = feature file
o = order
k  = factor width
n  = bundle depth
a  = abductiv principle code

>   where f (wf:ff:oStr:kStr:nStr:aStr:[]) = main' wf ff oStr kStr nStr aStr
>         f _ = return $ unlines
>               [ "usage:\tbufia wordfile featurefile o k n a",
>                 "\tdatafile is a text file containing a list of words (one word per line, symbols in words separated by spaces",
>                 "\tfeaturefile is a text file containing a comma-delimited feature system",
>                 "\to is one of {Succ,Prec}",
>                 "\tk is the factor width",
>                 "\tn is the bundle depth",
>                 "\ta is the abductive principle code {0,1,2}"
>               ]

> main' :: String -> String -> String ->  
>          String -> String -> String -> IO String

> main' wf ff oStr kStr nStr aStr = do
>   fStr <- readFile ff
>   wStr <- readFile wf
>   let sys = Feature.hread fStr :: Feature.Sys
>       ord = orderOfStr oStr    :: Order 
>       k   = read kStr          :: Int
>       n   = read nStr          :: Int
>       a   = read aStr          :: Int

>       pd :: Set Struc
>       pd  = (Set.fromList                         -- positive data
>              . List.map (Struc.ofWord sys)
>              . toFactors ord k (<=)
>              . List.map words
>              . lines) wStr

>       kws = kStrings k (Feature.symbols sys)      -- all strings of length k

        kstrucs = Set.map (Struc.ofWord sys) kws    -- strucs thereof

>       filterf Prec ws w = any (\x -> x `List.isSubsequenceOf` w) ws
>       filterf Succ ws w = any (\x -> x `List.isInfixOf` w) ws

>       nextGreaterThan = Struc.nextGreater' sys n  -- nextSupFac function
>       (<:<) = Struc.isLessThan ord                
>       (<::<) x ys = Set.foldr' f False ys
>         where f y bool = bool || (x <:< y) 
>       (>::>) x ys = Set.foldr' f False ys
>         where f y bool = bool || (y <:< x)
>       initialQ = Queue.push minFactor Queue.empty

>       learn :: Queue Struc -> (Set [String]) -> Struc -> Set Struc -> Set Struc -> Set Struc
>       learn queue negData struc visited constraints
>         | Queue.isEmpty queue                                                           --(1)
>         || Struc.size struc > k = constraints
>         | struc >::> constraints = learn qs negData hd visited constraints              --(2)
>         | struc <::< pd = learn nextQ negData hd (Set.insert struc visited) constraints    --(3)
>         | a == 0 = learn qs negData hd visited (Set.insert struc constraints)           --(4)
>         | a == 1                                                                        --(5)
>           && not (Set.isSubsetOf strucExt negData) =
>           learn qs (Set.union negData strucExt) hd visited (Set.insert struc constraints)  
>         | a == 2                                                                        --(6)
>           && zeroIntersect strucExt negData =
>           learn qs (Set.union negData strucExt) hd visited (Set.insert struc constraints) 
>         | otherwise = learn nextQ negData hd (Set.insert struc visited) constraints        --(7)
>         where
>           (hd,qs)    = Queue.pop queue
>           nextStrucs = Set.difference (nextGreaterThan struc) visited
>           nextQ      = Queue.pushMany (Set.toList nextStrucs) qs
>           strucExt   = Set.filter (filterf ord (Struc.minExtension sys struc)) kws
>       in return
>          (Struc.setHshow
>          (learn initialQ Set.empty minFactor Set.empty Set.empty))


     (1) We stop if any of the following conditions are met:
     * the Queue is empty we stop.
     * if the struc is larger than k (since all subsequent strucs will also be larger than k)

     (2) if any constraint is contained within the struc then we leave it and move on

     (3) if the struc is contained in the positive data then we add it to
     the visited structures (= not in the grammar) and move on

     (4) Now the struc is not in the positive data and not already
     subsumed by an existing constraint so next we check which abductive principle applies

     if abductive principle 0 is selected then we add the struc to the
     constraints and we can ignore the negData

     (5) if abductive principle 1 is selected then we care
     whether the struc accounts for any new negData. If so, we add
     it to the constraints and add its extension to the negData

     (6) if abductive principle 2 is selected then we care
     whether the struc only account for new negData. If so, we add it to
     the constraints and and add its extension to the negData
  
     (7) Otherwise we have abdutive principle 1 or 2 AND the extension of
     struc is either already subsumed by the current grammar (1) or
     its extension overlaps with the current grammar (2) and will be
     skipped. Therefore we add the struc to the set of visited
     structures and move on

