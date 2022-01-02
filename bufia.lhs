> {-|
> Program:    Bufia
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> ..description TBD..
> Right now this is a placeholder that we develop over time.
> 
> -}


> import System.Environment (getArgs)
> import qualified Feature as Feature
> import qualified Struc as Struc
> import Data.List (intercalate)


> main :: IO ()
> main = putStrLn =<< f =<< getArgs

wf = wordfile
ff = feature file
o  = order
k  = factor width
n  = bundle depth
a  = abductiv principle code

>     where f (wf:ff:oStr:kStr:nStr:aStr:[]) = main' wf ff oStr kStr nStr aStr
>           f _ = return $ unlines
>                 [ "usage:\tbufia datafile featurefile",
>                   "\tdatafile is a text file containing a list of words (one word per line, symbols in words separated by spaces",
>                   "\tfeaturefile is a text file containing a comma-delimited feature system"
>                 ]

> main' :: String -> String -> String ->  
>          String -> String -> String -> IO String

> main' wf ff oStr kStr nStr aStr = do
>   fStr <- readFile ff
>   wStr <- readFile wf
>   let sys = Feature.hread fStr
>       ord = orderOfStr oStr 
>       k   = read kStr       :: Int
>       n   = read nStr       :: Int
>       a   = read aStr       :: Int
>       kws = kStrings (symbols sys)
>       nextGreaterThan = Struc.nextGreater' sys n
>   in (return
>       . intercalate "\n"
>       . Struc.setHshow
>       . learn sys ord k n abd kws nextGreaterThan
>       . Set.fromList
>       . map Struc.ofWord
>       . toFactors ord k (<=)
>       . map words
>       . lines) wStr


The functions below instantiate what is necessary to for the BUFIA
learning algorithm below.  There is one more `nextGreaterThan` which
here is defined internally within `main`

> type Factor = Struc.Struc
> type NextFactors = Factor -> Set Factor

> minFactor :: Factor
> minFactor = Struc.minStruc
> 
> (<:<) :: Factor -> Factor -> Bool
> (<:<) x y = x `Struc.isLessThan` y

> (<::<) :: Factor -> Set Factor -> Bool       
> (<::<) x ys = Set.foldr' foldfunction False ys
>  where
>    foldfunction y bool = bool || (x <:< y) 

> (>::>) :: Factor -> Set Factor -> Bool       
> (>::>) x ys = Set.foldr' foldfunction False ys
>   where
>     foldfunction y bool = bool || (y <:< x)



> learn :: Sys         ->
>          Order       ->  -- order (Succ or Prec)              fixed
>          Int         ->  -- maxFactorWidth (k value)          fixed
>          Int         ->  -- maxBundleDepth (n value)          fixed
>          Int         ->  -- abductive principle code          fixed
>          NextFactors ->  -- the "Next SuperFactors" function  fixed
>          (Set Struc) ->  -- posdata                           fixed 
>          (Set Struc)

> learn sys ord k n abd nextSupFac posdata =
>   learn' initialQ ord k n abd nextSupFac posData Set.empty minFactor Set.empty Set.empty
>     where initialQ = Queue.push minFactor Queue.empty

> learn' :: Queue a        -> -- queue                          updates
>           Order          -> -- order (Succ or Prec)           fixed
>           Int            -> -- maxFactorWidth (k value)       fixed
>           Int            -> -- maxBundleDepth                 fixed
>           Int            -> -- abductive principle code            fixed
>           NextFactors    -> -- the "Next SuperFactors" function    fixed 
>           (Set Struc)    -> -- positive data sample                    fixed
>           (Set [String]) -> -- negative data that is accounted for     updates
>           Factor         -> -- current structure                       updates
>           (Set Factor)   -> -- visited structures                           updates
>           (Set Factor)   -> -- discovered absent structures (constraints)   updates  
>           (Set Factor)      -- output grammar

> learn' queue ord k n abd nextSupFac posData negData struc visited constraints

     We stop if any of the following conditions are met:
     * the Queue is empty we stop.
     * if the struc is larger than k (since all subsequent strucs will also be larger than k)

>   | Queue.isEmpty queue 
>   || size struc > k      = constraints

     if any constraint is contained within the struc then we leave it
     and move on

>   | struc >::> constraints = learn' qs ord k n abd nextSupFac posData negData hd visited constraints

     if the struc is contained in the positive data then we add it to
     the visited structures (= not in the grammar) and move on

>   | struc <::< posData = learn' nextQ ord k nextSupFac posData negData hd (Set.insert struc visited) constraints

    Now the struc is not in the positive data and not already
    subsumed by an existing constraint so next we check which abductive principle applies

    if abductive principle 0 is selected then we add the struc to the
    constraints and we can ignore the negData

>   | abd == 0 =
>      learn' qs ord k n abd nextSupFac posData negData hd visited (Set.insert struc constraints)

     if abductive principle 1 is selected then we care
     whether the struc accounts for any new negData. If so, we add
     it to the constraints and add its extension to the negData

>   | abd == 1
>   && not (Set.isSubsetOf strucExt negData) =
>       learn' qs ord k abd nextSupFac posData (Set.union negData strucExt) hd visited (Set.insert struc constraints)

     if abductive principle 2 is selected then we care
     whether the struc only account for new negData. If so, we add it to
     the constraints and and add its extension to the negData
  
>   | abd == 2
>   && zeroIntersect strucExt negData =
>       learn' qs ord k abd nextSupFac posData (Set.union negData strucExt) hd visited (Set.insert struc constraints)

     Otherwise we have abdutive principle 1 or 2 AND the extension of
     struc is either already subsumed by the current grammar (1) or
     its extension overlaps with the current grammar (2) and will be
     skipped. Therefore we add the struc to the set of visited
     structures and move on

>   | otherwise =
>      learn' qs ord k n abd nextSupFac posData negData hd (Set.insert struc visited) constraints
>   where
>     (hd,qs) = Queue.pop queue
>     nextQ = nextQueue struc visited qs
>     strucExt = kExtension sys ord struc






I tried to compile the feature system ahead of time and then read it
as a file. I thought it would make a difference. It doesn't seem
too...

<1> where f (d:[]) = main' <$> readFile d

<1> main' :: String -> String
<1a>main' = Feature.showSys . Feature.makeSystem . Table.ofStr
<1b>main' = Feature.showSys . Feature.readSys
<1> 1a and 1b only differ in about 6s...
