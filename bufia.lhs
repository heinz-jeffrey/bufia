> {-|
> Module:    Base
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
>
> This program implements BUFIA (Chandlee et al. 2019)
> for models of words that use either successor or precedence
> for order, and represent symbols with sets of properties
> (e.g. phonological features) 
>
> -}

{-# LANGUAGE BangPatterns #-}

> import System.Environment (getArgs)
> import System.Console.GetOpt ( ArgDescr(NoArg, ReqArg)
>                              , ArgOrder(RequireOrder)
>                              , OptDescr(Option)
>                              , getOpt
>                              , usageInfo
>                              )
> import System.Exit (exitFailure)

> import Base
> import qualified PriorityQueue as Queue
> import qualified Feature as Feature
> import qualified Struc as Struc
> import qualified Data.List as List
> import qualified Data.Set as Set 

> type Queue = Queue.PriorityQueue
> type Set = Set.Set
> type Struc = Struc.Struc

> initialQ :: Queue Struc
> initialQ = Queue.push minFactor Queue.empty

> minFactor :: Struc
> minFactor = Struc.minStruc


> main :: IO ()
> main = uncurry act =<< compilerOpts =<< getArgs

-- There are exactly two necessary arguments: the data file and the feature file.

> act :: Options -> [String] -> IO ()
> act opts files
>     | optShowVersion opts          = printVersion
>     | optShowUsage opts            = printUsage
>     | opt_a opts > 2
>       || opt_a opts < 0            = printUsage >> exitFailure
>     | null files                   = printUsage >> exitFailure
>     | not . null $ drop 2 files    = printUsage >> exitFailure
>     | otherwise                    = do
>         wStr <- readFile (files !! 0)
>         fStr <- readFile (files !! 1)             
>         putStrLn . Struc.setHshow $ 
>           learn wStr fStr opts initialQ Set.empty minFactor Set.empty Set.empty
>     where printUsage = putStr $ usageInfo usageHeader options

> compilerOpts :: [String] -> IO (Options, [String])
> compilerOpts argv
>     = case getOpt RequireOrder options argv   -- replace RequireOrder with Permute ?
>       of (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
>          (_, _, errs) -> ioError . userError $
>                          concat errs ++ usageInfo usageHeader options

> usageHeader :: String
> usageHeader = "Usage: bufia [OPTIONS...] wordfile featurefile"

> printVersion :: IO ()
> printVersion = putStrLn "Version 1.0"

> data Options = Options
>   {optShowVersion   :: Bool
>    , optShowUsage   :: Bool
>    , opt_k          :: Int
>    , opt_n          :: Int
>    , opt_a          :: Int
>    , opt_m          :: Maybe Int
>    , opt_b          :: Bool
>    , opt_order      :: Order
>   } deriving Show

> defaultOptions :: Options
> defaultOptions = Options
>                  { optShowVersion    = False
>                  , optShowUsage      = False
>                  , opt_k             = 3
>                  , opt_n             = 3
>                  , opt_a             = 1
>                  , opt_m             = Nothing
>                  , opt_b             = True
>                  , opt_order         = Succ
>                  }

> options :: [OptDescr (Options -> Options)]
> options
>     = [ Option ['k'] []
>         (ReqArg (\f opts ->
>                  opts { opt_k = read f })
>                 "Int"
>         )
>         "the max factor width (k-value, default 3)"
>       , Option ['n'] []
>         (ReqArg (\f opts ->
>                  opts { opt_n = read f })
>                 "Int"
>         )
>         "the max number of features in a bundle (default 3)"
>       , Option ['a'] []
>         (ReqArg (\f opts ->
>                  opts { opt_a = read f })
>                 "Int"
>         )
>         "which abductive principle to use {0,1,2} (default 1)"
>       , Option ['m'] []
>         (ReqArg (\f opts ->
>                  opts { opt_m = Just (read f :: Int) })
>                 "Maybe Int"
>         )
>         "the max number of constraints to return (default None)"
>       , Option ['b'] []
>         (ReqArg (\f opts ->
>                  opts { opt_b = read f })
>                 "Bool"
>         )
>         "If 'True' then boundaries '#' are added to all words (default True)"
>       , Option ['o'] ["order"]
>         (ReqArg (\f opts ->
>                  opts { opt_order = orderOfStr f })
>                 "Order"
>         )
>         "the order of the model: 'succ' or 'prec' (default succ)"
>       , Option ['h','?'] []
>         (NoArg (\opts -> opts { optShowUsage = True }))
>         "show this help"
>       , Option ['v'] []
>         (NoArg (\opts -> opts { optShowVersion = True }))
>         "show version number"
>       ]

> addwbs :: Bool -> [String] -> [String]
> addwbs False xs = xs
> addwbs True xs = ("#":xs) ++ ["#"]

> learn :: String
>       -> String
>       -> Options
>       -> Queue Struc
>       -> (Set [String])
>       -> Struc
>       -> Set Struc
>       -> Set Struc
>       -> Set Struc

> learn wStr fStr opts queue negData struc visited constraints =
>   let sys = Feature.hread fStr 
>       ord = opt_order opts
>       k   = opt_k opts
>       n   = opt_n opts
>       a   = opt_a opts    
>       m   = opt_m opts
>       b   = opt_b opts
>       pd :: Set Struc          -- positive data
>       pd  = (Set.fromList                         
>               . List.map (Struc.ofWord sys)
>               . reduceWords ord k
>               . List.map (addwbs b)
>               . List.map words
>               . lines) wStr

       kws = kStrings k (Feature.symbols sys)      -- all strings of length k

>       filterf :: Ord a => Order -> Set [a] -> [a] -> Bool
>       filterf Prec ws x = any (\w -> w `List.isSubsequenceOf` x) ws
>       filterf Succ ws x = any (\w -> w `List.isInfixOf` x) ws
>       nextGreaterThan :: Struc -> Set Struc
>       nextGreaterThan = Struc.nextGreater' sys n
>       (<:<) = Struc.isLessThan ord                
>       (<::<) x ys = any (\y -> x <:< y) ys
>       (>::>) x ys = any (\y -> y <:< x) ys
>       learn' :: Queue Struc -> (Set [String]) -> Struc -> Set Struc -> Set Struc -> Set Struc
>       learn' !queue !negData !struc !visited !constraints
>         | Queue.isEmpty queue                                                            --(1)
>           || Struc.size struc > k
>           || Just (Set.size constraints) == m = constraints
>         | struc >::> constraints = learn' qs negData hd visited constraints              --(2)
>         | struc <::< pd = learn' nextQ negData hd (Set.insert struc visited) constraints --(3)
>         | a == 0 = learn' qs negData hd visited (Set.insert struc constraints)           --(4)
>         | a == 1                                                                         --(5)
>           && not (Set.isSubsetOf strucExt negData) =
>           learn' qs (Set.union negData strucExt) hd visited (Set.insert struc constraints)  
>         | a == 2                                                                         --(6)
>           && zeroIntersect strucExt negData =
>           learn' qs (Set.union negData strucExt) hd visited (Set.insert struc constraints) 
>         | otherwise = learn' nextQ negData hd (Set.insert struc visited) constraints     --(7)
>         where
>           (hd,qs)    = Queue.pop queue
>           nextStrucs = Set.difference (nextGreaterThan struc) visited
>           nextQ      = Queue.pushMany (Set.toList nextStrucs) qs
>           strucExt   = Struc.extension ord sys k struc
>     in learn' initialQ Set.empty minFactor Set.empty Set.empty



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
