> {-|
> Program:   Inspect
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> Inspect reads a nonnegative integer k, 
> and a featurefile, a grammarfile,
> and outputs lines of comma seperated values which show,
> for each constraint C in the grammar file,
> (those constraints being obtained in the order they are listed)
> the number of k-grams C forbids,
> the number of those k-grams which are new to the grammar,
> the number of those k-grams which are already in the grammar,
> the total number of k-grams forbidden by the grammar
> which now includes C.
>
> If using the command line it is helpful to pipe it
> into a utility for pretty printing.
> For example:
> 
> ./inspect 3 featurefile grammarfile | column -s\; -t
>
> -}

> import System.Environment (getArgs)
> import System.Console.GetOpt ( ArgDescr(NoArg, ReqArg)
>                              , ArgOrder(RequireOrder)
>                              , OptDescr(Option)
>                              , getOpt
>                              , usageInfo
>                              )
> import System.Exit (exitFailure)
> import Base
> import qualified Feature as Feature (Sys (..), hread, adjustSys)
> import qualified Struc as Struc (setHread,hread,extension,hshow)
> import Data.List (intercalate, mapAccumL)
> import qualified Data.Set as Set ( Set
>                                  , elems
>                                  , filter
>                                  , fromList
>                                  , size
>                                  , difference
>                                  , intersection
>                                  , union
>                                  , empty
>                                  )

> type Set = Set.Set
> type Sys = Feature.Sys


%%% THE MAIN FUNCTION  (`act` is the real main, see below)

> main :: IO ()
> main = uncurry act =<< compilerOpts =<< getArgs

%%% DEALING WITH OPTIONS

> compilerOpts :: [String] -> IO (Options, [String])
> compilerOpts argv
>     = case getOpt RequireOrder options argv   -- replace RequireOrder with Permute ?
>       of (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
>          (_, _, errs) -> ioError . userError $
>                          concat errs ++ usageInfo usageHeader options

> usageHeader :: String
> usageHeader = "Usage: inspect [OPTIONS...] k featurefile grammarfile"

> printVersion :: IO ()
> printVersion = putStrLn "Version 1.0"

> data Options = Options
>   {optShowVersion   :: Bool
>    , optShowUsage   :: Bool
>    , opt_f          :: Int
>    , opt_order      :: Order
>   } deriving Show

> defaultOptions :: Options
> defaultOptions = Options
>                  { optShowVersion    = False
>                  , optShowUsage      = False
>                  , opt_f             = 0
>                  , opt_order         = Succ
>                  }

> options :: [OptDescr (Options -> Options)]
> options
>     = [ Option ['f'] []
>         (ReqArg (\f opts ->
>                  opts { opt_f = read f })
>                 "Int"
>         )
>         "how feature-values are ordered {0,1,2} (default 0)"
>       , Option ['o'] ["order"]
>         (ReqArg (\f opts ->
>                  opts { opt_order = orderOfStr f })
>                 "Order"
>         )
>         "the order of the word model: 'succ' or 'prec'"
>       , Option ['h','?'] []
>         (NoArg (\opts -> opts { optShowUsage = True }))
>         "show this help"
>       , Option ['v'] []
>         (NoArg (\opts -> opts { optShowVersion = True }))
>         "show version number"
>       ]


%%% THE REAL PROGRAM

> act :: Options -> [String] -> IO ()
> act opts args
>     | optShowVersion opts          = printVersion
>     | optShowUsage opts            = printUsage
>     | opt_f opts > 2
>       || opt_f opts < 0            = printUsage >> exitFailure
>     | length args /= 3             = printUsage >> exitFailure
>     | otherwise                    = do
>         fStr <- readFile (args !! 1)
>         gStr <- readFile (args !! 2)
>         let
>           k      = read (args !! 0)
>           ord    = opt_order opts
>           sys    = Feature.hread Nothing Nothing fStr
>           newsys = (Feature.adjustSys (opt_f opts)) sys
>           grm    = Struc.setHread newsys gStr
>           symbs  = Feature.symbols newsys
>           in ( putStrLn
>                . intercalate "\n"
>                . addheader
>                . map (showsizes newsys)
>                . scanl1 inspect
>                . map (embiggen ord newsys k)
>                . map (Struc.hread newsys)
>                . lines) gStr
>           where printUsage = putStr $ usageInfo usageHeader options


`makethem k symbols [[]]` returns all logically possible kgrams out of
the `symbols`.

> makethem :: Int -> [b] -> [[b]] -> [[b]]
> makethem 0 bs grams = grams
> makethem k bs grams = makethem (k-1) bs [ (b:gram) | b <- bs, gram <- grams]

`extension ord sys k x` gives the set of words of size k that match struc x.

mapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
scanl :: (b -> a -> b) -> b -> [a] -> [b]

scanl inspect Set.empty [(struc, extensions)]
scanl1 :: (a -> a -> a) -> [a] -> [a]

embiggen
  :: Order
  -> Feature.Sys
  -> Int
  -> Struc.Struc
  -> ( Struc.Struc
     , Feature.Set [Symbol]
     , Feature.Set [Symbol]
     , Set.Set a1
     , Set.Set a2
     )

(constraint, its extension, new ones, old ones, grammar including current constraint)

> embiggen ord sys k s = ( s , es , es, Set.empty, es)
>   where es = Struc.extension ord sys k s

inspect
  :: Ord a1 =>
  (a2, b, Set.Set a1, d1, e1)
  -> (a3, Set.Set a1, c, d2, e2)
  -> (a3, Set.Set a1, Set.Set a1, Set.Set a1, Set.Set a1)

> inspect
>   ( _ , _  , _ , _ , g )
>   ( s', es', _ , _ , _ )
>   =
>   ( s'
>   , es'
>   , Set.difference es' g
>   , Set.intersection g es'
>   , Set.union es' g )

s   - struc
es  - extension of struc
new - subset of es that is not in g (if any)
old - subset of es that belongs to g (if any)
g   - exenstion of grammar

> showsizes sys (s, es, new, old, g) = intercalate ";"
>   [ Struc.hshow sys s
>   , show (Set.size es)
>   , show (Set.size new)
>   , show (Set.size old)
>   , show (Set.size g)
>   ]
>
>
> addheader xs = header:xs
>   where header = "constraint;extension;new;old;grammar"
