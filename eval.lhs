> {-|
> Program:    Eval
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> Eval reads a  featurefile, grammarfile, and wordfile
> and outputs which words violate which constraints
> in the grammar (if any).
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
> import qualified Feature as Feature (Sys,hread,adjustSys)
> import Struc
> import Data.List (intercalate)
> import qualified Data.Set as Set (Set,elems,filter)

> type Set = Set.Set
> type Sys = Feature.Sys


> main :: IO ()
> main = uncurry act =<< compilerOpts =<< getArgs


> act :: Options -> [String] -> IO ()
> act opts files
>     | optShowVersion opts          = printVersion
>     | optShowUsage opts            = printUsage
>     | opt_f opts > 2
>       || opt_f opts < 0            = printUsage >> exitFailure
>     | null files                   = printUsage >> exitFailure
>     | not . null $ drop 3 files    = printUsage >> exitFailure
>     | otherwise                    = do
>         fStr <- readFile (files !! 0)
>         gStr <- readFile (files !! 1)
>         wStr <- readFile (files !! 2)
>         let ord    = opt_order opts
>             sys    = Feature.hread Nothing Nothing fStr
>             newsys = (Feature.adjustSys (opt_f opts)) sys
>             grm    = Struc.setHread newsys gStr
>           in ( putStrLn
>              . intercalate "\n"
>              . map (showEval newsys ord)
>              . map (eval newsys ord grm)
>              . map words
>              . lines) wStr
>     where printUsage = putStr $ usageInfo usageHeader options


> compilerOpts :: [String] -> IO (Options, [String])
> compilerOpts argv
>     = case getOpt RequireOrder options argv   -- replace RequireOrder with Permute ?
>       of (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
>          (_, _, errs) -> ioError . userError $
>                          concat errs ++ usageInfo usageHeader options

> usageHeader :: String
> usageHeader = "Usage: eval [OPTIONS...] featurefile grammarfile wordfile"

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



`eval order grammar struc` returns a triple whose:
first element  is the struc itself
second element is the number of constraints it violates
third elementi is the list of constraints it violates.

> stringify :: [String] -> String
> stringify = intercalate " "

> eval :: Sys -> Order -> Set Struc -> [String] -> ([String], Int, [Struc])
> eval sys ord cs x = (x, length vs, vs)
>   where vs  = Set.elems $ Set.filter f cs
>         f c = Struc.isLessThan ord c (Struc.ofWord sys x)

> showEval :: Sys -> Order -> ([String], Int, [Struc]) -> String
> showEval sys ord (x,n,vs) = intercalate "\t" [stringify x, show n, Struc.listHshow sys vs]

