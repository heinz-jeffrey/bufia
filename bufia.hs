
import System.Environment (getArgs)
import System.Console.GetOpt ( ArgDescr(NoArg, ReqArg)
                             , ArgOrder(RequireOrder)
                             , OptDescr(Option)
                             , getOpt
                             , usageInfo
                             )
import System.Exit (exitFailure)

import Base
import qualified Table as Table
import qualified Learn as Learn
import Feature 

import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

type Map = Map.Map
type Set = Set.Set


main :: IO ()
main = uncurry act =<< compilerOpts =<< getArgs

-- There is exactly one necessary argument: the data file.

act :: Options -> [String] -> IO ()
act opts files
    | optShowVersion opts            = printVersion
    | optShowUsage opts              = printUsage
    | not ((opt_model opts == "sl")
      || (opt_model opts == "sp"))   = printUsage >> exitFailure
    | opt_abd opts > 3
      || opt_abd opts < 0            = printUsage >> exitFailure
    | null files                     = printUsage >> exitFailure
    | not . null $ drop 1 files      = printUsage >> exitFailure
    | opt_feat opts == Nothing       = printUsage >> exitFailure
    | otherwise                      = do
        x <- readFile wordfile
        f <- readFile featfile
        let wordlist = map words (lines x)
            -- converts the wordfile to a list of words [[String]]
            sys = (Feature.makeSystem . Table.ofStr) f
            -- converts the feature file to the feature system Sys
          in putStr $ bufia opts sys wordlist
          -- runs bufia with the options and prints the results to stdout
    where
      printUsage = putStr $ usageInfo usageHeader options
      wordfile = head files
      featfile = maybe "" id (opt_feat opts)

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv
    = case getOpt RequireOrder options argv
      of (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
         (_, _, errs) -> ioError . userError $
                         concat errs ++ usageInfo usageHeader options

usageHeader :: String
usageHeader = "Usage: bufia [OPTIONS...] -f featurefile wordfile"

printVersion :: IO ()
printVersion = putStrLn "Version 0.1"

data Options = Options
  {optShowVersion   :: Bool
   , optShowUsage   :: Bool
   , opt_k          :: Int
   , opt_size       :: Int
   , opt_abd        :: Int
   , opt_feat       :: Maybe String
   , opt_model      :: String
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
                 { optShowVersion    = False
                 , optShowUsage      = False
                 , opt_k             = 2
                 , opt_size          = 3
                 , opt_abd           = 0
                 , opt_model         = "sl"
                 , opt_feat          = Nothing
                 }

options :: [OptDescr (Options -> Options)]
options
    = [ Option ['k'] []
        (ReqArg (\f opts ->
                 opts { opt_k = read f })
                "INT"
        )
        "the max factor width as an integer"
      , Option ['n'] []
        (ReqArg (\f opts ->
                 opts { opt_size  = read f })
                "INT"
        )
        "the max number of features in a bundle"
      , Option ['a'] []
        (ReqArg (\f opts ->
                 opts { opt_abd  = read f })
                "INT"
        )
        "which abductive principle to use {0,1,2}"
      , Option ['f'] ["features"]
        (ReqArg (\f opts ->
                 opts { opt_feat  = Just f })
                 "FILE"
        )
        "an unquoted csv file including featural information"
      , Option ['m'] ["model"]
        (ReqArg (\f opts ->
                 opts { opt_model  = f })
                "MODEL"
        )
        "the type of model {sl,sp}"
      , Option ['h','?'] []
        (NoArg (\opts -> opts { optShowUsage = True }))
        "show this help"
      , Option ['v'] []
        (NoArg (\opts -> opts { optShowVersion = True }))
        "show version number"
      ]

bufia :: Options -> Sys -> [[String]] -> String
bufia opts sys wordlist = show grammar
  where grammar = Learn.learn k posData negData struc
        k = opt_k opts
        order = orderOfStr (opt_model opts)
        posData = 
          negData = 
          struc =
          
-- | opt_model opts == "sl" = show opts
-- | opt_model opts == "sp" = show opts
-- | otherwise = show opts



-- Next we reduce the data to factors of size k
-- Note we add word boundaries only for successor 

allKgrams :: Set String -> Set [String]
-- gives all words of length k from a set of symbols
-- (a symbol is of type String)
allKgrams symbols =
  Set.fromList
  $ map (map (:[]))
  $ Set.toList 
  $ (++++) (List.map Set.fromList (replicate k symbols))

type WordList = [[String]]

posKgrams :: Order -> WordList -> Set [String]
posKgrams order wordlist =
  Set.fromList
  $ Reduce.toFactors order k (map (f order) wordlist)
  where f Succ = addWBs
        f Prec = id

posdata :: Order -> Sys -> WordList -> Set Struc
posdata order sys wordlist =
  Set.map (toStruc sys) (posKgrams order wordlist)

negdata :: Order -> [String] -> WordList -> Set [String]
negdata order symbols wordlist =
  Set.difference (allKgrams symbols) (posKgrams order wordlist)

-- These run the learner

grammar :: Order -> Sys -> WordList -> Set Struc
grammar order sys wordlist = Learn.learn k pdata ndata minStruc
  where pdata = posdata order sys wordlist
        ndata = order (symbols sys) wordlist
          


--
--
--
--
--

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
    addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply nextBundles xsl)
  
nextStrucStrucs :: Struc -> Set Struc
nextStrucStrucs = nextGreaterThan
