
import System.Environment (getArgs)
import System.Console.GetOpt ( ArgDescr(NoArg, ReqArg)
                             , ArgOrder(RequireOrder)
                             , OptDescr(Option)
                             , getOpt
                             , usageInfo
                             )
import System.Exit (exitFailure)
main :: IO ()
main = uncurry act =<< compilerOpts =<< getArgs


-- There is exactly one necessary argument: the data file.

act :: Options -> [String] -> IO ()
act opts files
    | optShowVersion opts        =  printVersion
    | optShowUsage opts          =  printUsage
    | null files                 =  printUsage >> exitFailure
    | not . null $ drop 1 files  =  printUsage >> exitFailure
    | otherwise                  =  print (opt_model opts) -- putStr $ bufia (contents datafile)
    where printUsage = putStr $ usageInfo usageHeader options

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv
    = case getOpt RequireOrder options argv
      of (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
         (_, _, errs) -> ioError . userError $
                         concat errs ++ usageInfo usageHeader options

usageHeader :: String
usageHeader = "Usage: bufia [OPTIONS...] datafile"

printVersion :: IO ()
printVersion = putStrLn "Version 0.1"

data Options = Options
  {optShowVersion   :: Bool
   , optShowUsage   :: Bool
   , opt_k          :: Int
   , opt_size       :: Int
   , opt_feat       :: Maybe String
   , opt_model      :: String
  }

defaultOptions :: Options
defaultOptions = Options
                 { optShowVersion    = False
                 , optShowUsage      = False
                 , opt_k             = 2
                 , opt_size          = 3
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
