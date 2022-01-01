> {-|
> Program:    Bufia
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> ..description TBD..
> Right now this is a placeholder that we develop over time.
> 
> -}


> import System.Environment (getArgs)
> import qualified Feature as Feature (hread)
> import qualified Struc as Struc (hshow, ofWord)
> import Data.List (intercalate)
> 
> main :: IO ()
> main = putStrLn =<< f =<< getArgs

<1> where f (d:[]) = main' <$> readFile d

>     where f (df:ff:[]) = main' ff df
>           f _ = return $ unlines
>                 [ "usage:\tbufia datafile featurefile",
>                   "\tdatafile is a text file containing a list of words (one word per line, symbols in words separated by spaces",
>                   "\tfeaturefile is a text file containing a comma-delimited feature system"
>                 ]

<1> main' :: String -> String
<1a>main' = Feature.showSys . Feature.ofTable . Table.ofStr
<1b>main' = Feature.showSys . Feature.readSys
<1> 1a and 1b only differ in about 6s...

> main' :: String -> String -> IO String

> main' f d = do
>   fStr <- readFile f
>   dStr <- readFile d
>   let sys = Feature.hread fStr in
>     (return . intercalate "\n"
>      . map Struc.hshow
>      . map (Struc.ofWord sys)
>      . map words
>      . lines) dStr

