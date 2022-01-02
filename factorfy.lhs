> import System.Environment (getArgs)
> import Data.List as List
> import Data.Set as Set
> import Base

> main :: IO ()
> main = putStr =<< f =<< getArgs
>     where f (d:k:o:[]) = main' k o <$> lines <$> readFile d
>           f _ = return $ unlines
>                 [ "usage:\tfactorfy datafile k order",
>                   "\tdatafile is a text file containing the data",
>                   "\tk is the factor width",
>                   "\torder is {s,p} for successor (local), precedence (piecewise) resp."
>                 ]

> main' :: String -> String -> [String] ->  String
> main' k o  = myshow . toFactors o' k' (<=) . List.map words
>   where k' = read k :: Int
>         o' = orderOfStr o

> myshow :: [[String]] -> String
> myshow = concat . List.map (unwords . (\x -> x ++ ["\n"]))
