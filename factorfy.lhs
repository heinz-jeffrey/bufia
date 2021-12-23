> import System.Environment (getArgs)
> import Data.List as List
> import Data.Set as Set

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
> main' k o  = myshow . toFactors o' k' . List.map words
>   where k' = read k :: Int
>         o' = orderOfStr o

> data Order = Succ | Prec

> orderOfStr :: String -> Order
> orderOfStr "p" = Prec
> orderOfStr "s" = Succ
> orderOfStr _ = error "order must be either p or s"

> extract :: Order -> ([a] -> [[a]])
> extract Prec = subsequences
> extract Succ = concat . List.map tails . inits

> factors' :: Ord a => Order -> Int -> [a] -> Set [a]
> factors' order k = Set.filter bylength . Set.fromList . extract order
>   where bylength w = len <= k && len > 0
>                      where len = length w

> factors :: Ord a => Order -> Int -> [[a]] -> Set [a]
> factors order k = List.foldl' Set.union Set.empty . List.map (factors' order k)

> toFactors :: Ord a => Order -> Int -> [[a]] -> [[a]]
> toFactors order k = Set.toList . factors order k

> myshow :: [[String]] -> String
> myshow = concat . List.map (unwords . (\x -> x ++ ["\n"]))
