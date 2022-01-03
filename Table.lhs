> {-|
> Module:    Table
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module reads simple csv text files into a table 
> -}

> module Table
>   ( Table,
>     hread,
>     rowNames,
>     colNames,
>     values
>   ) where

> import qualified Split as Split

import qualified Data.Set as Set

> import Data.List (nub)


> sep = ','

> (>.>) :: (a -> b) -> (b -> c) -> (a -> c)
> (>.>) f g = g . f

> type Table = [(String,String,String)]

> hread :: String -> Table
> hread =
>    lines          >.>  -- String -> [String], creates a list at newlines
>    Split.cleanup  >.>  -- [String] -> [String], removes tabs and spaces
>    splitLines     >.>  -- [String] -> [[String]], creates a list at commas
>    prepRowCols    >.>  -- [[String]] -> ([String],[String],[[String]])
>    rowColVals     >.>  -- ([String],[String],[[String]]) -> [(String,String,String)] this produces (row,col,val)
>    map (\(f,s,v) -> (s,f,v)) -- [(String,String,String)] -> [(String,String,String)] reordering the tuple to (col,row,val)


settify removes duplicates and sorts lists

settify :: Ord a => [a] -> [a]
settify = Set.toList . Set.fromList 

> colNames :: Table -> [String]
> colNames = nub . map (\(c,_,_) -> c)

> rowNames :: Table -> [String]
> rowNames = nub . map (\(_,r,_) -> r)

> values :: Table -> [String]
> values = nub . map (\(_,_,v) -> v)

> splitLines :: [String] -> [[String]]
> splitLines = map (Split.bySep sep)

> takeCol :: [[a]] -> ([a],[[a]])
> takeCol xs = (map head xs, map tail xs)

1st coordinate is the row labels (in our feature work that is the features)
2nd coordinate is the col labels (in our feature work that is the symbols)
3rd coordinate is the values

> prepRowCols :: [[a]] -> ([a],[a],[[a]])
> prepRowCols xss = (tail col1, head cols, tail cols)
>   where (col1, cols) = takeCol xss

> rowColVals :: ([a], [b], [[c]]) -> [(a,b,c)] 
> rowColVals ([], _, _) = []
> rowColVals ((a:as), bs, (cs:css)) =
>   rcvhelp a bs cs ++ rowColVals (as, bs, css)

> rcvhelp :: a -> [b] -> [c] -> [(a,b,c)]
> rcvhelp _ [] _ = []
> rcvhelp a (b:bs) (c:cs) = (a,b,c) : (rcvhelp a bs cs)

