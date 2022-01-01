> {-|
> Module:    Split
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> This module is like `words` and `lines`
> except it splits Strings by a seperator of type Char
> -}

> module Split
>   ( bySep,
>     cleanup
>   ) where

> import Data.List (takeWhile,dropWhile)

> killchars   = "\t "

> takeUpTo :: Char -> String -> String
> takeUpTo sep = takeWhile (/= sep)

> dropUpTo :: Char -> String -> String
> dropUpTo sep xs = if it == "" then "" else tail it
>   where it = dropWhile (/= sep) xs

> notOneOf :: Eq a => [a] -> a -> Bool
> notOneOf xs x = not (elem x xs)

> bySep :: Char -> String -> [String]
> bySep _ [] = []
> bySep sep s = (takeUpTo sep s) : (bySep sep) (dropUpTo sep s)

> cleanup :: [String] -> [String]
> cleanup = map (filter (notOneOf killchars)) 
