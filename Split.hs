module Split
  ( bySep,
    cleanup
  ) where

sep = ','
killchars = "\t "

takeUpToSep :: String -> String
takeUpToSep [] = []
takeUpToSep (x:xs)
  | x == sep  = []
  | otherwise = x:(takeUpToSep xs)

dropUpToSep :: String -> String
dropUpToSep [] = []
dropUpToSep (x:xs)
  | x == sep  = xs
  | otherwise = dropUpToSep xs

notOneOf :: Eq a => [a] -> a -> Bool
notOneOf xs x = not (elem x xs)

bySep :: String -> [String]
bySep [] = []
bySep s = (takeUpToSep s) : bySep (dropUpToSep s)

cleanup :: [String] -> [String]
cleanup = map (filter (notOneOf killchars)) 
