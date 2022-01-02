> {-|
> Program:    Eval
> Copyright: (c) 2021-2022 Jeffrey Heinz
> License:   MIT
> Eval reads a grammarfile, wordfile, and featurefile
> and outputs which words violate which constraints
> in the grammar (if any).
> -}


> import System.Environment (getArgs)
> import Base
> import qualified Feature as Feature (Sys,hread)
> import Struc
> import Data.List (intercalate)
> import qualified Data.Set as Set (Set,elems,filter)

> type Set = Set.Set
> type Sys = Feature.Sys
> 
> main :: IO ()
> main = putStrLn =<< f =<< getArgs

>     where f (gf:wf:ff:o:[]) = main' gf wf ff o
>           f _ = return $ unlines
>                 [ "usage:\teval grammarfile wordfile",
>                   "\tdatafile is a text file containing a list of words (one word per line, symbols in words separated by spaces",
>                   "\tfeaturefile is a text file containing a comma-delimited feature system"
>                 ]

> main' :: String -> String -> String -> String -> IO String

> main' gf wf ff o = do
>   gStr <- readFile gf
>   wStr <- readFile wf
>   fStr <- readFile ff
>   let ord = orderOfStr o
>       sys = Feature.hread fStr
>       grm = Struc.setHread gStr in
>     (return
>      . intercalate "\n"
>      . map (showEval ord)
>      . map (eval sys ord grm)
>      . map words 
>      . lines) wStr


`eval order grammar struc` returns a triple whose:
first element is the struc itself
second element is the number of constraints it violates
third elementi is the list of constraints it violates.

> stringify :: [String] -> String
> stringify = intercalate " "

> eval :: Sys -> Order -> Set Struc -> [String] -> ([String], Int, [Struc])
> eval sys ord cs x = (x, length vs, vs)
>   where vs  = Set.elems $ Set.filter f cs
>         f c = Struc.isLessThan ord c (Struc.ofWord sys x)

> showEval :: Order -> ([String], Int, [Struc]) -> String
> showEval ord (x,n,vs) = intercalate "\t" [stringify x, show n, Struc.listHshow vs]

newtype OrdStruc = SuccStruc Struc
newtype PrecStruc = PrecStruc Struc

instance DiscretePartialOrderWithMinimum SuccStruc where
  minimum = minStruc
  -- size (Struc xs) = sum (map size xs)
  size (Struc xs) = List.length xs
  (<:<) = strucIsInfixOf
  nextGreaterThan (Struc xsl) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise] where
    adjoinLeft = Set.singleton $ Struc (minBundle:xsl)
    adjoinRight = Set.singleton $ Struc (xsl ++ [minBundle])
    addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply nextBundles xsl)

instance DiscretePartialOrderWithMinimum PrecStruc where
  minimum = minStruc
  -- size (Struc xs) = sum (map size xs)
  size (Struc xs) = List.length xs
  (<:<) = strucIsSubsequenceOf
  nextGreaterThan (Struc xsl) = List.foldl Set.union Set.empty [adjoinLeft,adjoinRight,addEltsPointwise] where
    adjoinLeft = Set.singleton $ Struc (minBundle:xsl)
    adjoinRight = Set.singleton $ Struc (xsl ++ [minBundle])
    addEltsPointwise = Set.map (\xs -> Struc xs) (pointwiseApply nextBundles xsl)
