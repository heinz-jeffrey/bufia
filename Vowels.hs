module Vowels
  (csv,
   wordlist
  ) where

csv,bcsv :: String
csv = ",i,u,e,o,a\nhigh,+,+,-,-,-\nback,-,+,-,+,-\nlow,-,-,-,-,+"
bcsv = ",i,u,e,o,a,#\nhigh,+,+,-,-,-,-\nback,-,+,-,+,-,-\nlow,-,-,-,-,+,-\nboundary,-,-,-,-,-,+"
datastring = "i i\na a\ne o"

  
wordlist :: [[String]]
wordlist = [["i","i"],
            -- ["i","u"],  [+back]
            -- ["i","e"],            [-high,-low]
            -- ["i","o"],  [+back]   [-high,-low]
            -- ["i","a"],                           [+high,-high]
            -- ["u","i"],  [+back]
            -- ["u","u"],  [+back]
            -- ["u","e"],  [+back]   [-high,-low]
            -- ["u","o"],  [+back]   [-high,-low]
            -- ["u","a"],  [+back]
            -- ["e","i"],            [-high,-low] 
            -- ["e","u"],  [+back]   [-high,-low] 
            -- ["e","e"],            [-high,-low]
            -- ["e","o"],  [+back]   [-high,-low]
            -- ["e","a"],            [-high,-low]
            -- ["o","i"],  [+back]   [-high,-low]
            -- ["o","u"],  [+back]   [-high,-low]
            -- ["o","e"],  [+back]   [-high,-low]
            -- ["o","o"],  [+back]   [-high,-low]
            -- ["o","a"],  [+back]   [-high,-low]
            -- ["a","i"],
            -- ["a","u"],  [+back]
            -- ["a","e"],            [-high,-low]
            -- ["a","o"],  [+back]   [-high,-low]
            ["a","a"]
           ]
  
