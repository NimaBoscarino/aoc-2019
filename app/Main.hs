module Main where

-- import Lib
import Question5

import System.IO     

main :: IO ()
main = withFile "./inputs/5-a.txt" ReadMode (\handle -> do  
    contents <- hGetContents handle
    let val = question5a contents
    print val)