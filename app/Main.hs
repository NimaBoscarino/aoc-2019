module Main where

-- import Lib
import Question3

import System.IO     

main :: IO ()
main = withFile "./inputs/3-a.txt" ReadMode (\handle -> do  
    contents <- hGetContents handle
    let val = question3b contents
    print val)