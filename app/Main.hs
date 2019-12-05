module Main where

-- import Lib
import Question2

import System.IO     

main :: IO ()
main = withFile "./inputs/2-a.txt" ReadMode (\handle -> do  
    contents <- hGetContents handle
    let val = question2b contents
    print val)