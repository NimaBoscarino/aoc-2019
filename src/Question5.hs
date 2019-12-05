module Question5
    ( 
        question5a,
    ) where

import Data.List.Split
import Data.Sort

question5a contents = do     
    let strings = splitOn "," contents   
    let nums = map (read :: String -> Int) strings
    let stuff = operate nums (head nums) (1 : (tail nums)) [] -- have to provide "1" as the first input
    stuff -- contains both the final ticker, and the log

operate :: [Int] -> Int -> [Int] -> [Int] -> [[Int]]
operate nums _  [] log = [nums, log]
operate nums 1  (a:b:c:xs) log      = operate (replace nums c (nums !! a + nums !! b)) (head xs) (tail xs) log
operate nums 2  (a:b:c:xs) log      = operate (replace nums c (nums !! a * nums !! b)) (head xs) (tail xs) log
operate nums 3  (input:a:xs) log    = operate (replace nums a input) (head xs) (tail xs) log
operate nums 4  (a:xs) log          = operate nums (head xs) (tail xs) ((nums !! a) : log)
operate nums 99 _ log               = [nums, log]
operate y _ x log = [y, log]

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/?st=k3o4an91&sh=2d316fdd
replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
