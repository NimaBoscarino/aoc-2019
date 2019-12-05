module Question2
    ( 
        question2a,
        -- question2b,
    ) where

import Data.List.Split
import Data.Sort
import Data.List


-- question2b x = "hello world"


question2a contents = do     
    let strings = splitOn "," contents   
    let nums = replace (replace (map (read :: String -> Int) strings) 1 12) 2 2 -- replace is for setting input
    let stuff = operate nums (head nums) (tail nums)
    stuff

-- question2b contents = do
--     let strings = splitOn "," contents
--     let uncooked = map (read :: String -> Int) strings
--     let possible = [(i,j) | i <- [0..99], j <- [0..99] ]
--     let val = find (\(x, y) -> operation2b x y uncooked) possible
--     case val of
--         Just value -> 100 * fst value + snd value
--         Nothing    -> 0

-- operation2b x y uncooked = do
--     let nums = replace (replace uncooked 1 x) 2 y
--     let stuff = operate nums nums
--     stuff !! 0 == 19690720

operate :: [Int] -> Int -> [Int] -> [Int]
operate nums _  [] = nums
operate nums 1  (a:b:c:xs) = operate (replace nums c (nums !! a + nums !! b)) (head xs) (tail xs)
operate nums 2  (a:b:c:xs) = operate (replace nums c (nums !! a * nums !! b)) (head xs) (tail xs)
operate nums 99 _          = nums
operate y _ x = y

-- operate nums (a:b:c:d:xs) = case a of
--     1 -> operate (replace nums d (nums !! b + nums !! c)) xs
--     2 -> operate (replace nums d (nums !! b * nums !! c)) xs
--     99 -> nums

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/?st=k3o4an91&sh=2d316fdd
replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
