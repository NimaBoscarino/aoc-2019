
module Lib
    ( 
        question1a,
        question1b,
        question2a,
        question2b
    ) where

import Data.List.Split
import Data.List
import Control.Arrow
import System.IO     
        
question1a = do     
    withFile "./inputs/1-a.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let strings = splitOn "\n" contents   
        let nums = map (read :: String -> Integer) strings
        let result = (map (`div` 3) >>> map (subtract 2) >>> foldl (\acc x -> acc + x) 0) nums
        print result)

fuelCalc x
    | fuel > 6 = fuel + fuelCalc fuel
    | otherwise = fuel
    where fuel = (x `div` 3) - 2 
    
question1b = do     
    withFile "./inputs/1-a.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let strings = splitOn "\n" contents   
        let nums = map (read :: String -> Integer) strings
        let result = foldl (\acc x -> acc + fuelCalc x) 0 nums
        print result)

question2a = do     
    withFile "./inputs/2-a.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let strings = splitOn "," contents   
        let nums = replace (replace (map (read :: String -> Int) strings) 1 12) 2 2
        let stuff = operate nums nums
        print $ stuff !! 0)

question2b = do     
    withFile "./inputs/2-a.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let strings = splitOn "," contents
        let uncooked = map (read :: String -> Int) strings
        let possible = [(i,j) | i <- [0..99], j <- [0..99] ]
        let val = find (\(x, y) -> operation2b x y uncooked) possible
        case val of
            Just value -> print (100 * fst value + snd value)
            Nothing    -> print "Nothing"
        )    

operation2b x y uncooked = do
    let nums = replace (replace uncooked 1 x) 2 y
    let stuff = operate nums nums
    stuff !! 0 == 19690720

operate :: [Int] -> [Int] -> [Int]
operate nums (a:b:c:d:xs) = case a of
    1 -> operate (replace nums d (nums !! b + nums !! c)) xs
    2 -> operate (replace nums d (nums !! b * nums !! c)) xs
    99 -> nums
operate y x = y

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/?st=k3o4an91&sh=2d316fdd
replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
