module Question4
    ( 
        question4a,
        question4b
    ) where

import Data.List.Split
import Data.Sort

parseDay4 :: String -> [Integer]
parseDay4 input = map (read :: String -> Integer) $ splitOn "-" input


-- map (read :: String -> Integer) $
genRange :: (String -> Bool) -> [Integer] -> [Integer]
genRange f (a:b:[]) = [ i |   i <- [a..b]
                            ,let si = show i
                            ,(sort si) == si
                            ,f si
                    ]

hasDupe (a:[]) = False
hasDupe (a:xs) = if a == (head xs) then True else (hasDupe xs)

question4a range = length $ genRange (hasDupe) $ parseDay4 range

hasStrictDupe :: String -> Bool
hasStrictDupe s = 2 `elem` getCount (foldl (
        \acc val -> if val == (getVal acc) then ((1 + (head $ getCount acc)):(tail $ getCount acc), val) else (1:(getCount acc), val)
    ) 
    ([0], '.') s)

getCount (c,_) = c
getVal (_,v) = v

question4b range = length $ genRange (hasStrictDupe) $ parseDay4 range
