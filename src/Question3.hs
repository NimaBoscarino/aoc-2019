module Question3
    ( 
        question3a,
        question3b
    ) where

import Data.List.Split
import Data.List
import System.IO    
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Data.Maybe

data Direction = R | U | L | D
    deriving (Show, Eq)
data Move = Move { direction :: Direction, distance :: Float}
    deriving (Show)
type Moves = [Move]

data Coord = Coord { x :: Float, y :: Float }
    deriving (Show, Eq)
data Segment = Segment { start :: Point, end :: Point , cost :: Float }
    deriving (Show)
type Coords = [Coord]

makeDir :: Char -> Direction
makeDir d
    | d == 'R' = R
    | d == 'U' = U
    | d == 'L' = L
    | d == 'D' = D

stringToMove :: String -> Move
stringToMove (d:dist) = Move (makeDir d) $ (read :: String -> Float) dist

calcNewCoord :: Point -> Move -> Point
calcNewCoord (x,y) m
    | dir == R  = (x + dist, y)
    | dir == L  = (x - dist, y)
    | dir == U  = (x, y + dist)
    | dir == D  = (x, y - dist)
    where   dir = direction m
            dist = distance m

computeVisited :: Moves -> [Segment]
computeVisited ms = foldl (
    \acc val -> acc ++ [
        Segment
        (end $ last acc)
        (calcNewCoord (end $ last acc) val)
        (distance val + (cost $ last acc))
    ]) [Segment (0, 0) (0, 0) 0] ms

parseDay3 :: String -> [Moves]
parseDay3 input = map (\x -> map stringToMove x) (map (splitOn ",") (splitOn "\n" input))

intersectionsWith :: Coord -> [Coord] -> [Coords]
intersectionsWith x b = []

createCombinations x y = [(i,j) | i <- x, j <- y]

computeIntersections (a:b:[]) = createCombinations a b

checkIntersect (a, b) = intersectSegSeg (start a) (end a) (start b) (end b)
checkIntersectWCost (a, b) = (intersectSegSeg (start a) (end a) (start b) (end b), a, b)

manhattan :: Maybe (Point) -> Float
manhattan (Just (a,b)) = abs a + abs b

question3a x = minimum $ map manhattan $ filter (\x -> not $ isNothing x) $ map checkIntersect (computeIntersections $ map computeVisited $ parseDay3 x)

calcActualCost (Just (x, y), a, b) = (cost a) + (cost b) - abs ((getX $ end a) - x) - abs ((getX $ end b) - x) - abs ((getY $ end a) - y) - abs ((getY $ end b) - y)

getX (x, _) = x
getY(_, y) = y

question3b x = minimum $ filter (\x -> x /= 0) $ map calcActualCost $ filter (\(x,_,_) -> not $ isNothing x) $ map checkIntersectWCost $ computeIntersections $ map computeVisited $ parseDay3 x
