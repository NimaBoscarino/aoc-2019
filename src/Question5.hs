module Question5
    ( 
        question5a
    ) where

import Data.List.Split
import Data.Sort
import Debug.Trace

data P_Mode = Pos | Im
    deriving (Show, Eq)

data I_Code = I_Code { opcode :: Int, modes :: [P_Mode] }

question5a contents = do     
    let strings = splitOn "," contents   
    let nums = map (read :: String -> Int) strings
    let firstI_Code = parseOpCode (head nums)
    let stuff = operate nums firstI_Code (1 : (tail nums)) [] -- have to provide "1" as the first input
    stuff -- contains both the final ticker, and the log

operate :: [Int] -> I_Code -> [Int] -> [Int] -> [[Int]]
operate nums _ [] log = [nums, log]
operate nums (I_Code 1 p_m)  (a:b:c:xs) log      = do
    let newNums = (replace nums c ((useMode nums a $ p_m !! 0) + (useMode nums b $ p_m !! 1)))
    let newXs = lastN (length xs) newNums
    operate newNums (nextI_Code newXs) (tail newXs) log
operate nums (I_Code 2 p_m)  (a:b:c:xs) log      = do
    let newNums = (replace nums c ((useMode nums a $ p_m !! 0) * (useMode nums b $ p_m !! 1)))
    let newXs = lastN (length xs) newNums
    operate newNums (nextI_Code newXs) (tail newXs) log
operate nums (I_Code 3 p_m)  (input:a:xs) log    = do
    let newNums = (replace nums a input)
    let newXs = lastN (length xs) newNums
    operate newNums (nextI_Code xs) (tail newXs) log
operate nums (I_Code 4 p_m)  (a:xs) log          = operate nums (nextI_Code xs) (tail xs) ((useMode nums a $ p_m !! 0) : log)
operate nums (I_Code 99 _) _ log                 = [nums, log]
operate nums (I_Code hmm _)  x log               = [nums, (hmm:log)]

useMode nums index Pos = do
    let i = trace ("INDEX " ++ show index) $ index
    nums !! i
useMode nums val Im = val

nextI_Code l = parseOpCode $ head l

getParamLength op
    | op == 1 = 3
    | op == 2 = 3
    | op == 3 = 1
    | op == 4 = 1
    | op == 99 = 0

-- parseOpCode :: Int -> I_Code
parseOpCode x = do
    let asString = show x
    let opCode = getOpcode asString
    let opCode = trace ("OP CODE " ++ asString) $ getOpcode asString

    let paramLength = getParamLength opCode
    let padded = leftPad asString (paramLength + 2) "0" -- 2 is to accomodate the opcode digits
    let p_modes = map (\x -> if x == '0' then Pos else Im) (take paramLength padded)
    I_Code opCode $ reverse p_modes

-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz4dcu5/?st=k3o4an91&sh=2d316fdd
replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

leftPad :: String -> Int -> String -> String
leftPad s l "" = leftPad' s l ' '
leftPad s l (c:[]) = leftPad' s l c

leftPad' s 0 c = s
leftPad' s n c = if length s < n then [c] ++ leftPad' s (n - 1) c else leftPad' s (n - 1) c

getOpcode = (read :: String -> Int) . lastN 2

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs