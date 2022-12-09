module Main where

import Data.List ( nub ) 

main :: IO ()
main = do   
        str <- readFile "../../inputs/Day6.txt"
        putStrLn $ ("Day6.1: " ++) $ show $ solution  4  4 str
        putStrLn $ ("Day6.2: " ++) $ show $ solution 14 14 str

solution :: Eq a => Int -> Int -> [a] -> Int
solution len index str@(x:xs)
    | len /= length (nub (take len str))  = solution len (index+1) xs
    | otherwise                           = index
