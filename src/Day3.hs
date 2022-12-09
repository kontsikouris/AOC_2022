module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
        nums  <- map (map priority) . words <$> readFile "inputs/Day3.txt"
        putStrLn $ ("Day3.1: "++) $ show $ sum $ map common1 nums
        putStrLn $ ("Day3.2: "++) $ show $ sum $ map common2 $ combine3 nums

splitInHalf :: [a] -> ([a], [a])
splitInHalf str =  splitAt (length str `div` 2) str

combine3 :: [a] -> [[a]]
combine3 [] = []
combine3 (x:y:z:xs) = [x,y,z] : combine3 xs

priority :: Char -> Int
priority c
    | c `elem` ['a'..'z']   = ord c - ord 'a' + 1
    | c `elem` ['A'..'Z']   = ord c - ord 'A' + 27
    | otherwise             = undefined

sortedListsCommon :: Ord a => [a] -> [a] -> [a]
sortedListsCommon (x:xs) (y:ys)
    | x < y     = sortedListsCommon xs (y:ys)
    | x > y     = sortedListsCommon (x:xs) ys
    | otherwise = x : sortedListsCommon xs ys

sortedListsCommon [] _      = []
sortedListsCommon _ []      = []      


findCommon :: Ord a => [[a]] -> [a]
findCommon (x:y:xs)= findCommon $ sortedListsCommon x y : xs
findCommon [y] = y
findCommon []  = undefined 

common1 :: [Int] -> Int
common1 = head . findCommon . map sort . (\(x, y) -> [x, y]) . splitInHalf

common2 :: [[Int]] -> Int
common2 = head . findCommon . map sort