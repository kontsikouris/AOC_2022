module Main where

import Data.List ( sortBy )

main :: IO ()
main = do
        nums  <- castAndSort . lines <$> readFile "inputs/Day1.txt"
        putStrLn $ ("Day 1.1: " ++) . show $ head nums
        putStrLn $ ("Day 1.2: " ++) . show . sum . take 3 $ nums

castAndSort :: [String] -> [Int]
castAndSort = sortBy (flip compare) . map (sum . map read) . separator null

separator :: (a -> Bool) -> [a] -> [[a]]
separator p xs = separator' p (dropWhile p xs)
    where   separator' _ [] = []
            separator' p xs = let  (l1,lRest) = break p xs in l1 :separator p (dropWhile p lRest)