module Main where

import Data.Foldable ( Foldable(foldl') )

main :: IO ()
main = do   
        x <- calculate . transform <$> readFile "inputs/Day2.txt"
        putStrLn $ "Day2.1: " ++ show (fst x)
        putStrLn $ "Day2.2: " ++ show (snd x)

data RPS = Rock | Paper | Scissors deriving (Eq)

succRPS :: RPS -> RPS
succRPS Rock     = Paper
succRPS Paper    = Scissors
succRPS Scissors = Rock

prevRPS :: RPS -> RPS
prevRPS = succRPS . succRPS

cast :: String -> RPS
cast x
    | x == "A" || x == "X"  = Rock 
    | x == "B" || x == "Y"  = Paper
    | x == "C" || x == "Z"  = Scissors
    | otherwise             = undefined

resultPoints1 :: RPS -> RPS -> Int
resultPoints1 x y
    | x == y            = 3
    | succRPS x == y    = 6
    | otherwise         = 0

handPoints :: RPS -> Int
handPoints Rock     = 1
handPoints Paper    = 2
handPoints Scissors = 3

matchPoints1 :: RPS -> RPS -> Int
matchPoints1 x y = resultPoints1 x y + handPoints y

findHand :: RPS -> RPS -> RPS
findHand x Rock     = prevRPS x
findHand x Paper    = x
findHand x Scissors = succRPS x

matchPoints2 :: RPS -> RPS -> Int
matchPoints2 x y = matchPoints1 x (findHand x y)

accumulate :: (Int,Int) -> [RPS] -> (Int, Int)
accumulate (r1,r2) [x,y] = (matchPoints1 x y + r1, matchPoints2 x y + r2)
accumulate _ _ = undefined

transform :: String -> [[RPS]]
transform =  map (map cast . words) . lines

calculate :: [[RPS]] -> (Int, Int) 
calculate = foldl' accumulate (0,0)