{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do   inputLines <- lines <$> readFile "inputs/Day11.txt"
            let monkeyMap = Map.fromAscList $ zip [0..] (parseInput inputLines)
            let finalMonkeyMap n f = foldr id monkeyMap (replicate n $ monkeyRound f)
            putStrLn $ ("Day11.1 "++) $ show $ get2Max  $ finalMonkeyMap 20 (`div` 3)
            putStrLn $ ("Day11.1 "++) $ show $ get2Max  $ finalMonkeyMap 10000 (`mod` product [2,3,5,7,11,13,17,19])

-- The difference list structure is used for right to left concatenation of lsits.
-- There are resources online which explain how this is achieved
type DiffList a = [a] -> [a]
diffLappend :: DiffList a -> DiffList a -> DiffList a
diffLappend l1 l2 = l1 . l2

diffLtoList :: DiffList a -> [a]
diffLtoList l = l []

listtoDiffL :: [a] -> DiffList a
listtoDiffL l = (l ++)

emptyDiffL :: DiffList a
emptyDiffL = id

type Op = Int -> Int  -> Int
data Arg = Arg Int | Old deriving Show
type Info = (Arg,Op,Arg,Int,(Int,Int))
data Monkey = Monkey (DiffList Int) Int Info
type MonkeyMap = Map.Map Int Monkey

get2Max :: MonkeyMap -> Int
get2Max = product . take 2 . List.sortBy (flip compare) . map snd . Map.toList . fmap (\(Monkey _ n _) -> n)


monkeyRound :: (Int -> Int) -> MonkeyMap -> MonkeyMap
monkeyRound f monkeyMap  = foldl (turn f) monkeyMap [0..7]

turn  :: (Int -> Int) -> MonkeyMap -> Int -> MonkeyMap
turn f monkeyMap n =  Map.update (Just . concatL lFalse) cFalse 
                    . Map.update (Just . concatL lTrue) cTrue 
                    . Map.update (Just . setEmpty) n 
                    $ monkeyMap
    where   monkey@(Monkey dl k info@(_,_,_,_,(cTrue,cFalse))) = fromJust $ Map.lookup n monkeyMap
            (lTrue,lFalse) = executeTurn (diffLtoList dl) info f
            concatL l (Monkey l' k1 info) = Monkey (l' `diffLappend` listtoDiffL l) k1 info
            setEmpty  (Monkey _  k2 info) = Monkey emptyDiffL (k2 + length (diffLtoList dl)) info

executeTurn :: [Int] -> Info -> (Int -> Int) -> ([Int], [Int])
executeTurn [] _  _ = ([],[])
executeTurn (x:xs) info@(arg1, op, arg2, n, _) f = (if newWorry `mod` n == 0 then addFst else addSnd)  newWorry $ executeTurn xs info f
    where   addFst c (a,b) = (c:a,b)
            addSnd c (a,b) = (a,c:b)
            apply  Old    = x
            apply (Arg k) = k
            newWorry = f (apply arg1 `op` apply arg2)

parseInput :: [String] -> [Monkey]
parseInput [] = []
parseInput (_:items:op:test:trueCond:falseCond:list) = 
        Monkey  items' 0 (arg1, op', arg2, test', (trueCond',falseCond')) : parseInput (dropWhile null list)
    where   l = map (separator (`elem` " :,")) [items,op,test,trueCond,falseCond]
            castOp r | r == "*" = (*) | r == "+" = (+) | otherwise = undefined
            read' str = if str == "old" then Old else Arg (read str)
            items' = listtoDiffL . map read . drop 2 $ l !! 0
            (arg1,op',arg2) = (\[x,y,z] -> (read' x, castOp y, read' z)) . drop 3 $ l !! 1
            test'  = read . (!! 3) $ l !! 2
            trueCond' = read . (!! 5) $  l !! 3
            falseCond' = read . (!! 5) $  l !! 4

separator :: Eq a => (a -> Bool) -> [a] -> [[a]]
separator _ [] = []
separator p xs = let (l1,lRest) = break p (dropWhile p xs) in l1: separator p lRest
