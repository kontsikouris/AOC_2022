module Main where

import Data.List ( foldl', isPrefixOf )

type Layer = String 
type Stack = String
type Stacks = [Stack]
type Move  = (Int,Int,Int)

main :: IO ()
main = do
        x <- lines <$> readFile "../../inputs/Day5.txt" :: IO [String] -- read input and apply lines
        let     (layers,_:[]:strMoves) = break (isPrefixOf " 1") x      -- break the layers input from the moves (ignore the row of numbers and the empty line) 
                emptyStacks = createStacks $ head layers                -- create an initial list of empty stacks
                stacks = foldr addLayer emptyStacks layers              -- fill the stacks according to the layers
                moves = map ( (\[_,x,_,y,_,z] -> (read x,read y,read z)) . words) strMoves -- Convert String format of a move to type Move
                finalStack moveFunc = map head $ foldl' (applyMove moveFunc) stacks moves       
        putStrLn $  ("Day5.1: "++) $ finalStack obtainR
        putStrLn $  ("Day5.2: "++) $ finalStack splitAt

createStacks :: Layer -> Stacks
createStacks lstr = replicate ((length lstr + 1) `div` 4) []

addLayer :: Layer -> Stacks -> Stacks
addLayer [_,x,_] [l]    = [add x l] 
addLayer (_:x:_:_:xs) (l:ll) =  add x l : addLayer xs ll
addLayer _ _ = undefined

add :: Char -> Stack -> Stack
add x l = [x |x /= ' '] ++ l

applyMove :: (Int -> Stack -> (Stack, Stack)) -> Stacks -> Move -> Stacks
applyMove f stacks (x,y,z) = changeAt z (items ++) (stacks1 ++ [stack'] ++ stacks2)
    where   (stacks1,stack:stacks2) = splitAt (y-1) stacks
            (items,stack') = f x stack

changeAt :: (Eq t, Num t) => t -> (a -> a) -> [a] -> [a]
changeAt 1 f (l:ll) = f l : ll
changeAt n f (l:ll) = l : changeAt (n-1) f ll

obtainR :: (Eq t, Num t) => t -> [a] -> ([a], [a])
obtainR n l = obtainRAcc n l []
    where   obtainRAcc 0 xs     acc = (acc,xs)
            obtainRAcc n (x:xs) acc = obtainRAcc (n-1) xs (x:acc)
