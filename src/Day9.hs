
module Main where

import Control.Monad.Writer ( foldM, MonadWriter(writer), execWriter, Writer )
import Data.Set ( Set, singleton, size )

main :: IO ()
main = do   input <- readFile "inputs/Day9.txt"
            let writerMovesL n   =  concatMap (getWriterMoves n .  words) . lines $ input
            let appliedMoves n   = writer (initialState n, singleton (0,0)) >>= \rope -> writerSequence rope (writerMovesL n)
            let tailsMovesNum n  = size . execWriter $ appliedMoves n
            putStrLn $ ("Day9.1: " ++) $ show $ tailsMovesNum 2
            putStrLn $ ("Day9.2: " ++) $ show $ tailsMovesNum 10

type Point = (Int,Int)
type Rope = [Point]
type TailMoves = Set Point

initialState :: Int -> Rope
initialState n = replicate n (0,0)

writerSequence :: Rope -> [Rope -> Writer TailMoves Rope] -> Writer TailMoves Rope
writerSequence = foldM (flip ($))


updateRope :: [Point] -> [Point]
updateRope (x:y:xs) =  x : updateRope (updateNext x y :xs)
updateRope l = l

updateNext :: Point -> Point -> Point
updateNext (headX,headY) tail@(tailX,tailY)
    | abs diffX < 2 && abs diffY < 2 = tail
    | otherwise   = (tailX + signum diffX,tailY + signum diffY)
    where   diffX = headX - tailX
            diffY = headY - tailY

writerMove :: Int -> (Rope -> Rope) -> Rope -> Writer TailMoves Rope
writerMove n move rope = let newRope = move rope in writer (newRope, singleton (newRope !! (n-1)))

moveL :: Rope -> Rope
moveL ((headX,headY):tail) = updateRope ((headX-1,headY):tail)

moveR :: Rope -> Rope
moveR ((headX,headY):tail) = updateRope ((headX+1,headY):tail)

moveU :: Rope -> Rope
moveU ((headX,headY):tail) = updateRope ((headX,headY+1):tail)

moveD :: Rope -> Rope
moveD ((headX,headY):tail) = updateRope ((headX,headY-1):tail)

getWriterMoves :: Int -> [String] -> [Rope -> Writer TailMoves Rope]
getWriterMoves n [x,y] = replicate (read y) . writerMove n $ case x of
        "L" -> moveL 
        "R" -> moveR
        "U" -> moveU
        "D" -> moveD
        _   -> undefined
