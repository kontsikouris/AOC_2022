module Main where

import qualified Data.Set as Set
import qualified Data.List as List

main :: IO () 
main = do   inputLines <- lines <$> readFile "inputs/Day14.txt" 
            let pointLists = map (makePoints . separator (`elem` " ,->")) inputLines
            let abyss = maximum $ map (maximum . map snd) pointLists
            let rocksSet = List.foldl' makeRocks Set.empty pointLists
            let sandSource = (500,0) :: Point
            putStrLn $ ("Day14.1: "++) $ show $ findTime1 rocksSet sandSource abyss 0
            putStrLn $ ("Day14.2: "++) $ show $ findTime2 rocksSet sandSource (Just (abyss+2)) 0

type Point = (Int,Int)
type Occupied = Set.Set Point
type Floor = Maybe Int

makePoints :: [String] -> [Point]
makePoints [] = []
makePoints (x:y:xs) = (read x, read y): makePoints xs 
makePoints _ = undefined

addRocks :: Point -> Point -> Occupied -> Occupied
addRocks (x,y) (z,w) set
    | x == z    = Set.union set $ Set.fromAscList [(x,k) | k <- lY]
    | y == w    = Set.union set $ Set.fromAscList [(k,y) | k <- lX]
    | otherwise = undefined
    where   lY = if y < w then [y..w] else [w..y]
            lX = if x < z then [x..z] else [z..x]

makeRocks :: Occupied -> [Point] -> Occupied
makeRocks set []     = set
makeRocks set [p]    = Set.insert p set
makeRocks set (p:xs) = snd $ List.foldl' (\(p1,set) p2 -> (p2, addRocks p1 p2 set)) (p,set) xs

addSand :: Occupied -> Point -> Int -> Floor -> Occupied
addSand set (x,y) abyss floor
    | maybe False (\n -> y+1 == n) floor      = Set.insert (x,y) set
    | maybe (y >= abyss) (const False) floor  = Set.empty
    | Set.notMember (x  ,y+1) set             = addSand set (x  ,y+1) abyss floor
    | Set.notMember (x-1,y+1) set             = addSand set (x-1,y+1) abyss floor
    | Set.notMember (x+1,y+1) set             = addSand set (x+1,y+1) abyss floor
    |         otherwise                       = Set.insert (x,y) set

findTime1 :: Occupied -> Point -> Int -> Int -> Int
findTime1 set source abyss n
    | Set.null set' = n
    | otherwise     = findTime1 set' source abyss (n+1)
    where set' = addSand set source abyss Nothing

findTime2 :: Occupied -> Point -> Floor -> Int -> Int
findTime2 set source floor n
    | Set.member source set = n
    | otherwise     = findTime2 set' source floor (n+1)
    where set' = addSand set source 0 floor



separator :: (a -> Bool) -> [a] -> [[a]]
separator p xs = separator' p (dropWhile p xs)
    where   separator' _ [] = []
            separator' p xs = let  (l1,lRest) = break p xs in l1 :separator p (dropWhile p lRest)


