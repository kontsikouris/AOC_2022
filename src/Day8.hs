module Main where

import qualified Data.Map as Map
import Control.Applicative ( ZipList(ZipList) )
import Data.Maybe ( fromJust )
import Data.Coerce (coerce)

main :: IO ()
main = do   grid <- lines <$> readFile "../../inputs/Day8.txt"
            let dim@(i1,i2) = (length grid, length (head grid))
            let pointList =  [(n,m) | n <- [1..i1], m <- [1..i2]]
            let gridMap   = makeGridMap pointList grid
            putStrLn $ ("Day8.1: "++) $ show $ findVisibleNum dim pointList gridMap
            putStrLn $ ("Day8.2: "++) $ show $ findMaxScenic  dim pointList gridMap

type Tree = Char
type Row = [Tree]
type Grid = [Row]
type Point = (Int,Int)
type Dim = (Int,Int)
type Height = Char -- Char is an instance of the Ord class

makeGridMap :: [Point] -> Grid -> Map.Map Point Tree
makeGridMap points = Map.fromList . zip points . concat

findVisibleNum :: Dim -> [Point] -> Map.Map Point Tree -> Int
findVisibleNum d@(d1,d2) list gridMap = sum     $ map (\point -> isTreeVisible d point gridMap) list

findMaxScenic  :: Dim -> [Point] -> Map.Map Point Tree -> Int
findMaxScenic  d@(d1,d2) list gridMap = maximum $ map (\point -> scenicScore   d point gridMap) list

isTreeVisible :: Dim -> Point -> Map.Map Point Tree -> Int
isTreeVisible dim@(d1,d2) point@(n,m) gridMap 
    | n == 1 || n == d1 || m == 1 || m == d2 = 1
    | or (traverseGridT dim point (fromJust $ Map.lookup point gridMap) gridMap checkVisibilityT) = 1
    | otherwise = 0

checkVisibilityT :: Point -> Height -> Map.Map Point Tree -> [Point] -> Bool
checkVisibilityT (n,m) h gridMap = all  (\point -> fromJust (Map.lookup point gridMap) < h)

scenicScore :: Dim -> Point -> Map.Map Point Tree -> Int
scenicScore dim@(d1,d2) point@(n,m) gridMap
    | n == 1 || n == d1 || m == 1 || m == d2 = 0
    | otherwise                              = product $ traverseGridT dim point (fromJust $ Map.lookup point gridMap) gridMap scenicScoreT

scenicScoreT    :: Point -> Height -> Map.Map Point Tree -> [Point]-> Int
scenicScoreT    (n,m) h gridMap l =  uncurry uncurryf $ span (\point -> fromJust (Map.lookup point gridMap) < h) l
    where uncurryf a b = length a + if null b then 0 else 1

traverseGridT :: Dim -> Point -> Height -> Map.Map Point Tree -> (Point -> Height -> Map.Map Point Tree -> [Point] -> a) -> [a]
traverseGridT d@(d1,d2) c@(n,m) h gridMap f = map (f c h gridMap) $ coerce pointLL
    where   rowf (a,b) c = (c,b)
            colf (a,b) c = (a,c)
            l1  = coerce  [map (rowf c),map (rowf c), map (colf c), map (colf c)]    :: ZipList ([Int] -> [Point])
            l2  = coerce [[(n-1),(n-2)..1],[(n+1)..d1],[(m-1),(m-2)..1],[(m+1)..d2]] :: ZipList  [Int]
            pointLL =  l1 <*> l2