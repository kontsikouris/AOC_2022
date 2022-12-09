module Main where

import qualified Data.Map as Map
import Data.Maybe ( fromJust )

main :: IO ()
main = do   grid <- lines <$> readFile "inputs/Day8.txt"
            let dimensions@(i1,i2) = (length grid, length (head grid))
            let gridMap   = makeGridMap grid
            let coordList =  [(n,m) | n <- [1..i1], m <- [1..i2]]
            putStrLn $ ("Day8.1: "++) $ show $ findVisibleNum dimensions coordList gridMap
            putStrLn $ ("Day8.2: "++) $ show $ findMaxScenic  dimensions coordList gridMap


type Tree = Char
type Row = [Tree]
type Grid = [Row]
type Coordinates = (Int,Int)
type Dimensions = (Int,Int)
type Height = Char -- Char is an instance of the Ord class


indexTrees :: Int -> Grid -> [(Coordinates,Tree)]
indexTrees _ []     = [] 
indexTrees n (l:ls) = indexTrees' n 1 l ++ indexTrees (n+1) ls

indexTrees' :: Int -> Int -> [Tree] -> [(Coordinates, Tree)]
indexTrees' _ _ []       = []
indexTrees' n m (x:xs)  = ((n,m),x) : indexTrees' n (m+1) xs

makeGridMap :: Grid -> Map.Map Coordinates Tree
makeGridMap = Map.fromList . indexTrees 1

findVisibleNum :: Dimensions -> [Coordinates] -> Map.Map Coordinates Tree -> Int
findVisibleNum d@(d1,d2) list gridMap = sum     $ map (\coord -> isTreeVisible d coord gridMap) list


findMaxScenic  :: Dimensions -> [Coordinates] -> Map.Map Coordinates Tree -> Int
findMaxScenic  d@(d1,d2) list gridMap = maximum $ map (\coord -> scenicScore   d coord gridMap) list

-- Return 1 if tree is visible else return 0
isTreeVisible :: Dimensions -> Coordinates -> Map.Map Coordinates Tree -> Int
isTreeVisible dim@(d1,d2) coord@(n,m) gridMap = if n == 1 || n == d1 || m == 1 || m == d2 then 1 else checkVisibility dim coord (fromJust $ Map.lookup coord gridMap) gridMap

checkVisibility :: Dimensions -> Coordinates -> Height -> Map.Map Coordinates Tree -> Int
checkVisibility d c h gridMap = if or (traverseGridT d c h gridMap checkVisibilityT) then 1 else 0 

checkVisibilityT :: Dimensions -> Coordinates -> Height -> Map.Map Coordinates Tree -> (Coordinates -> Int -> Coordinates) -> [Int] -> Bool
checkVisibilityT _ (n,m) h gridMap f = all  (\k -> fromJust (Map.lookup (f (n,m) k) gridMap) < h)


scenicScore :: Dimensions -> Coordinates -> Map.Map Coordinates Tree -> Int
scenicScore dim@(d1,d2) coord@(n,m) gridMap = if n == 1 || n == d1 || m == 1 || m == d2 then 0 else findScenicScore dim coord (fromJust $ Map.lookup coord gridMap) gridMap

findScenicScore :: Dimensions -> Coordinates -> Height -> Map.Map Coordinates Tree -> Int
findScenicScore d c h gridMap = product $ traverseGridT d c h gridMap scenicScoreT

traverseGridT :: (Num a, Enum a) => (a, a) -> (a, a) -> t1 -> t2 -> ((a, a) -> (a, a) -> t1 -> t2 -> ((b, b) -> b -> (b, b)) -> [a] -> c) -> [c]
traverseGridT d@(d1,d2) c@(n,m) h gridMap f = zipWith (f d c h gridMap) [rowf,rowf, colf,colf] [[(n-1),(n-2)..1],[(n+1)..d1],[(m-1),(m-2)..1],[(m+1)..d2]]
    where   rowf (a,b) c = (c,b)
            colf (a,b) c = (a,c)

scenicScoreT    :: Dimensions -> Coordinates -> Height -> Map.Map Coordinates Tree -> (Coordinates -> Int -> Coordinates) -> [Int] -> Int
scenicScoreT    _ (n,m) h gridMap f l =  uncurry uncurryf $ span (\k -> fromJust (Map.lookup (f (n,m) k) gridMap) < h) l
    where uncurryf a b = length a + if null b then 0 else 1