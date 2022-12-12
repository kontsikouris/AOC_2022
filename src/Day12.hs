module Main where

--import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe ( fromMaybe )
import Data.Char ( ord )

main :: IO ()
main = do   grid <- lines <$> readFile "inputs/Day12.txt"
            let dim@(i1,i2) = (length grid, length (head grid))
            let pointList =  [(n,m) | n <- [1..i1], m <- [1..i2]]
            let indexedGrid = zip pointList $ concat grid
            let ((start,end),gridMap)   = makeGridMap indexedGrid
            let lowestPoints = map (fmap (const 'a')) . filter (\(_,c) -> c == 'a' || c == 'S') $ indexedGrid
            putStrLn $ ("Day12.1 "++) $ show $ fromMaybe (-1) $ getShortestDist dim (Map.delete start gridMap) [(start,'a')] end
            putStrLn $ ("Day12.2 "++) $ show $ fromMaybe (-1) $ getShortestDist dim (Map.delete start gridMap) lowestPoints  end
            
type Elevation = Char
type Row = [Elevation]
type Grid = [Row]
type Point = (Int,Int)
type StartEnd = (Point,Point)
type Dim = (Int,Int)
type State = (Point,Elevation)
type GridMap = Map.Map Point Char

makeGridMap :: [State] -> (StartEnd, GridMap)
makeGridMap = Map.mapAccumWithKey f ((0,0),(0,0)). Map.fromList
    where f (start,end) point c
                | c == 'S'  = ((point, end ),'a')
                | c == 'E'  = ((start,point),'z')
                | otherwise = ((start, end ), c )

getShortestDist :: Dim -> GridMap -> [State] -> Point -> Maybe Int
getShortestDist dim gridMap states end
        | any cond states  = Just 0
        | null states      = Nothing
        | otherwise        = (+1) <$> getShortestDist dim gridMap' newStates end
        where   (gridMap',newStates) = concat <$> List.mapAccumL (getMoves dim) gridMap states
                cond state = end == fst state

getMoves :: Dim -> GridMap -> State -> (GridMap,[State])
getMoves (d1,d2) gridMap ((x,y), height)  = concat <$> List.mapAccumL accF gridMap points  
        where   
                f point = maybe [] (\c -> [(point,c)])
                alterf Nothing = (Nothing,Nothing)
                alterf (Just c) = if cond c then (Just c, Nothing) else (Nothing, Just c)
                accF gridMap point = let (maybeC,gridMap') = Map.alterF alterf point gridMap in (gridMap', f point maybeC) 
                cond c = (ord c - ord height) <= 1
                points' = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
                points = filter (\(n,m) -> 1 <= n && n <= d1 && 1 <= m && m <= d2) points' :: [Point]