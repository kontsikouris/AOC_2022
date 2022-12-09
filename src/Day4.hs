module Main where

main :: IO ()
main = do
        x <- map ( map read . separator (`elem` "-,")) . lines <$> readFile "../Inputs/Day4.txt" :: IO [[Int]]
        print $ ("Day4.1: " ++) . show . sum $ map checkContains x
        print $ ("Day4.2: " ++) . show . sum $ map checkOverlaps x


checkContains :: [Int] -> Int
checkContains [x,y,z,w]
    | x <= z && w <= y  = 1
    | x >= z && w >= y  = 1
    | otherwise         = 0  
checkContains _ = undefined

checkOverlaps :: [Int] -> Int
checkOverlaps [x,y,z,w]
    | z <= x && x <= w || z <= y && y <= w  = 1
    | x <= z && z <= y || x <= w && w <= y  = 1
    | otherwise                             = 0 

separator :: (a -> Bool) -> [a] -> [[a]]
separator _ [] = []
separator p xs = let (l1,lRest) = splitList p xs in l1: separator p lRest

-- same as span but removes the element that equals a
splitList :: (a -> Bool) -> [a] -> ([a], [a])
splitList p (x:xs)
    | p x    =  ([], xs)
    | otherwise     =  let (l1,l2) = splitList p xs in (x:l1, l2) 
splitList l [] = ([], [])
