module Main where

--import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe ( fromMaybe )
import Data.Char ( ord )

main :: IO ()
main = do   inputLines <- lines <$> readFile "inputs/Day13.txt"
            let separatedPairs = separator null . map (concatMap extractListBracket . separator (`elem` ", ")) $  inputLines
            let mixedListPairs = map (map strToMixed) separatedPairs
            putStrLn $ ("Day13.1: "++) $ show $ sum $ map fst $ filter (checkPairs . snd) $ zip [1..] mixedListPairs 
            putStrLn $ ("Day13.2: "++) $ show $ product $ findIndices 1 (List.sortBy mixedCompare (concat mixedListPairs))


data MixedList a = MixedValue a | MixedL [MixedList a]


mixedCompare :: Ord a => MixedList a -> MixedList a -> Ordering
mixedCompare (MixedValue n) (MixedValue m)   = compare n m
mixedCompare (MixedValue n) (MixedL xs)      = mixedCompare (MixedL [MixedValue n]) (MixedL xs)
mixedCompare (MixedL xs) (MixedValue n)      = mixedCompare (MixedL xs) (MixedL [MixedValue n])
mixedCompare (MixedL []) (MixedL [])         = EQ
mixedCompare (MixedL (x:xs)) (MixedL [])     = GT
mixedCompare (MixedL []) (MixedL (x:xs))     = LT
mixedCompare (MixedL (x:xs)) (MixedL (y:ys)) = mixedCompare x y `mappend` mixedCompare (MixedL xs) (MixedL ys)


extractListBracket :: String -> [String]
extractListBracket [] = []
extractListBracket ('[':xs) = "[" : extractListBracket xs
extractListBracket (']':xs) = "]" : extractListBracket xs
extractListBracket xs = let (l1,l2) = break (`elem` "[]") xs in l1 : extractListBracket l2


strToMixed :: [String] -> MixedList Int
strToMixed ("[":xs) = snd $ strToMixed' xs
strToMixed [xs]       = MixedValue (read xs) 
strToMixed _ = undefined

strToMixed' :: [String] -> ([String],MixedList Int)
strToMixed' [] = ([],MixedL [])
strToMixed' ("]":xs) = (xs, MixedL [])
strToMixed' ("[":xs) = mixedConcat mixed <$> strToMixed' str 
        where   (str,mixed) = strToMixed' xs
strToMixed' (x:xs) = mixedConcat (MixedValue (read x)) <$> strToMixed' xs 

mixedConcat :: MixedList a -> MixedList a -> MixedList a
mixedConcat c (MixedL b) = MixedL (c:b)
mixedConcat _ _ = undefined

checkPairs :: [MixedList Int] -> Bool
checkPairs [pair1,pair2] = mixedCompare pair1 pair2 /= GT 
checkPairs _ = undefined


findIndices  :: Int -> [MixedList Int] -> [Int]
findIndices  n (mixed:l) = if mixedCompare ll2 mixed /= GT then n: findIndices' (n+1) (mixed:l) else findIndices (n+1) l
        where ll2 = MixedL [MixedL [MixedValue 2]]

findIndices' :: Int -> [MixedList Int] -> [Int]
findIndices' n (mixed:l) = if mixedCompare ll6 mixed /= GT then [n] else findIndices' (n+1) l
        where ll6 = MixedL [MixedL [MixedValue 6]]


separator :: (a -> Bool) -> [a] -> [[a]]
separator p xs = separator' p (dropWhile p xs)
    where   separator' _ [] = []
            separator' p xs = let  (l1,lRest) = break p xs in l1 :separator p (dropWhile p lRest)