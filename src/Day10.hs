module Main where

import Data.List ( mapAccumL )

main :: IO ()
main = do   commands <- map strToCommand . lines <$> readFile "inputs/Day10.txt"
            let executionL = executionList commands
            let signalsSum = sum . map (uncurry (*)) . filter (\(a,_) -> (a + 20) `mod` 40 == 0)
            putStrLn $ ("Day10.1: "++) $ show $ signalsSum executionL
            putStrLn "Day10.2: "
            mapM_ putStrLn . splitInSegments 40 . map writePixel $ executionL

data Command = Noop | Addx Int

strToCommand :: String -> Command
strToCommand "noop" = Noop
strToCommand str    = Addx (read (drop 5 str))

type Cycle = Int
type Register = Int
type State = (Cycle, Register)

executionList :: [Command] -> [State]
executionList =  concat . snd . mapAccumL f (1,1)
    where   f (cycle,reg)  Noop    = ((cycle+1,reg),  [(cycle,reg)])
            f (cycle,reg) (Addx n) = ((cycle+2,reg+n),[(cycle,reg),(cycle+1,reg)])
--executionList :: [Command] -> State -> [State]
--executionList [] _ = []
--executionList (Noop:xs)    (cycle,reg) = (cycle, reg) : executionList xs (cycle+1, reg)
--executionList (Addx n:xs)  (cycle,reg) = (cycle, reg) : (cycle+1,reg) : executionList xs (cycle+2,reg+n) 

writePixel :: State -> Char
writePixel (cycle,register) =  if abs a <= 1 then '#' else '.'
    where a = register - ((cycle-1) `mod` 40)

splitInSegments :: Int -> [a] -> [[a]]
splitInSegments _ [] = []
splitInSegments n l = uncurry (:) . fmap (splitInSegments n) $ splitAt n l