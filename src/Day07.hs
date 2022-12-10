module Main where

import Data.List

main :: IO ()
main = do   fsZipper <- moveHome . flip constructFS mkFSZipper . map strToLine . lines <$> readFile "inputs/Day7.txt" 
            let sizes = findSumL . getFS $ fsZipper
            let totalSize = head sizes
            print . sum . filter (<= 100000) $ sizes
            print . minimum . filter (totalSize - 40000000 <=) $ sizes

type FileName = String
type FolderName = String
type ParentFolderName = String
type FileSize = Int
type FSPrev  = [FS]
type FSAfter = [FS]
type FSRootPath  = [(FSPrev, FSAfter, ParentFolderName)]

data FS =   FSFile   { getFName :: FileName,  getFSize :: FileSize}
        |   FSFolder { getFName :: FolderName, getfsList :: [FS]}
        deriving Show

data FSZipper = CurrFolder {getFS :: FS, getFSRootPath :: FSRootPath } 
        deriving Show


data Line = CD String | LS | FSLine FS

moveOut :: FSZipper -> FSZipper
moveOut (CurrFolder fs ((fsPrev,fsAfter,fName):fsRootPath)) = CurrFolder (FSFolder fName (fsPrev ++ [fs] ++ fsAfter)) fsRootPath
moveOut _ = undefined

moveIn :: FolderName -> FSZipper -> FSZipper
moveIn fName fsZipper = CurrFolder fs $  (fsPrev, fsAfter, getFName $ getFS fsZipper) : getFSRootPath fsZipper  
        where   (fsPrev,fs:fsAfter) = break ((fName ==) . getFName) (getfsList $ getFS fsZipper)

moveHome :: FSZipper -> FSZipper
moveHome fsZipper@(CurrFolder _ x) = if null x then fsZipper else moveHome $ moveOut fsZipper

mkFolder :: String -> FS
mkFolder fname = FSFolder fname []

mkFSZipper :: FSZipper
mkFSZipper = CurrFolder (mkFolder "/") []

setFolderContents :: [FS] -> FS -> FS
setFolderContents l (FSFolder name _) = FSFolder name l

strToLine :: String -> Line
strToLine str
    | "$ ls"   `isPrefixOf`  str = LS
    | "$ cd "  `isPrefixOf`  str = CD (drop 5 str)
    | "dir "   `isPrefixOf`  str = FSLine (mkFolder (drop 4 str))
    | otherwise                  = (\[x,y] -> FSLine (FSFile y (read x))) (words str)

constructFS :: [Line] -> FSZipper -> FSZipper
constructFS []           fsZipper   = fsZipper
constructFS (CD r:lines) fsZipper   = constructFS lines $ case r of
                                    ".." -> moveOut   fsZipper
                                    "/"  -> moveHome  fsZipper
                                    _    -> moveIn  r fsZipper
constructFS (LS:lines)   (CurrFolder fs fsl)   = constructFS rest (CurrFolder (setFolderContents (map unlinefs fsLines) fs) fsl)
    where   isCommand line = case line of FSLine _ -> False ; _ -> True
            (fsLines,rest) = break isCommand lines
            unlinefs (FSLine x) = x

constructFS  _ _ = undefined

findSumL :: FS -> [FileSize]
findSumL (FSFolder name l) = (totalFileSize + folderSums) : concat lFolder'
    where   isFile fs = case fs of FSFile _ _ -> True ; _ -> False
            (lFile, lFolder) = partition isFile l
            lFolder' = map findSumL lFolder
            totalFileSize = sum $ map getFSize lFile
            folderSums  = sum (map head lFolder')
