module Main where

import System.Environment
import Data.List
import FileUtils
import Disc
import SimpleBalance

blankDisc :: Disc
blankDisc = Disc (500*1024*1024) []

makeDiscs' :: [File] -> [Disc]
makeDiscs' = makeDiscs blankDisc

largerThanDisc :: Disc -> File -> Bool
largerThanDisc disc f = (filesize f > maxSize disc)

anyLargerThanDisc :: Disc -> [File] -> Bool
anyLargerThanDisc disc fs = any (==True) $ map (largerThanDisc disc) fs

main :: IO ()
main = do
  args <- getArgs
  let fn = head args
  input <- readFile fn
  let fileList = lines input
  fileExistsList <- mapM doesFileExist fileList
  let allExist = all (==True) fileExistsList
  
  if not allExist then do
    putStrLn "One or more specified files do not exist:"
    let existanceMap = zip fileExistsList fileList
    let nonExistent = map snd $ filter (not . fst) existanceMap
    let output = "  " ++ intercalate "\n  " nonExistent
    putStrLn output
    putStrLn "Aborting..."
  else do
    fileSizes <- mapM getFileSize fileList
    let fs = zip fileList fileSizes

    if anyLargerThanDisc blankDisc fs then do
      putStrLn "One or more specified files is larger than the disc. Aborting..."
    else do
      let discs = makeDiscs' fs
      let allFiles :: [[File]]
          allFiles = map files discs
      let filenameList :: [[String]]
          filenameList = map (map (filename)) allFiles
      let perDiscOutput :: [String]
          perDiscOutput = map (intercalate "\n") filenameList
      let output = intercalate "\n-----\n" perDiscOutput
      putStrLn output
