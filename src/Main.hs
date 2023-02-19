module Main where

import Data.List
import FileUtils
import Disc
import SimpleBalance
import ArgParsing
import Output

largerThanDisc :: Disc -> File -> Bool
largerThanDisc disc f = filesize f > maxSize disc

anyLargerThanDisc :: Disc -> [File] -> Bool
anyLargerThanDisc disc = any (largerThanDisc disc)

main :: IO ()
main = do
  checkForHelp
  
  blankDisc <- getBlankDisc
  prefix <- getPrefix
  
  let makeDiscs' :: [File] -> [Disc]
      makeDiscs' = makeDiscs blankDisc

  fn <- getFileList
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
      putStrLn "One or more specified files is larger than the disc:"
      let sizeMap = zip fileSizes fileList
      let tooBig = map (\(s, f) -> f ++ "  (" ++ show s ++ ")") $ filter ((> maxSize blankDisc) . fst) sizeMap
      let output = "  " ++ intercalate "\n  " tooBig
      putStrLn output
      putStrLn "Aborting..."
    else do
      outputFn <- getOutputType
      let discs = makeDiscs' fs
      -- display percentage usage
      let percentages = map (\d -> fromIntegral (discUsed d) / fromIntegral (maxSize d)) discs
      mapM_ (\(i, percent) -> putStrLn $ "Disc " ++ show i ++ ": " ++ show (percent*100) ++ "%") $ zip [1..] percentages
      -- mean percentage is also the total usage
      let totalPercent = sum percentages / fromIntegral (length percentages)
      putStrLn $ "Total usage: " ++ show (totalPercent * 100) ++ "%"
      let output = outputFn discs
      let enum = zip [1..] output

      mapM_ (\(i, content) -> writeFile (prefix ++ show i) content) enum
