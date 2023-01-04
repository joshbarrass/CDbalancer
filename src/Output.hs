module Output
  ( filelistOutput
  , graftPointOutput
  ) where

import Data.List
import System.FilePath
import Disc

getFilenameLists :: [Disc] -> [[String]]
getFilenameLists discs = let
  allFiles = map files discs
  in map (map filename) allFiles

concatToFilelists :: [[String]] -> [String]
concatToFilelists = map ((++ "\n") . intercalate "\n")

-- each string in the output list is one output file
filelistOutput :: [Disc] -> [String]
filelistOutput discs = let
  filenameList = getFilenameLists discs
  in concatToFilelists filenameList

-- replaces all backslash and equals characters with escaped versions
escapeGraftPoint :: String -> String
escapeGraftPoint [] = []
escapeGraftPoint ('\\':xs) = '\\' : '\\' : escapeGraftPoint xs
escapeGraftPoint ('=':xs) = '\\' : '=' : escapeGraftPoint xs
escapeGraftPoint (x:xs) = x : escapeGraftPoint xs

-- generate graft point syntax for a single filename
makeGraftPoint :: String -> String
makeGraftPoint p = let
  escaped = escapeGraftPoint p
  dirname = takeDirectory escaped
  graftPoint = dirname ++ "/"
  in graftPoint ++ '=' : escaped
  

graftPointOutput :: [Disc] -> [String]
graftPointOutput discs = let
  filenameList = getFilenameLists discs
  graftPointList = map (map makeGraftPoint) filenameList
  in concatToFilelists graftPointList
