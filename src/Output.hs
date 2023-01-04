module Output
  ( filelistOutput
  ) where

import Data.List
import Disc

-- each string in the output list is one output file
filelistOutput :: [Disc] -> [String]
filelistOutput discs = let
  allFiles = map files discs
  filenameList = map (map filename) allFiles
  in map ((++ "\n") . intercalate "\n") filenameList
  
