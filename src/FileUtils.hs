module FileUtils
  ( getFileSize
  , doesFileExist
  , allFilesExist
  ) where 

import System.Directory

allFilesExist :: [FilePath] -> IO Bool
allFilesExist fs = do
  fileExistsList <- mapM doesFileExist fs
  return $ all (==True) fileExistsList
