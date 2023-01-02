{-# LANGUAGE QuasiQuotes #-}
module ArgParsing
  (getDocopt
  ,getBlankDisc
  ,getFileList
  ) where

import System.Environment (getArgs)
import System.Console.Docopt

import Disc

patterns :: Docopt
patterns = [docopt|
CDbalancer

Usage:
  CDbalancer [--disc-size=<size>] <file_list>

Options:
  -h --help              Show this screen.
  -s --disc-size=<size>  Size of the disc in bytes [default: 524288000].
|]

getArgOrExit = getArgOrExitWith patterns

getDocopt :: IO Arguments
getDocopt = parseArgsOrExit patterns =<< getArgs

getBlankDisc :: IO Disc
getBlankDisc = do
  args <- getDocopt
  let size = getArgWithDefault args "524288000" (longOption "disc-size")
  return $ Disc (read size) []

getFileList :: IO String
getFileList = do
  args <- getDocopt
  fileList <- args `getArgOrExit` (argument "file_list")
  return fileList