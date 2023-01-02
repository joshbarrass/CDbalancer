{-# LANGUAGE QuasiQuotes #-}
module ArgParsing
  (getDocopt
  ,getBlankDisc
  ,getFileList
  ,getPrefix
  ,checkForHelp
  ) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

import Disc

patterns :: Docopt
patterns = [docopt|
CDbalancer

Usage:
  CDbalance --help
  CDbalancer [--disc-size=<size>] [--output-prefix=<prefix>] <file_list>

Options:
  -h --help                    Show this screen.
  -s --disc-size=<size>        Size of the disc in bytes [default: 524288000].
  -p --output-prefix=<prefix>  Set the prefix of the output files [default: disc].
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

getPrefix :: IO String
getPrefix = do
  args <- getDocopt
  return $ getArgWithDefault args "disc" (longOption "output-prefix")

checkForHelp :: IO ()
checkForHelp = do
  args <- getDocopt
  when (args `isPresent` (longOption "help")) $ do
    exitWithUsage patterns
