{-# LANGUAGE QuasiQuotes #-}
module ArgParsing
  (getDocopt
  ,getBlankDisc
  ,getFileList
  ,getPrefix
  ,checkForHelp
  ,getOutputType
  ,getBalancingType
  ) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

import Disc
import Output

import SimpleBalance
import SmartBalance
import SortedSmart

patterns :: Docopt
patterns = [docopt|
CDbalancer

Usage:
  CDbalance --help
  CDbalancer [--disc-size=<size>] [--output-prefix=<prefix>] [--balancing=<type>] [--graft-points] <file_list>

Options:
  -h --help                    Show this screen.
  -s --disc-size=<size>        Size of the disc in bytes [default: 524288000].
  -p --output-prefix=<prefix>  Set the prefix of the output files [default: disc].
  -b --balancing=<type>        Choose balancing type. Valid options: simple, smart, sortsmart. [default: sortsmart]
  -g --graft-points            Write output in graft-points format, for compatibility with mkisofs
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

getOutputType :: IO ([Disc] -> [String])
getOutputType = do
  args <- getDocopt
  -- case to make adding future formats easier
  return $ case () of _
                        | (args `isPresent` (longOption "graft-points")) -> graftPointOutput
                        | otherwise -> filelistOutput

getBalancingType :: IO (Disc -> [File] -> [Disc])
getBalancingType = do
  args <- getDocopt
  let balancingType = getArgWithDefault args "sortsmart" (longOption "balancing")
  -- case to make adding future formats easier
  case balancingType of
    "simple" -> printAndReturn "Using simple balancing." SimpleBalance.makeDiscs
    "smart" -> printAndReturn "Using smart balancing." SmartBalance.makeDiscs
    "sortsmart" -> printAndReturn "Using sortsmart balancing." SortedSmart.makeDiscs
    _ -> printAndReturn "Unknown balancing type. Defaulting to sortsmart balancing." SortedSmart.makeDiscs
  where printAndReturn :: String -> b -> IO b
        printAndReturn toPrint toReturn = do
          putStrLn toPrint
          return toReturn
