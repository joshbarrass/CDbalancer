module Disc
  ( Disc(..)
  , File
  , discUsed
  , discRemaining
  , filename
  , filesize
  ) where

-- 

type File = (String, Integer)

filename :: File -> String
filename = fst

filesize :: File -> Integer
filesize = snd

-- Disc:
--   maxSize: size of the disc, in bytes
--   files: list of files on the disc
data Disc = Disc { maxSize :: Integer, files :: [File] }

-- total size of all files on the disc
discUsed :: Disc -> Integer
discUsed = sum . map filesize . files

-- total size remaining on the disc
discRemaining :: Disc -> Integer
discRemaining disc = maxSize disc - discUsed disc
