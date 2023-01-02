module SimpleBalance
  ( makeDiscs
  , makeDisc
  ) where

import Disc

-- Simple balancing: iterate through the list of files in order and
-- add them to a disc. Once the next file will no longer fit in a
-- disc, start a new disc.

-- make a new disc. The first argument should be a blank disc (and the
-- function can be curried with this), followed by a list of
-- files. Returns the filled disc and the list of remaining files.
-- If the first argument is not a blank disc, files will be added to
-- this disc without removing the existing files.
makeDisc :: Disc -> [File] -> (Disc, [File])
makeDisc disc [] = (disc, [])
makeDisc disc (f:fs)
  | filesize f <= discRemaining disc = makeDisc (Disc size $ f : files') fs
  | otherwise = (disc, f:fs)
  where size = maxSize disc
        files' = files disc

-- distribute all files across discs using the simple balancing
-- method. The first argument should be a blank disc, which will be
-- used as the template for all discs, followed by a list of
-- files. Returns a list of discs.
makeDiscs :: Disc -> [File] -> [Disc]
makeDiscs _ [] = []
makeDiscs blank fs = let
  (disc, fs') = makeDisc blank fs
  in disc : makeDiscs blank fs'
