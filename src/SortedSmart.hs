module SortedSmart
  ( makeDiscs
  , makeDisc
  ) where

import Disc
import qualified SmartBalance
import Data.List

-- Sorted smart balancing: identical to smart balancing, but the list
-- of files is sorted into descending order of size before
-- balancing. This can often lead to more compact storage of files.

makeDisc = SmartBalance.makeDisc

-- sort files in descending order of size
sortFilesDes :: [File] -> [File]
sortFilesDes = sortBy (\(_, s1) (_, s2) -> compare s2 s1)

-- distribute all files across discs using the sorted smart balancing
-- method. The first argument should be a blank disc, which will be
-- used as the template for all discs, followed by a list of
-- files. Returns a list of discs.
makeDiscs :: Disc -> [File] -> [Disc]
makeDiscs blank fs = SmartBalance.makeDiscs blank $ sortFilesDes fs
