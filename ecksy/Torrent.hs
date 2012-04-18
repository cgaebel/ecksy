module Torrent ( module Torrent.C
               , module Torrent.Magnet
               , module Torrent.Ops
               , downloadFolder
               ) where

import Torrent.C
import Torrent.Magnet
import Torrent.Ops

import Data.Text ( Text )

-- | Temporary hack until we figure out config files.
downloadFolder :: Text
downloadFolder = "downloads"
