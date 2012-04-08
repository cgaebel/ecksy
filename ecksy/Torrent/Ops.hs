module Torrent.Ops ( SessionSummary(..)
                   , TorrentSummary(..)
                   , getSessionSummary
                   , getTorrentSummary
                   ) where

import Data.Text
import Import
import Torrent.C

data TorrentSummary = TSummary { torName :: Text
                               , torSavePath :: Text
                               , torState :: TorrentState
                               , torDownloadRate :: Int
                               , torUploadRate :: Int
                               , torUploadLimit :: Int
                               , torDownloadLimit :: Int
                               , torTotalSize :: Integer
                               , torTotalDownloaded :: Integer
                               , torProgress :: Double -- ^ In the range [0, 1]
                               , torPaused :: Bool
                               , torInfoHash :: Sha1Hash
                               }

data SessionSummary = SSummary { sesPaused :: Bool
                               , sesUploadLimit :: Int
                               , sesDownloadLimit :: Int
                               , sesTorrents :: [TorrentSummary]
                               }

getTorrentSummary :: LTor -> Torrent -> IO TorrentSummary
getTorrentSummary lt t = TSummary <$> (pack <$> torrentName lt t)
                                  <*> (pack <$> torrentSavePath lt t)
                                  <*> torrentState lt t
                                  <*> torrentDownloadRate lt t
                                  <*> torrentUploadRate lt t
                                  <*> torrentDownloadLimit lt t
                                  <*> torrentUploadLimit lt t
                                  <*> torrentSize lt t
                                  <*> totalDownloaded lt t
                                  <*> torrentProgress lt t
                                  <*> isPaused lt t
                                  <*> infoHash lt t

getSessionSummary :: LTor -> Session -> IO SessionSummary
getSessionSummary lt s = SSummary <$> isSessionPaused lt s
                                  <*> sessionUploadRateLimit lt s
                                  <*> sessionDownloadRateLimit lt s
                                  <*> (mapM (getTorrentSummary lt) =<< getTorrents lt s)
