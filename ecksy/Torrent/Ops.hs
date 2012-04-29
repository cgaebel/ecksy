{-# LANGUAGE TemplateHaskell #-}
module Torrent.Ops ( SessionSummary(..)
                   , TorrentSummary(..)
                   , getSessionSummary
                   , getTorrentSummary
                   ) where

import Prelude ( Int
               , Integer
               , Double
               , Bool
               , IO
               , Show
               , putStrLn
               , (++)
               , ($)
               , (.)
               )

import Control.Applicative
import Control.Monad
import Data.Aeson.TH
import qualified Data.List as L
import Data.Text

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
                               , torInfoHash :: Text
                               }
    deriving Show

$(deriveJSON (L.drop 3) ''TorrentSummary)

data SessionSummary = SSummary { sesPaused :: Bool
                               , sesUploadLimit :: Int
                               , sesDownloadLimit :: Int
                               , sesTorrents :: [TorrentSummary]
                               }
    deriving Show

$(deriveJSON (L.drop 3) ''SessionSummary)

getTorrentSummary :: LTor -> Torrent -> IO TorrentSummary
getTorrentSummary lt t = TSummary <$> torrentName lt t
                                  <*> torrentSavePath lt t
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
