module Handler.RenameTorrent ( postRenameTorrentR ) where

import Data.Text as T

import Auth ( requireLogin )
import Control.Monad.Trans.Maybe
import Import
import Torrent

-- | Renames a torrent with a given info hash to have the given name.
postRenameTorrentR :: Handler RepJson
postRenameTorrentR = requireLogin $ do (ihash, newName) <- runInputPost $ (,)
                                                                     <$> ireq textField "infohash"
                                                                     <*> ireq textField "name"

                                       (tl, ses) <- getTorSession <$> getYesod

                                       r <- runMaybeT $ do guard $ T.all isHexDigit ihash
                                                           t <- MaybeT . liftIO $ findTorrent tl ses ihash
                                                           liftIO $ setTorrentName tl t newName

                                                           lift . runDB $ updateWhere [ DownloadLinkInfoHash ==. ihash ]
                                                                                      [ DownloadLinkName =. newName ]

                                       case r of
                                          Nothing -> invalidArgs [ "infohash" ]
                                          Just _  -> sendResponse $ RepJson emptyContent
