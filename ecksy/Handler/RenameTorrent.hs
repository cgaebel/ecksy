module Handler.RenameTorrent ( postRenameTorrentR ) where

import Data.Text as T

import Auth ( requireLogin )
import Control.Monad.Trans.Maybe
import Import
import Torrent

checkValidity :: InfoHash -> Handler ()
checkValidity h | T.all isHexDigit h = return ()
                | otherwise          = invalidArgs [ "infohash" ]

-- | Renames a torrent with a given info hash to have the given name.
postRenameTorrentR :: Handler RepJson
postRenameTorrentR = requireLogin $ do (ihash, newName) <- runInputPost $ (,)
                                                                     <$> ireq textField "infohash"
                                                                     <*> ireq textField "name"

                                       checkValidity ihash

                                       (tl, ses) <- getTorSession <$> getYesod

                                       r <- runMaybeT $ do t <- MaybeT . liftIO $ findTorrent tl ses ihash
                                                           liftIO $ setTorrentName tl t newName
                                                           lift . runDB $ updateWhere [ DownloadLinkInfoHash ==. ihash ]
                                                                                      [ DownloadLinkName =. newName ]

                                       case r of
                                          Nothing -> invalidArgs [ "infohash" ]
                                          Just _  -> sendResponse $ RepJson emptyContent
