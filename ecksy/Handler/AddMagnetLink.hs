module Handler.AddMagnetLink ( postAddMagnetLinkR ) where

import Auth ( requireLogin )
import Import
import Torrent

postAddMagnetLinkR :: Handler RepJson
postAddMagnetLinkR = requireLogin $ do mlink <- runInputPost $ ireq textField "link"

                                       (tl, ses) <- getTorSession <$> getYesod

                                       r <- runMaybeT $ do (ihash, Just dn, _) <- MaybeT . return $ parseMagnetLink mlink

                                                           t <- MaybeT . liftIO . addMagnetURI tl ses mlink $ downloadFolder ++ "/" ++ ihash ++ "/"
                                                           liftIO $ setTorrentName tl t dn

                                                           lift . runDB . insertUnique $ DownloadLink ihash dn mlink

                                       case r of
                                           Just _  -> sendResponse $ RepJson emptyContent
                                           Nothing -> invalidArgs [ "link" ]
