module Handler.AddMagnetLink ( postAddMagnetLinkR ) where

import Auth ( requireLogin )
import Import
import Torrent

postAddMagnetLinkR :: Handler RepJson
postAddMagnetLinkR = requireLogin $ do mlink <- runInputPost $ ireq textField "link"

                                       case parseMagnetLink mlink of
                                           Just ((ihash, Just dn, _)) -> go mlink ihash dn
                                           _                          -> invalidArgs [ "link" ]
    where
        go :: Text -> Text -> Text -> Handler RepJson
        go mlink ihash dn = do _ <- runDB . insertUnique $ DownloadLink ihash dn mlink

                               (tl, ses) <- getTorSession <$> getYesod
                               t <- liftIO . addMagnetURI tl ses mlink $ downloadFolder ++ "/" ++ ihash ++ "/"
                               case t of
                                  Just t' -> do liftIO $ setTorrentName tl t' dn
                                                sendResponse $ RepJson emptyContent
                                  Nothing -> invalidArgs [ "link" ]
