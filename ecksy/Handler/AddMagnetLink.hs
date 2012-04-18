module Handler.AddMagnetLink ( postAddMagnetLinkR ) where

import Auth ( requireLogin )
import Import
import Torrent

postAddMagnetLinkR :: Handler RepJson
postAddMagnetLinkR = requireLogin $ do mlink <- runInputPost $ ireq textField "link"

                                       ihash <- case magnetInfoHash mlink of
                                                   Nothing -> invalidArgs [ "link" ]
                                                   Just h  -> return h

                                       _ <- runDB . insertUnique $ DownloadLink ihash mlink

                                       (tl, ses) <- getTorSession <$> getYesod
                                       _ <- liftIO . addMagnetURI tl ses mlink $ downloadFolder <> "/" <> ihash <> "/"

                                       sendResponse $ RepJson emptyContent
