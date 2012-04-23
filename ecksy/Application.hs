{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql ( runMigration, SqlPersist )
import Network.HTTP.Conduit (newManager, def)

import Control.Concurrent

import Torrent

-- Import all relevant handler modules here.
import Handler.AddMagnetLink
import Handler.Config
import Handler.Home
import Handler.Login
import Handler.Logout
import Handler.RenameTorrent
import Handler.UpdateTorrents

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: LTor -> Session -> AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication lTor sesh conf logger = do
    foundation <- makeFoundation lTor sesh conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

makeFoundation :: LTor -> Session -> AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation lTor sesh conf setLogger = do
        m <- newManager def
        s <- staticSite
        dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
                Database.Persist.Store.loadConfig >>=
                Database.Persist.Store.applyEnv
        p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
        Database.Persist.Store.runPool dbconf (runMigration migrateAll >> loadOldTorrents) p

        return $ App conf setLogger s m p dbconf lTor sesh
    where
        loadOldTorrents :: SqlPersist IO ()
        loadOldTorrents = do ts <- map entityVal <$> selectList [] [ Asc DownloadLinkInfoHash ]
                             _ <- liftIO . forkIO $ mapM_ (\(DownloadLink i n m) -> queueForDownload i n m) ts
                             return ()

        queueForDownload :: InfoHash -> DisplayName -> MagnetLink -> IO (Maybe Torrent)
        queueForDownload ihash dn mlink = do t <- addMagnetURI lTor sesh mlink $ downloadFolder ++ "/" ++ ihash ++ "/"
                                             case t of
                                                 Just t' -> do setTorrentName lTor t' dn
                                                               return t
                                                 Nothing -> return t

-- for yesod devel
getApplicationDev :: LTor -> Session -> IO (Int, Application)
getApplicationDev lt s =
    defaultDevelApp loader $ makeApplication lt s
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
