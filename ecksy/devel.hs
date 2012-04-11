{-# LANGUAGE PackageImports #-}
import "ecksy" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

import "ecksy" Torrent ( makeSession, withLibTorrent )

import "ecksy" Init

-- | TODO: Move to config.
updateFrequency :: Integer
updateFrequency = 24*60*60

main :: IO ()
main = do
    putStrLn "Starting devel application"
    r <- withLibTorrent $ \lTor -> do
            sesh <- makeSession lTor
            -- We only fork the blacklist thread in development! Otherwise,
            -- torrents may start connecting to blacklisted IPs before the
            -- blacklist has time to load.
            _ <- forkIO . keepUpdated lTor [sesh] $ fromIntegral updateFrequency
            (port, app) <- getApplicationDev lTor sesh
            runner <- selectRunner
            forkIO $ runner defaultSettings { settingsPort = port
                                            } app
            loop

    case r of
        Left msg -> do putStrLn $ "[!!] Error: " ++ msg
                       main
        Right _  -> return ()

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
