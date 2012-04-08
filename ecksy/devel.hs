{-# LANGUAGE PackageImports #-}
import "ecksy" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

import "ecksy" Torrent ( withLibTorrent )

main :: IO ()
main = do
    putStrLn "Starting devel application"
    r <- withLibTorrent $ \lTor -> do
            (port, app) <- getApplicationDev lTor
            forkIO $ runSettings defaultSettings
                        { settingsPort = port
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
