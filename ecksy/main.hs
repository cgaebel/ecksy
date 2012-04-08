import Prelude              (IO)

import Settings             (parseExtra)

import Import

import Application          (makeApplication)

import Yesod.Default.Config
import Yesod.Logger (defaultDevelopmentLogger)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsHost)

import Network.Wai.Handler.WarpTLS

import Torrent.C

main :: IO ()
main = do r <- withLibTorrent $ \lt -> do
               config <- fromArgs parseExtra
               logger <- defaultDevelopmentLogger
               app <- makeApplication lt config logger
               print $ appHost config
               runTLS {-runSettings-} (TLSSettings "/tmp/work/ecksy.crt" "/tmp/work/ecksy.key")
                      defaultSettings { settingsPort = appPort config
                                      , settingsHost = appHost config
                                      } app
          case r of
            Left msg -> putStrLn $ "[!!] Error: " ++ msg
            Right _  -> return ()
