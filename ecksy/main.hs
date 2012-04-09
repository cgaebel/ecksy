import Prelude              (IO)

import Settings             (parseExtra)

import Import

import Application          (makeApplication, selectRunner)

import Yesod.Default.Config
import Yesod.Logger (defaultDevelopmentLogger)
import Network.Wai.Handler.Warp
    (defaultSettings, settingsPort, settingsHost)

import Init

import Torrent.C

-- | TODO: Move to config.
updateFrequency :: Integer
updateFrequency = 24*60*60

main :: IO ()
main = do r <- withLibTorrent $ \lt -> do
               config <- fromArgs parseExtra
               logger <- defaultDevelopmentLogger
               sesh <- makeSession lt
               keepUpdated lt sesh $ fromIntegral updateFrequency
               app <- makeApplication lt sesh config logger
               print $ appHost config
               runner <- selectRunner
               runner defaultSettings { settingsPort = appPort config
                                      , settingsHost = appHost config
                                      } app
          case r of
            Left msg -> putStrLn $ "[!!] Error: " ++ msg
            Right _  -> return ()
