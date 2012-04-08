import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

import Torrent.C

main :: IO ()
main = do r <- withLibTorrent \lt ->
               defaultMain (fromArgs parseExtra) $ makeApplication lt
       case r of
          Left msg -> putStrLn $ "[!!] Error: " ++ msg
          Right _  -> return ()
