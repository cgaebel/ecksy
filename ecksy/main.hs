import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

import Torrent.C ()

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
