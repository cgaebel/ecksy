{-# LANGUAGE OverloadedStrings #-}
module Init ( selectRunner
            , keepUpdated
            ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad

import Data.Attoparsec.Text as A

import Data.Conduit.TMChan
import Data.Maybe
import Data.Text.Encoding ( decodeUtf8With )
import Data.Text.Encoding.Error ( ignore )

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib
import Data.Text as T
import Data.Time.Clock

import System.IO

import Import

import qualified Network.HTTP.Conduit as CH
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import System.Directory ( doesDirectoryExist )

import Torrent

blocklists :: [Text]
blocklists = [ "http://list.iblocklist.com/?list=bt_level1&fileformat=p2p&archiveformat=gz"
             , "http://list.iblocklist.com/?list=ijfqtofzixtwayqovmxn&fileformat=p2p&archiveformat=gz"
             ]

flushOut :: String -> IO ()
flushOut msg = do putStrLn msg
                  hFlush stdout

-- | Sparks a thread which will keep all the sessions updated with the
--   above blocklists every NominalDiffTime seconds.
--
--   TODO: Use a logger!
keepUpdated :: LTor -> [Session] -> NominalDiffTime -> IO ()
keepUpdated lt sx dt = do updateAll
                          _ <- forkIO . forever $ pauseFor dt >> updateAll
                          return ()
    where
        updateAll :: IO ()
        updateAll = do flushOut "Updating blocklist..."
                       filt <- getBlocklist lt
                       mapM_ (\s -> setIPFilter lt s filt) sx

pauseFor :: NominalDiffTime -> IO ()
pauseFor t | t < realToFrac (maxBound :: Int) / 1e6 = threadDelay $ floor (realToFrac t * 1e6 :: Double)
           | otherwise                             = do threadDelay maxBound
                                                        pauseFor $ t - 1e6 * (fromIntegral (maxBound :: Int))

getBlocklist ::  LTor -> IO IPFilter
getBlocklist lt = do f <- makeIPFilter lt
                     let blockRange = addFilteredRange lt f
                     mapM_ (repeatedly . updateBlockList blockRange) blocklists
                     return f

-- | Keeps trying until no exception occurs. BEWARE: This could end in an infinite loop!
repeatedly :: IO a -> IO a
repeatedly x = do r <- try' x
                  case r of
                     Left  _  -> repeatedly x
                     Right r' -> return r'

try' :: IO a -> IO (Either E.SomeException a)
try' = E.try

-- | TODO: Update this to use stm-conduit. HTTP in one thread,
--         gzip and parsing in the other.
updateBlockList :: (Text -> Text -> IO ()) -- ^ The "add range" function
                -> Text -- ^ The url of the (possibly gzipped) p2p blocklist.
                -> IO ()
updateBlockList blockRange url = do req <- CH.parseUrl $ unpack url
                                    chan <- atomically $ newTBMChan 256
                                    _ <- forkIO . CH.withManager $ \man -> do
                                            CH.Response _ _ hdrs src <- CH.http req man
                                            let unzipper = case isJust $ lookup "application/x-gzip" hdrs of
                                                              True  -> ungzip
                                                              False -> CL.map id
                                            src $= unzipper $$ sinkTBMChan chan

                                    sourceTBMChan chan
                                        $= CB.lines
                                        $$ CL.mapM_ (liftIO . blockRange' . parseLine . decodeUtf8With ignore)
    where
        blockRange' :: Result (Text, Text) -> IO ()
        blockRange' (Fail _ _ _)      = return ()
        blockRange' (A.Done _ (x, y)) = blockRange x y
        blockRange' (Partial f)       = blockRange' $ f ""

parseLine :: Text -> Result (Text, Text)
parseLine src = parse parser src
    where
        parser :: Parser (Text, Text)
        parser = do skipWhile (/= ':')
                    _ <- char ':'
                    parseTwoIPs <|> parser
            where
                parseTwoIPs :: Parser (Text, Text)
                parseTwoIPs = do ip1 <- parseIPAddress
                                 _ <- char '-'
                                 ip2 <- parseIPAddress
                                 return (ip1, ip2)

        -- https://tools.ietf.org/id/draft-main-ipaddr-text-rep-01.txt
        parseIPAddress :: Parser Text
        parseIPAddress = do a <- d8
                            _ <- char '.'
                            b <- d8
                            _ <- char '.'
                            c <- d8
                            _ <- char '.'
                            d <- d8
                            return $ a ++ "." ++ b ++ "." ++ c ++ "." ++ d
            where
                d8 :: Parser Text
                d8 =  do s25 <- string "25"
                         d   <- digit
                         return $ s25 `snoc` d
                  <|> do s2 <- char '2'
                         dx <- satisfy (\c -> c >= '0' &&  c <= '4')
                         dy <- digit
                         return $ pack [ s2, dx, dy ]
                  <|> do s1 <- char '1'
                         dx <- digit
                         dy <- digit
                         return $ pack [ s1, dx, dy ]
                  <|> do s1 <- satisfy (\c -> c >= '1' && c <= '9')
                         dx <- digit
                         return $ pack [ s1, dx ]
                  <|> do d <- digit
                         return $ pack [ d ]
        

-- | Used to select the appropriate runner, depending on whether or not an SSL
--   certificate is available.
--
--   TODO: Select the location of the certificate with the config file.
selectRunner :: IO (Settings -> Application -> IO ())
selectRunner = do e <- doesDirectoryExist "config/cert"
                  case e of
                    True -> do putStrLn "SSL certificate found. Starting site with HTTPS."
                               return $ runTLS (TLSSettings "config/cert/ecksy.crt" "config/cert/ecksy.key")
                    False -> do putStrLn "[**] No SSL certificate found! Starting site with HTTP."
                                return runSettings
