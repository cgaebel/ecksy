{-# LANGUAGE OverloadedStrings #-}
module Init ( selectRunner
            , keepUpdated
            ) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad

import Data.Attoparsec.ByteString as A

import Data.ByteString ( ByteString )
import Data.Char
import Data.Maybe
import Data.Text.Encoding ( decodeUtf8' )
import Data.Word

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
        updateAll = do flushOut "Updating blocklist."
                       filt <- getBlocklist lt
                       mapM_ (\s -> setIPFilter lt s filt) sx

pauseFor :: NominalDiffTime -> IO ()
pauseFor t | t < realToFrac (maxBound :: Int) / 1e6 = threadDelay $ floor (realToFrac t * 1e6 :: Double)
           | otherwise                             = do threadDelay maxBound
                                                        pauseFor $ t - 1e6 * (fromIntegral (maxBound :: Int))

getBlocklist ::  LTor -> IO IPFilter
getBlocklist lt = do f <- makeIPFilter lt
                     let blockRange = addFilteredRange lt f
                     mapM_ (keepTryingToUpdateBlockList blockRange) blocklists
                     return f

-- | Repeatedly tries to update the block list with a url it succeeds. This is
--   needed because the iblocklist servers are very finicky.
--
--   TODO: Logger!
keepTryingToUpdateBlockList :: (Text -> Text -> IO ()) -> Text -> IO ()
keepTryingToUpdateBlockList blockRange url = do r <- (E.try $ updateBlockList blockRange url) :: IO (Either E.SomeException ())
                                                case r of
                                                    Left err -> do flushOut $ "Blocklist update failed [ " ++ show err ++ " ]. Retrying..."
                                                                   keepTryingToUpdateBlockList blockRange url
                                                    Right () -> return ()

-- | TODO: Update this to use stm-conduit. HTTP in one thread,
--         gzip and parsing in the other.
updateBlockList :: (Text -> Text -> IO ()) -- ^ The "add range" function
                -> Text -- ^ The url of the (possibly gzipped) p2p blocklist.
                -> IO ()
updateBlockList blockRange url = do req <- CH.parseUrl $ unpack url
                                    CH.withManager $ \man -> do
                                        CH.Response _ _ hdrs src <- CH.http req man
                                        let unzipper = case isJust $ lookup "application/x-gzip" hdrs of
                                                          True -> ungzip
                                                          False -> CL.mapM (return . id)
                                        src
                                          $= unzipper
                                          $= CB.lines
                                          $$ CL.mapM_ (liftIO . blockRange' . parseLine)
    where
        blockRange' :: Result (Text, Text) -> IO ()
        blockRange' (A.Fail _ _ _)    = return ()
        blockRange' (A.Done _ (x, y)) = blockRange x y
        blockRange' (A.Partial f)     = blockRange' $ f ""

char2word :: Char -> Word8
char2word = fromIntegral . ord

-- | The format of a p2p file is:
--
--   identifier:0.0.0.0-255.255.255.255
parseLine :: ByteString -> Result (Text, Text)
parseLine src = parse parser src
    where
        parser :: Parser (Text, Text)
        parser = do skipWhile (/= char2word ':')
                    _ <- word8 $ char2word ':'
                    ip1 <- tryDecodeUtf8 =<< A.takeWhile (/= char2word '-')
                    _ <- word8 $ char2word '-'
                    ip2 <- tryDecodeUtf8 =<< takeByteString
                    return (ip1, ip2)
            where
                tryDecodeUtf8 :: ByteString -> Parser Text
                tryDecodeUtf8 bs = case decodeUtf8' bs of
                                      Left _ -> fail "non-utf8 character found. dropping line."
                                      Right t -> return t

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
