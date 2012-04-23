{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TemplateHaskell #-}
module Torrent.C ( IPFilter
                 , Torrent
                 , TorrentState(..)
                 , Session
                 , LTor( makeIPFilter
                       , addFilteredRange

                       , torrentSavePath
                       , torrentName
                       , setTorrentName
                       , setRatio
                       , setTorrentUploadLimit
                       , torrentUploadLimit
                       , setTorrentDownloadLimit
                       , torrentDownloadLimit
                       , pauseTorrent
                       , resumeTorrent
                       , isPaused
                       , isSeed
                       , infoHash
                       , torrentProgress
                       , torrentDownloadRate
                       , torrentUploadRate
                       , torrentSize
                       , totalDownloaded
                       , moveStorage
                       , torrentState
                       , toMagnetURI

                       , makeSession
                       , addMagnetURI
                       , pauseSession
                       , resumeSession
                       , isSessionPaused
                       , removeTorrent
                       , findTorrent
                       , getTorrents
                       , setSessionUploadRateLimit
                       , sessionUploadRateLimit
                       , setSessionDownloadRateLimit
                       , sessionDownloadRateLimit
                       , setIPFilter
                       )
                 , withLibTorrent
                 ) where

import Control.Applicative
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe

import Data.Aeson.TH
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.List as L
import Data.Maybe
import Data.Text

import Prelude
import System.Posix.DynamicLinker

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Torrent.Magnet

-- | Performs a free in a new thread, for maximum speed.
asyncFree :: Ptr a -> IO ()
asyncFree p = do _ <- forkIO $ free p
                 return ()

data IPFilter_
newtype IPFilter = IPF (ForeignPtr IPFilter_)

data Torrent_
data Torrent = TOR (ForeignPtr Torrent_)
                   (TVar Text)

data Session_
data Session = SES (ForeignPtr Session_)
                   (TVar (HashMap InfoHash Text))

data LTor = LTor { makeIPFilter :: IO IPFilter
                 , addFilteredRange :: IPFilter -> Text -> Text -> IO ()

                 , torrentSavePath :: Torrent -> IO Text
                 , torrentName :: Torrent -> IO Text
                 , setTorrentName :: Torrent -> Text -> IO ()
                 , setRatio :: Torrent -> Double -> IO ()
                 , setTorrentUploadLimit :: Torrent -> Int -> IO ()
                 , torrentUploadLimit :: Torrent -> IO Int
                 , setTorrentDownloadLimit :: Torrent -> Int -> IO ()
                 , torrentDownloadLimit :: Torrent -> IO Int
                 , pauseTorrent :: Torrent -> IO ()
                 , resumeTorrent :: Torrent -> IO ()
                 , isPaused :: Torrent -> IO Bool
                 , isSeed :: Torrent -> IO Bool
                 , infoHash :: Torrent -> IO Text
                 , torrentProgress :: Torrent -> IO Double
                 , torrentDownloadRate :: Torrent -> IO Int
                 , torrentUploadRate :: Torrent -> IO Int
                 , torrentSize :: Torrent -> IO Integer
                 , totalDownloaded :: Torrent -> IO Integer
                 , moveStorage :: Torrent -> Text -> IO ()
                 , torrentState :: Torrent -> IO TorrentState
                 , toMagnetURI :: Torrent -> IO Text

                 , makeSession :: IO Session
                 , addMagnetURI :: Session -> Text -> Text -> IO (Maybe Torrent)
                 , pauseSession :: Session -> IO ()
                 , resumeSession :: Session -> IO ()
                 , isSessionPaused :: Session -> IO Bool
                 , removeTorrent :: Session -> Torrent -> Bool -> IO ()
                 , findTorrent :: Session -> Text -> IO (Maybe Torrent)
                 , getTorrents :: Session -> IO [Torrent]
                 , setSessionUploadRateLimit :: Session -> Int -> IO ()
                 , sessionUploadRateLimit :: Session -> IO Int
                 , setSessionDownloadRateLimit :: Session -> Int -> IO ()
                 , sessionDownloadRateLimit :: Session -> IO Int
                 , setIPFilter :: Session -> IPFilter -> IO ()
                 }

data TorrentState = QueuedForChecking
                  | Checking
                  | DownloadingMetadata
                  | Downloading
                  | Finished
                  | Seeding
                  | Allocating
                  | CheckingResumeData
    deriving (Show, Enum, Bounded)

$(deriveJSON id ''TorrentState)

type Err = String

withText :: Text -> (CString -> IO a) -> IO a
withText t = withCString (unpack t)

validateFPtr :: String -> FunPtr a -> ErrorT Err IO (FunPtr a)
validateFPtr m f | f == nullFunPtr = throwError $ m ++ " should not be null!"
                 | otherwise      = return f

nullCheck :: Ptr a -> Ptr a
nullCheck p = assert (p /= nullPtr) p

getFunc :: DL -> String -> ErrorT Err IO (FunPtr a)
getFunc dl name = liftIO (dlsym dl name) >>= validateFPtr name

getFreeFunc :: DL -> String -> ErrorT Err IO (FunPtr (Ptr a -> IO ()))
getFreeFunc = getFunc

type MakeIPFilter = IO (Ptr IPFilter_)
foreign import ccall "dynamic"
    mkMakeIPFilter :: FunPtr MakeIPFilter -> MakeIPFilter

adaptMakeIPFilter :: FunPtr (Ptr IPFilter_ -> IO ()) -> MakeIPFilter -> IO IPFilter
adaptMakeIPFilter freeFunc f = IPF <$> (newForeignPtr freeFunc =<< nullCheck <$> f)

type AddFilteredRange = Ptr IPFilter_ -> CString -> CString -> IO ()
foreign import ccall "dynamic"
    mkAddFilteredRange :: FunPtr AddFilteredRange -> AddFilteredRange

adaptAddFilteredRange :: AddFilteredRange -> (IPFilter -> Text -> Text -> IO ())
adaptAddFilteredRange f (IPF fp) start' end' = withText start' $ \start ->
                                               withText end'   $ \end ->
                                               withForeignPtr fp  $ \p ->
                                                   f p start end

type TorrentSavePath = Ptr Torrent_ -> IO CString
foreign import ccall "dynamic"
    mkTorrentSavePath :: FunPtr TorrentSavePath -> TorrentSavePath

adaptTorrentSavePath :: TorrentSavePath -> (Torrent -> IO Text)
adaptTorrentSavePath f (TOR fp _) = withForeignPtr fp $ \p -> do
                                    cstr <- nullCheck <$> f p
                                    str <- peekCString cstr
                                    asyncFree cstr
                                    return $ pack str

type SetRatio = Ptr Torrent_ -> CFloat -> IO ()
foreign import ccall "dynamic"
    mkSetRatio :: FunPtr SetRatio -> SetRatio

adaptSetRatio :: SetRatio -> (Torrent -> Double -> IO ())
adaptSetRatio f (TOR fp _) r = withForeignPtr fp $ \p ->
                               f p $ realToFrac r

type SetTorrentLimit = Ptr Torrent_ -> CInt -> IO ()
foreign import ccall "dynamic"
    mkSetTorrentLimit :: FunPtr SetTorrentLimit -> SetTorrentLimit

adaptSetTorrentLimit :: SetTorrentLimit -> (Torrent -> Int -> IO ())
adaptSetTorrentLimit f (TOR fp _) lim = withForeignPtr fp $ \p ->
                                        f p $ fromIntegral lim

type GetTorrentLimit = Ptr Torrent_ -> IO CInt
foreign import ccall "dynamic"
    mkGetTorrentLimit :: FunPtr GetTorrentLimit -> GetTorrentLimit

adaptGetTorrentLimit :: GetTorrentLimit -> (Torrent -> IO Int)
adaptGetTorrentLimit f (TOR fp _) = withForeignPtr fp $ \p ->
                                    fromIntegral <$> f p

type TorrentAction = Ptr Torrent_ -> IO ()
foreign import ccall "dynamic"
    mkTorrentAction :: FunPtr TorrentAction -> TorrentAction

adaptTorrentAction :: TorrentAction -> (Torrent -> IO ())
adaptTorrentAction f (TOR fp _) = withForeignPtr fp f

type TorrentBool = Ptr Torrent_ -> IO CInt
foreign import ccall "dynamic"
    mkTorrentBool :: FunPtr TorrentBool -> TorrentBool

adaptTorrentBool :: TorrentBool -> (Torrent -> IO Bool)
adaptTorrentBool f (TOR fp _) = withForeignPtr fp $ \p -> do
                                r <- f p
                                case r of
                                    0 -> return False
                                    _ -> return True

type InfoHashT = Ptr Torrent_ -> IO CString

mkInfoHash :: FunPtr TorrentSavePath -> TorrentSavePath
mkInfoHash = mkTorrentSavePath

adaptInfoHash :: InfoHashT -> (Torrent -> IO Text)
adaptInfoHash = adaptTorrentSavePath

type TorrentProgress = Ptr Torrent_ -> IO CFloat
foreign import ccall "dynamic"
    mkTorrentProgress :: FunPtr TorrentProgress -> TorrentProgress

adaptTorrentProgress :: TorrentProgress -> (Torrent -> IO Double)
adaptTorrentProgress f (TOR fp _) = withForeignPtr fp $ \p ->
                                    realToFrac <$> f p

-- Limits and rates have the same data types.
mkGetTorrentRate :: FunPtr GetTorrentLimit -> GetTorrentLimit
mkGetTorrentRate = mkGetTorrentLimit
adaptGetTorrentRate :: GetTorrentLimit -> Torrent -> IO Int
adaptGetTorrentRate = adaptGetTorrentLimit

type TorrentTotalSize = Ptr Torrent_ -> IO CSize
foreign import ccall "dynamic"
    mkTorrentTotalSize :: FunPtr TorrentTotalSize -> TorrentTotalSize

adaptTorrentTotalSize :: TorrentTotalSize -> (Torrent -> IO Integer)
adaptTorrentTotalSize f (TOR fp _) = withForeignPtr fp $ \p ->
                                     fromIntegral <$> f p

type MoveStorage = Ptr Torrent_ -> CString -> IO ()
foreign import ccall "dynamic"
    mkMoveStorage :: FunPtr MoveStorage -> MoveStorage

adaptMoveStorage :: MoveStorage -> (Torrent -> Text -> IO ())
adaptMoveStorage f (TOR fp _) s = withForeignPtr fp $ \p ->
                                  withText s $ \cs ->
                                  f p cs

type TorrentStateFunc = Ptr Torrent_ ->  IO CInt
foreign import ccall "dynamic"
    mkTorrentState :: FunPtr TorrentStateFunc -> TorrentStateFunc

adaptTorrentState :: TorrentStateFunc -> (Torrent -> IO TorrentState)
adaptTorrentState f (TOR fp _) = withForeignPtr fp $ \p ->
                                 toEnum <$> fromIntegral <$> f p

type MakeSession = IO (Ptr Session_)
foreign import ccall "dynamic"
    mkMakeSession :: FunPtr MakeSession -> MakeSession

adaptMakeSession :: FunPtr (Ptr Session_ -> IO ()) -> MakeSession -> IO Session
adaptMakeSession freeFunc f = SES <$> (newForeignPtr freeFunc =<< nullCheck <$> f)
                                  <*> (atomically $ newTVar M.empty)

type AddMagnetURI = Ptr Session_ -> CString -> CString -> IO (Ptr Torrent_)
foreign import ccall "dynamic"
    mkAddMagnetURI :: FunPtr AddMagnetURI -> AddMagnetURI

adaptAddMagnetURI :: FunPtr (Ptr Torrent_ -> IO ())
                  -> AddMagnetURI
                  -> (Session -> Text -> Text -> IO (Maybe Torrent))
adaptAddMagnetURI freeFunc f (SES s' m) uri' tgt' = withForeignPtr s' $ \s ->
                                                    withText uri' $ \uri -> do
                                                     case parseMagnetLink uri' of
                                                        Just (ihash, mname, _) -> withText tgt' $ \tgt -> do
                                                                                  t <- newForeignPtr freeFunc =<< nullCheck <$> f s uri tgt
                                                                                  name <- atomically $ do let mname' = fromMaybe "" mname
                                                                                                          modifyTVar' m $ M.insert ihash mname'
                                                                                                          newTVar mname'
                                                                                  return . Just $ TOR t name
                                                        Nothing -> return Nothing

type SessionAction = Ptr Session_ -> IO ()
foreign import ccall "dynamic"
    mkSessionAction :: FunPtr SessionAction -> SessionAction

adaptSessionAction :: SessionAction -> (Session -> IO ())
adaptSessionAction f (SES s _) = withForeignPtr s f

type SessionPaused = Ptr Session_ -> IO CInt
foreign import ccall "dynamic"
    mkSessionPaused :: FunPtr SessionPaused -> SessionPaused

adaptSessionPaused :: SessionPaused -> (Session -> IO Bool)
adaptSessionPaused f (SES s _) = withForeignPtr s $ \p -> do
                                 r <- f p
                                 case r of
                                    0 -> return False
                                    _ -> return True

type RemoveTorrent = Ptr Session_ -> Ptr Torrent_ -> CInt -> IO ()
foreign import ccall "dynamic"
    mkRemoveTorrent :: FunPtr RemoveTorrent -> RemoveTorrent

adaptRemoveTorrent :: (Torrent -> IO InfoHash)
                   -> RemoveTorrent
                   -> (Session -> Torrent -> Bool -> IO ())
adaptRemoveTorrent getIHash f (SES s' m) tor@(TOR t' _) del = withForeignPtr s' $ \s ->
                                                              withForeignPtr t' $ \t -> do
                                                               ihash <- getIHash tor
                                                               atomically . modifyTVar' m $ M.delete ihash
                                                               f s t . fromIntegral $ fromEnum del

type FindTorrent = Ptr Session_ -> CString -> IO (Ptr Torrent_)
foreign import ccall "dynamic"
    mkFindTorrent :: FunPtr FindTorrent -> FindTorrent

adaptFindTorrent :: FunPtr (Ptr Torrent_ -> IO ()) -> FindTorrent -> (Session -> Text -> IO (Maybe Torrent))
adaptFindTorrent freeFunc f (SES s' m) h' = withForeignPtr s' $ \s ->
                                            withText h' $ \h -> runMaybeT $ do
                                             tp <- getRet =<< liftIO (f s h)
                                             mapMaybeT atomically $ do
                                                name <- MaybeT $ M.lookup h' <$> readTVar m
                                                TOR tp <$> lift (newTVar name)
    where
        getRet p | p == nullPtr = mzero
                 | otherwise   = liftIO $ newForeignPtr freeFunc p

data TorrentList_
type TorrentList  = ForeignPtr TorrentList_

type TListElems = Ptr TorrentList_ -> IO CInt
foreign import ccall "dynamic"
    mkTListElems :: FunPtr TListElems -> TListElems

adaptTListElems :: TListElems -> (TorrentList -> IO Int)
adaptTListElems f fp = withForeignPtr fp $ (fromIntegral <$>) . f

type TListDump = Ptr TorrentList_ -> Ptr (Ptr Torrent_) -> IO ()
foreign import ccall "dynamic"
    mkTListDump :: FunPtr TListDump -> TListDump

adaptTListDump :: FunPtr (Ptr Torrent_ -> IO ())
               -> (TorrentList -> IO Int)
               -> TListDump
               -> (TorrentList -> IO [ForeignPtr Torrent_])
adaptTListDump fTor getLen f fp = do len <- getLen fp
                                     (mapM (newForeignPtr fTor) =<<) $
                                       withForeignPtr fp $ \p ->
                                        allocaArray len $ \tors ->
                                         f p tors >> peekArray len tors

type GetTorrents = Ptr Session_ -> IO (Ptr TorrentList_)
foreign import ccall "dynamic"
    mkGetTorrents :: FunPtr GetTorrents -> GetTorrents

adaptGetTorrents :: DL
                 -> (Torrent -> IO InfoHash)
                 -> GetTorrents
                 -> ErrorT Err IO (Session -> IO [Torrent])
adaptGetTorrents dl ih f = do f_tl <- getFreeFunc dl "free_torrent_list"
                              f_th <- getFreeFunc dl "free_torrent_handle"
                              tl_elems <- adaptTListElems <$> mkTListElems <$> getFunc dl "tlist_elems"
                              tl_dump  <- adaptTListDump f_th tl_elems <$> mkTListDump <$> getFunc dl "tlist_dump"

                              return $ \(SES s m) -> withForeignPtr s $ \p -> do
                                                     ts <- tl_dump =<< newForeignPtr f_tl =<< f p
                                                     is <- mapM (\t -> ih $ TOR t undefined) ts
                                                     ns <- atomically $ do m' <- readTVar m
                                                                           mapM (\i -> newTVar $ M.lookupDefault "" i m') is
                                                     return . L.map (uncurry TOR) $ L.zip ts ns

type SetSessionLimit = Ptr Session_ -> CInt -> IO ()
foreign import ccall "dynamic"
    mkSetSessionLimit :: FunPtr SetSessionLimit -> SetSessionLimit

adaptSetSessionLimit :: SetSessionLimit -> (Session -> Int -> IO ())
adaptSetSessionLimit f (SES s _) lim = withForeignPtr s $ \p ->
                                       f p $ fromIntegral lim

type GetSessionLimit = Ptr Session_ -> IO CInt
foreign import ccall "dynamic"
    mkGetSessionLimit :: FunPtr GetSessionLimit -> GetSessionLimit

adaptGetSessionLimit :: GetSessionLimit -> (Session -> IO Int)
adaptGetSessionLimit f (SES s _) = withForeignPtr s $ \p ->
                                   fromIntegral <$> f p

type SetIPFilter = Ptr Session_ -> Ptr IPFilter_ -> IO ()
foreign import ccall "dynamic"
    mkSetIPFilter :: FunPtr SetIPFilter -> SetIPFilter

adaptSetIPFilter :: SetIPFilter -> (Session -> IPFilter -> IO ())
adaptSetIPFilter f (SES s' _) (IPF filt) = withForeignPtr s' $ \s ->
                                           withForeignPtr filt $ f s

-- | Dynamically loads the libtorrent C bindings. If an error occurs, it will
--   be in Left. Otherwise, the passed function will be executed with a valid
--   LibTorrent instance.
withLibTorrent :: (LTor -> IO a) -> IO (Either String a)
withLibTorrent f = withDL "liblibtorrent-c.so" [ RTLD_LAZY ] $ \dl -> runErrorT $ do
                    f_ipf   <- getFreeFunc dl "free_ip_filter"
                    f_th    <- getFreeFunc dl "free_torrent_handle"
                    f_ses   <- getFreeFunc dl "free_session"

                    f_ih <- adaptInfoHash . mkInfoHash <$> getFunc dl "info_hash"

                    -- If functions have the same params/return value, we might reuse their adapters/makers. Don't be scared.
                    liftIO . f =<< LTor <$> (adaptMakeIPFilter f_ipf   . mkMakeIPFilter     <$> getFunc dl "make_ip_filter")
                                        <*> (adaptAddFilteredRange     . mkAddFilteredRange <$> getFunc dl "add_filtered_range")

                                        <*> (adaptTorrentSavePath      . mkTorrentSavePath  <$> getFunc dl "torrent_save_path")
                                        <*> (return $ \(TOR _ name) -> atomically $ readTVar name) -- torrentName
                                        <*> (return $ \(TOR _ name) -> atomically . writeTVar name) -- setTorrentName
                                        <*> (adaptSetRatio             . mkSetRatio         <$> getFunc dl "set_ratio")
                                        <*> (adaptSetTorrentLimit      . mkSetTorrentLimit  <$> getFunc dl "set_torrent_upload_limit")
                                        <*> (adaptGetTorrentLimit      . mkGetTorrentLimit  <$> getFunc dl "get_torrent_upload_limit")
                                        <*> (adaptSetTorrentLimit      . mkSetTorrentLimit  <$> getFunc dl "set_torrent_download_limit")
                                        <*> (adaptGetTorrentLimit      . mkGetTorrentLimit  <$> getFunc dl "get_torrent_download_limit")
                                        <*> (adaptTorrentAction        . mkTorrentAction    <$> getFunc dl "pause_torrent")
                                        <*> (adaptTorrentAction        . mkTorrentAction    <$> getFunc dl "resume_torrent")
                                        <*> (adaptTorrentBool          . mkTorrentBool      <$> getFunc dl "is_paused")
                                        <*> (adaptTorrentBool          . mkTorrentBool      <$> getFunc dl "is_seed")
                                        <*> pure f_ih -- infoHash
                                        <*> (adaptTorrentProgress      . mkTorrentProgress  <$> getFunc dl "torrent_progress")
                                        <*> (adaptGetTorrentRate       . mkGetTorrentRate   <$> getFunc dl "torrent_download_rate")
                                        <*> (adaptGetTorrentRate       . mkGetTorrentRate   <$> getFunc dl "torrent_upload_rate")
                                        <*> (adaptTorrentTotalSize     . mkTorrentTotalSize <$> getFunc dl "total_torrent_size")
                                        <*> (adaptTorrentTotalSize     . mkTorrentTotalSize <$> getFunc dl "total_downloaded")
                                        <*> (adaptMoveStorage          . mkMoveStorage      <$> getFunc dl "move_storage")
                                        <*> (adaptTorrentState         . mkTorrentState     <$> getFunc dl "torrent_state")
                                        <*> (adaptInfoHash             . mkInfoHash         <$> getFunc dl "torrent_magnet_uri")

                                        <*> (adaptMakeSession f_ses    . mkMakeSession      <$> getFunc dl "make_session")
                                        <*> (adaptAddMagnetURI f_th    . mkAddMagnetURI     <$> getFunc dl "add_magnet_uri")
                                        <*> (adaptSessionAction        . mkSessionAction    <$> getFunc dl "pause_session")
                                        <*> (adaptSessionAction        . mkSessionAction    <$> getFunc dl "resume_session")
                                        <*> (adaptSessionPaused        . mkSessionPaused    <$> getFunc dl "is_session_paused")
                                        <*> (adaptRemoveTorrent f_ih   . mkRemoveTorrent    <$> getFunc dl "remove_torrent")
                                        <*> (adaptFindTorrent f_th     . mkFindTorrent      <$> getFunc dl "find_torrent")
                                        <*> (adaptGetTorrents dl f_ih =<< mkGetTorrents     <$> getFunc dl "get_torrents")
                                        <*> (adaptSetSessionLimit      .  mkSetSessionLimit <$> getFunc dl "set_session_upload_rate_limit")
                                        <*> (adaptGetSessionLimit      .  mkGetSessionLimit <$> getFunc dl "session_upload_rate_limit")
                                        <*> (adaptSetSessionLimit      .  mkSetSessionLimit <$> getFunc dl "set_session_download_rate_limit")
                                        <*> (adaptGetSessionLimit      .  mkGetSessionLimit <$> getFunc dl "session_download_rate_limit")
                                        <*> (adaptSetIPFilter          .  mkSetIPFilter     <$> getFunc dl "set_ip_filter")
