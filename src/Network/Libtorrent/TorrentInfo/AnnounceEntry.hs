{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | <http://www.libtorrent.org/reference-Core.html#announce-entry announce_entry> structure for "Libtorrent"

module Network.Libtorrent.TorrentInfo.AnnounceEntry( TrackerSource(..)
                                           , AnnounceEntry(..)
                                           , newAnnounceEntry
                                           , nextAnnounceIn
                                           , minAnnounceIn
                                           , reset
                                           , canAnnounce
                                           , isWorking
                                           , trim
                                           , getAnnounceEntryUrl
                                           , setAnnounceEntryUrl
                                           , getAnnounceEntryTrackerid
                                           , setAnnounceEntryTrackerid
                                           , getMessage
                                           , setMessage
                                           , getLastError
                                           , getAnnounceEntryNextAnnounce
                                           , getMinAnnounce
                                           , getScrapeIncomplete
                                           , getScrapeComplete
                                           , getScrapeDownloaded
                                           , getTier
                                           , getFailLimit
                                           , getFails
                                           , getUpdating
                                           , getSource
                                           , getVerified
                                           , getStartSent
                                           , getCompleteSent
                                           , getSendStats
                                           ) where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Text                    (Text)
import qualified Data.Text.Foreign            as TF
import           Data.Word                    (Word64, Word8)
import           Foreign.C.Types              (CInt)
import           Foreign.ForeignPtr           (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils        (toBool)
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Cpp        as C
import qualified Language.C.Inline.Unsafe     as CU

import           Network.Libtorrent.ErrorCode
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.String
import           Network.Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/torrent_info.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data TrackerSource =
  TrackerSourceNone
  | SourceTorrent
  | SourceClient
  | SourceMagnetLink
  | SourceTex
  deriving (Show, Enum, Bounded, Eq, Ord)

newtype AnnounceEntry = AnnounceEntry { unAnnounceEntry :: ForeignPtr (CType AnnounceEntry)}

instance Show AnnounceEntry where
  show _ = "AnnounceEntry"

instance Inlinable AnnounceEntry where
  type (CType AnnounceEntry) = C'AnnounceEntry

instance FromPtr AnnounceEntry where
  fromPtr = objFromPtr AnnounceEntry $ \ptr ->
    [CU.exp| void { delete $(announce_entry * ptr); } |]

instance WithPtr AnnounceEntry where
  withPtr (AnnounceEntry fptr) = withForeignPtr fptr


newAnnounceEntry :: MonadIO m =>  Text -> m AnnounceEntry
newAnnounceEntry url = liftIO $ do
  s <- textToStdString url
  withPtr s $ \sPtr ->
    fromPtr [CU.exp| announce_entry * { new announce_entry(*$(string * sPtr)) } |]

nextAnnounceIn :: MonadIO m =>  AnnounceEntry -> m CInt
nextAnnounceIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->next_announce_in() } |]

minAnnounceIn :: MonadIO m =>  AnnounceEntry -> m CInt
minAnnounceIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->min_announce_in() } |]

reset :: MonadIO m =>  AnnounceEntry -> m ()
reset ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(announce_entry * hoPtr)->reset() } |]

-- void failed (aux::session_settings const& sett, int retry_interval = 0);

canAnnounce :: MonadIO m =>  AnnounceEntry -> Word64 -> Bool -> m Bool
canAnnounce ho now isSeed =
  liftIO . withPtr ho $ \hoPtr -> do
  let isSeed' = if isSeed then 1 else 0
  res <- [CU.exp| bool { $(announce_entry * hoPtr)->can_announce(ptime($(uint64_t now)), $(bool isSeed')) } |]
  return $ res > 0

isWorking :: MonadIO m =>  AnnounceEntry -> m Bool
isWorking ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(announce_entry * hoPtr)->is_working() } |]

trim :: MonadIO m =>  AnnounceEntry -> m ()
trim ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(announce_entry * hoPtr)->trim() } |]

getAnnounceEntryUrl :: MonadIO m =>  AnnounceEntry -> m Text
getAnnounceEntryUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->url) } |]
  stdStringToText res

setAnnounceEntryUrl :: MonadIO m =>  AnnounceEntry -> Text -> m ()
setAnnounceEntryUrl ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->url = std::string($(const char * cstr), $(size_t clen))} |]

getAnnounceEntryTrackerid :: MonadIO m =>  AnnounceEntry -> m Text
getAnnounceEntryTrackerid ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->trackerid) } |]
  stdStringToText res

setAnnounceEntryTrackerid :: MonadIO m =>  AnnounceEntry -> Text -> m ()
setAnnounceEntryTrackerid ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->trackerid = std::string($(const char * cstr), $(size_t clen))} |]

getMessage :: MonadIO m =>  AnnounceEntry -> m Text
getMessage ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->message) } |]
  stdStringToText res

setMessage :: MonadIO m =>  AnnounceEntry -> Text -> m ()
setMessage ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->message = std::string($(const char * cstr), $(size_t clen))} |]

getLastError :: MonadIO m =>  AnnounceEntry -> m ErrorCode
getLastError ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(announce_entry * hoPtr)->last_error) } |]

getAnnounceEntryNextAnnounce :: MonadIO m =>  AnnounceEntry -> m Word64
getAnnounceEntryNextAnnounce ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(announce_entry * hoPtr)->next_announce.time } |]

getMinAnnounce :: MonadIO m =>  AnnounceEntry -> m Word64
getMinAnnounce ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(announce_entry * hoPtr)->min_announce.time } |]

getScrapeIncomplete :: MonadIO m =>  AnnounceEntry -> m CInt
getScrapeIncomplete ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->scrape_incomplete } |]

getScrapeComplete :: MonadIO m =>  AnnounceEntry -> m CInt
getScrapeComplete ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(announce_entry * hoPtr)->scrape_complete } |]

getScrapeDownloaded :: MonadIO m =>  AnnounceEntry -> m CInt
getScrapeDownloaded ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->scrape_downloaded } |]

getTier :: MonadIO m =>  AnnounceEntry -> m Word8
getTier ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->tier } |]

getFailLimit :: MonadIO m =>  AnnounceEntry -> m Word8
getFailLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->fail_limit } |]

getFails :: MonadIO m =>  AnnounceEntry -> m Word8
getFails ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->fails } |]

getUpdating :: MonadIO m =>  AnnounceEntry -> m Bool
getUpdating ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(announce_entry * hoPtr)->updating } |]

getSource :: MonadIO m =>  AnnounceEntry -> m (BitFlags TrackerSource)
getSource ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(announce_entry * hoPtr)->source } |]

getVerified :: MonadIO m =>  AnnounceEntry -> m Bool
getVerified ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(announce_entry * hoPtr)->verified } |]

getStartSent :: MonadIO m =>  AnnounceEntry -> m Bool
getStartSent ho =
  liftIO . withPtr ho $ \hoPtr ->
  (> 0 ) <$> [CU.exp| bool { $(announce_entry * hoPtr)->start_sent } |]

getCompleteSent :: MonadIO m =>  AnnounceEntry -> m Bool
getCompleteSent ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(announce_entry * hoPtr)->complete_sent } |]

getSendStats :: MonadIO m =>  AnnounceEntry -> m Bool
getSendStats ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(announce_entry * hoPtr)->send_stats } |]


