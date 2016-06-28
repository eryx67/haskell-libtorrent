{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | <http://www.libtorrent.org/reference-Core.html#announce-entry announce_entry> structure for "Libtorrent"

module Libtorrent.TorrentInfo.AnnounceEntry( TrackerSource(..)
                                           , AnnounceEntry(..)
                                           , newAnnounceEntry
                                           , nextAnnounceIn
                                           , minAnnounceIn
                                           , reset
                                           , canAnnounce
                                           , isWorking
                                           , trim
                                           , getUrl
                                           , setUrl
                                           , getTrackerid
                                           , setTrackerid
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

import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Data.Word (Word64, Word8)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.ErrorCode
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.String
import           Libtorrent.Types

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
  deriving (Show, Enum, Bounded)

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


newAnnounceEntry :: Text -> IO AnnounceEntry
newAnnounceEntry url = do
  s <- textToStdString url
  withPtr s $ \sPtr ->
    fromPtr [CU.exp| announce_entry * { new announce_entry(*$(string * sPtr)) } |]

nextAnnounceIn :: AnnounceEntry -> IO CInt
nextAnnounceIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->next_announce_in() } |]

minAnnounceIn :: AnnounceEntry -> IO CInt
minAnnounceIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->min_announce_in() } |]

reset :: AnnounceEntry -> IO ()
reset ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(announce_entry * hoPtr)->reset() } |]

-- void failed (aux::session_settings const& sett, int retry_interval = 0);

canAnnounce :: AnnounceEntry -> Word64 -> Bool -> IO Bool
canAnnounce ho now isSeed =
  withPtr ho $ \hoPtr -> do
  let isSeed' = if isSeed then 1 else 0
  res <- [CU.exp| bool { $(announce_entry * hoPtr)->can_announce(ptime($(uint64_t now)), $(bool isSeed')) } |]
  return $ res > 0

isWorking :: AnnounceEntry -> IO Bool
isWorking ho =
  withPtr ho $ \hoPtr ->
  (> 0) <$> [CU.exp| bool { $(announce_entry * hoPtr)->is_working() } |]

trim :: AnnounceEntry -> IO ()
trim ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(announce_entry * hoPtr)->trim() } |]

getUrl :: AnnounceEntry -> IO Text
getUrl ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->url) } |]
  stdStringToText res

setUrl :: AnnounceEntry -> Text -> IO ()
setUrl ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->url = std::string($(const char * cstr), $(size_t clen))} |]

getTrackerid :: AnnounceEntry -> IO Text
getTrackerid ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->trackerid) } |]
  stdStringToText res

setTrackerid :: AnnounceEntry -> Text -> IO ()
setTrackerid ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->trackerid = std::string($(const char * cstr), $(size_t clen))} |]

getMessage :: AnnounceEntry -> IO Text
getMessage ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(announce_entry * hoPtr)->message) } |]
  stdStringToText res

setMessage :: AnnounceEntry -> Text -> IO ()
setMessage ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
  let clen = fromIntegral len
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(announce_entry * hoPtr)->message = std::string($(const char * cstr), $(size_t clen))} |]

getLastError :: AnnounceEntry -> IO ErrorCode
getLastError ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(announce_entry * hoPtr)->last_error) } |]

getAnnounceEntryNextAnnounce :: AnnounceEntry -> IO Word64
getAnnounceEntryNextAnnounce ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(announce_entry * hoPtr)->next_announce.time } |]

getMinAnnounce :: AnnounceEntry -> IO Word64
getMinAnnounce ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(announce_entry * hoPtr)->min_announce.time } |]

getScrapeIncomplete :: AnnounceEntry -> IO CInt
getScrapeIncomplete ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->scrape_incomplete } |]

getScrapeComplete :: AnnounceEntry -> IO CInt
getScrapeComplete ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(announce_entry * hoPtr)->scrape_complete } |]

getScrapeDownloaded :: AnnounceEntry -> IO CInt
getScrapeDownloaded ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(announce_entry * hoPtr)->scrape_downloaded } |]

getTier :: AnnounceEntry -> IO Word8
getTier ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->tier } |]

getFailLimit :: AnnounceEntry -> IO Word8
getFailLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->fail_limit } |]

getFails :: AnnounceEntry -> IO Word8
getFails ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| uint8_t { $(announce_entry * hoPtr)->fails } |]

getUpdating :: AnnounceEntry -> IO Bool
getUpdating ho =
  withPtr ho $ \hoPtr ->
  (> 0) <$> [CU.exp| bool { $(announce_entry * hoPtr)->updating } |]

getSource :: AnnounceEntry -> IO (BitFlags TrackerSource)
getSource ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(announce_entry * hoPtr)->source } |]

getVerified :: AnnounceEntry -> IO Bool
getVerified ho =
  withPtr ho $ \hoPtr ->
  (> 0) <$> [CU.exp| bool { $(announce_entry * hoPtr)->verified } |]

getStartSent :: AnnounceEntry -> IO Bool
getStartSent ho =
  withPtr ho $ \hoPtr ->
  (> 0 ) <$> [CU.exp| bool { $(announce_entry * hoPtr)->start_sent } |]

getCompleteSent :: AnnounceEntry -> IO Bool
getCompleteSent ho =
  withPtr ho $ \hoPtr ->
  (> 0) <$> [CU.exp| bool { $(announce_entry * hoPtr)->complete_sent } |]

getSendStats :: AnnounceEntry -> IO Bool
getSendStats ho =
  withPtr ho $ \hoPtr ->
  (> 0) <$> [CU.exp| bool { $(announce_entry * hoPtr)->send_stats } |]


