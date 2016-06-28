{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-RSS.html#feed_handle feed_handle> structure for "Libtorrent"

module Libtorrent.Rss (FeedHandle
                      , FeedStatus
                      , FeedSettings
                      , FeedItem
                      , unFeedHandle
                      , unFeedStatus
                      , unFeedSettings
                      , unFeedItem
                      , updateFeed
                      , getFeedStatus
                      , setFeedSettings
                      , getFeedStatusUrl
                      , getFeedStatusTitle
                      , getFeedStatusDescription
                      , getFeedStatusLastUpdate
                      , getFeedStatusNextUpdate
                      , getFeedStatusUpdating
                      , getFeedStatusItems
                      , getFeedStatusError
                      , getFeedStatusTtl
                      , getFeedSettingsUrl
                      , setFeedSettingsUrl
                      , getFeedSettingsAutoDownload
                      , setFeedSettingsAutoDownload
                      , getFeedSettingsAutoMapHandles
                      , setFeedSettingsAutoMapHandles
                      , getFeedSettingsDefaultTtl
                      , setFeedSettingsDefaultTtl
                      , getFeedSettingsAddArgs
                      , setFeedSettingsAddArgs
                      , getFeedItemUrl
                      , getFeedItemUuid
                      , getFeedItemTitle
                      , getFeedItemDescription
                      , getFeedItemComment
                      , getFeedItemCategory
                      , getFeedItemSize
                      , getFeedItemHandle
                      , getFeedItemInfoHash
                      ) where

import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.ErrorCode
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Rss.FeedHandle
import           Libtorrent.Rss.FeedItem
import           Libtorrent.Session.AddTorrentParams (AddTorrentParams)
import           Libtorrent.Sha1Hash (Sha1Hash)
import           Libtorrent.String
import           Libtorrent.TH (defineStdVector)
import           Libtorrent.TorrentHandle (TorrentHandle)
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/rss.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"


C.verbatim "typedef std::vector<libtorrent::feed_handle> VectorFeedHandle;"
C.verbatim "typedef std::vector<libtorrent::feed_item> VectorFeedItem;"

$(defineStdVector "feed_item" "VectorFeedItem" ''C'FeedItem ''C'VectorFeedItem ''FeedItem)

$(defineStdVector "feed_handle" "VectorFeedHandle" ''C'FeedHandle ''C'VectorFeedHandle ''FeedHandle)

newtype FeedStatus = FeedStatus { unFeedStatus :: ForeignPtr (CType FeedStatus)}

instance Show FeedStatus where
  show _ = "FeedStatus"

instance Inlinable FeedStatus where
  type (CType FeedStatus) = C'FeedStatus

instance FromPtr FeedStatus where
  fromPtr = objFromPtr FeedStatus $ \ptr ->
    [CU.exp| void { delete $(feed_status * ptr) } |]

instance WithPtr FeedStatus where
  withPtr (FeedStatus fptr) = withForeignPtr fptr

newtype FeedSettings = FeedSettings { unFeedSettings :: ForeignPtr (CType FeedSettings)}

instance Show FeedSettings where
  show _ = "FeedSettings"

instance Inlinable FeedSettings where
  type (CType FeedSettings) = C'FeedSettings

instance FromPtr FeedSettings where
  fromPtr = objFromPtr FeedSettings $ \ptr ->
    [CU.exp| void { delete $(feed_settings * ptr) } |]

instance WithPtr FeedSettings where
  withPtr (FeedSettings fptr) = withForeignPtr fptr

updateFeed :: FeedHandle -> IO ()
updateFeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(feed_handle * hoPtr)->update_feed() } |]

getFeedStatus :: FeedHandle -> IO FeedStatus
getFeedStatus ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| feed_status * { new feed_status($(feed_handle * hoPtr)->get_feed_status()) } |]

setFeedSettings :: FeedHandle -> FeedSettings -> IO ()
setFeedSettings ho fs =
  withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  [CU.exp| void { $(feed_handle * hoPtr)->set_settings(*$(feed_settings * fsPtr)) } |]

-- Feed status
getFeedStatusUrl :: FeedStatus -> IO Text
getFeedStatusUrl ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->url) } |]
  stdStringToText res

getFeedStatusTitle :: FeedStatus -> IO Text
getFeedStatusTitle ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->title) } |]
  stdStringToText res

getFeedStatusDescription :: FeedStatus -> IO Text
getFeedStatusDescription ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->description) } |]
  stdStringToText res

getFeedStatusLastUpdate :: FeedStatus -> IO C.CTime
getFeedStatusLastUpdate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(feed_status * hoPtr)->last_update } |]

getFeedStatusNextUpdate :: FeedStatus -> IO CInt
getFeedStatusNextUpdate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_status * hoPtr)->next_update } |]

getFeedStatusUpdating :: FeedStatus -> IO Bool
getFeedStatusUpdating ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_status * hoPtr)->updating } |]

getFeedStatusItems :: FeedStatus -> IO (StdVector FeedItem)
getFeedStatusItems fs =
  withPtr fs $ \fsPtr -> do
  fromPtr [CU.exp| VectorFeedItem * { new VectorFeedItem($(feed_status * fsPtr)->items) } |]

getFeedStatusError :: FeedStatus -> IO ErrorCode
getFeedStatusError ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(feed_status * hoPtr)->error) } |]

getFeedStatusTtl :: FeedStatus -> IO CInt
getFeedStatusTtl ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_status * hoPtr)->ttl } |]

-- Feed settings

getFeedSettingsUrl :: FeedSettings -> IO Text
getFeedSettingsUrl ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_settings * hoPtr)->url) } |]
  stdStringToText res

setFeedSettingsUrl :: FeedSettings -> Text -> IO ()
setFeedSettingsUrl ho val = do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(feed_settings * hoPtr)->url = std::string($(const char * cstr), $(size_t clen))} |]

getFeedSettingsAutoDownload :: FeedSettings -> IO Bool
getFeedSettingsAutoDownload ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_settings * hoPtr)->auto_download } |]

setFeedSettingsAutoDownload :: FeedSettings -> Bool -> IO ()
setFeedSettingsAutoDownload ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(feed_settings * hoPtr)->auto_download = $(bool val')} |]

getFeedSettingsAutoMapHandles :: FeedSettings -> IO Bool
getFeedSettingsAutoMapHandles ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_settings * hoPtr)->auto_map_handles } |]

setFeedSettingsAutoMapHandles :: FeedSettings -> Bool -> IO ()
setFeedSettingsAutoMapHandles ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(feed_settings * hoPtr)->auto_map_handles = $(bool val')} |]

getFeedSettingsDefaultTtl :: FeedSettings -> IO CInt
getFeedSettingsDefaultTtl ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_settings * hoPtr)->default_ttl } |]

setFeedSettingsDefaultTtl :: FeedSettings -> CInt -> IO ()
setFeedSettingsDefaultTtl ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(feed_settings * hoPtr)->default_ttl = $(int val)} |]

getFeedSettingsAddArgs :: FeedSettings -> IO AddTorrentParams
getFeedSettingsAddArgs ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| add_torrent_params * { new add_torrent_params($(feed_settings * hoPtr)->add_args) } |]

setFeedSettingsAddArgs :: FeedSettings -> AddTorrentParams -> IO ()
setFeedSettingsAddArgs ho val =
  withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(feed_settings * hoPtr)->add_args = *$(add_torrent_params * valPtr)} |]

-- Feed item
getFeedItemUrl :: FeedItem -> IO Text
getFeedItemUrl ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->url) } |]
  stdStringToText res

getFeedItemUuid :: FeedItem -> IO Text
getFeedItemUuid ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->uuid) } |]
  stdStringToText res

getFeedItemTitle :: FeedItem -> IO Text
getFeedItemTitle ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->title) } |]
  stdStringToText res

getFeedItemDescription :: FeedItem -> IO Text
getFeedItemDescription ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->description) } |]
  stdStringToText res

getFeedItemComment :: FeedItem -> IO Text
getFeedItemComment ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->comment) } |]
  stdStringToText res

getFeedItemCategory :: FeedItem -> IO Text
getFeedItemCategory ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->category) } |]
  stdStringToText res

getFeedItemSize :: FeedItem -> IO C.CSize
getFeedItemSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(feed_item * hoPtr)->size } |]

getFeedItemHandle :: FeedItem -> IO TorrentHandle
getFeedItemHandle ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| torrent_handle * { new torrent_handle($(feed_item * hoPtr)->handle) } |]

getFeedItemInfoHash :: FeedItem -> IO Sha1Hash
getFeedItemInfoHash ho =
  withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(feed_item * hoPtr)->info_hash) } |]
