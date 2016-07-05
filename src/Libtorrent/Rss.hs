{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-RSS.html#feed_handle feed_handle> structure for "Libtorrent"

module Libtorrent.Rss (FeedHandle(..)
                      , FeedStatus(..)
                      , FeedSettings(..)
                      , FeedItem(..)
                      , newFeedSettings
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

import           Control.Monad.IO.Class (MonadIO, liftIO)
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

newFeedSettings :: MonadIO m =>  m FeedSettings
newFeedSettings =
  liftIO $ fromPtr [CU.exp| feed_settings * { new feed_settings() } |]

updateFeed :: MonadIO m =>  FeedHandle -> m ()
updateFeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(feed_handle * hoPtr)->update_feed() } |]

getFeedStatus :: MonadIO m =>  FeedHandle -> m FeedStatus
getFeedStatus ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| feed_status * { new feed_status($(feed_handle * hoPtr)->get_feed_status()) } |]

setFeedSettings :: MonadIO m =>  FeedHandle -> FeedSettings -> m ()
setFeedSettings ho fs =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  [C.exp| void { $(feed_handle * hoPtr)->set_settings(*$(feed_settings * fsPtr)) } |]

-- Feed status
getFeedStatusUrl :: MonadIO m =>  FeedStatus -> m Text
getFeedStatusUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->url) } |]
  stdStringToText res

getFeedStatusTitle :: MonadIO m =>  FeedStatus -> m Text
getFeedStatusTitle ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->title) } |]
  stdStringToText res

getFeedStatusDescription :: MonadIO m =>  FeedStatus -> m Text
getFeedStatusDescription ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_status * hoPtr)->description) } |]
  stdStringToText res

getFeedStatusLastUpdate :: MonadIO m =>  FeedStatus -> m C.CTime
getFeedStatusLastUpdate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(feed_status * hoPtr)->last_update } |]

getFeedStatusNextUpdate :: MonadIO m =>  FeedStatus -> m CInt
getFeedStatusNextUpdate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_status * hoPtr)->next_update } |]

getFeedStatusUpdating :: MonadIO m =>  FeedStatus -> m Bool
getFeedStatusUpdating ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_status * hoPtr)->updating } |]

getFeedStatusItems :: MonadIO m =>  FeedStatus -> m (StdVector FeedItem)
getFeedStatusItems fs =
  liftIO . withPtr fs $ \fsPtr -> do
  fromPtr [C.exp| VectorFeedItem * { new VectorFeedItem($(feed_status * fsPtr)->items) } |]

getFeedStatusError :: MonadIO m =>  FeedStatus -> m ErrorCode
getFeedStatusError ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(feed_status * hoPtr)->error) } |]

getFeedStatusTtl :: MonadIO m =>  FeedStatus -> m CInt
getFeedStatusTtl ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_status * hoPtr)->ttl } |]

-- Feed settings

getFeedSettingsUrl :: MonadIO m =>  FeedSettings -> m Text
getFeedSettingsUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_settings * hoPtr)->url) } |]
  stdStringToText res

setFeedSettingsUrl :: MonadIO m =>  FeedSettings -> Text -> m ()
setFeedSettingsUrl ho val = liftIO $ do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(feed_settings * hoPtr)->url = std::string($(const char * cstr), $(size_t clen))} |]

getFeedSettingsAutoDownload :: MonadIO m =>  FeedSettings -> m Bool
getFeedSettingsAutoDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_settings * hoPtr)->auto_download } |]

setFeedSettingsAutoDownload :: MonadIO m =>  FeedSettings -> Bool -> m ()
setFeedSettingsAutoDownload ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(feed_settings * hoPtr)->auto_download = $(bool val')} |]

getFeedSettingsAutoMapHandles :: MonadIO m =>  FeedSettings -> m Bool
getFeedSettingsAutoMapHandles ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(feed_settings * hoPtr)->auto_map_handles } |]

setFeedSettingsAutoMapHandles :: MonadIO m =>  FeedSettings -> Bool -> m ()
setFeedSettingsAutoMapHandles ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(feed_settings * hoPtr)->auto_map_handles = $(bool val')} |]

getFeedSettingsDefaultTtl :: MonadIO m =>  FeedSettings -> m CInt
getFeedSettingsDefaultTtl ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(feed_settings * hoPtr)->default_ttl } |]

setFeedSettingsDefaultTtl :: MonadIO m =>  FeedSettings -> CInt -> m ()
setFeedSettingsDefaultTtl ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(feed_settings * hoPtr)->default_ttl = $(int val)} |]

getFeedSettingsAddArgs :: MonadIO m =>  FeedSettings -> m AddTorrentParams
getFeedSettingsAddArgs ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| add_torrent_params * { new add_torrent_params($(feed_settings * hoPtr)->add_args) } |]

setFeedSettingsAddArgs :: MonadIO m =>  FeedSettings -> AddTorrentParams -> m ()
setFeedSettingsAddArgs ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(feed_settings * hoPtr)->add_args = *$(add_torrent_params * valPtr)} |]

-- Feed item
getFeedItemUrl :: MonadIO m =>  FeedItem -> m Text
getFeedItemUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->url) } |]
  stdStringToText res

getFeedItemUuid :: MonadIO m =>  FeedItem -> m Text
getFeedItemUuid ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->uuid) } |]
  stdStringToText res

getFeedItemTitle :: MonadIO m =>  FeedItem -> m Text
getFeedItemTitle ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->title) } |]
  stdStringToText res

getFeedItemDescription :: MonadIO m =>  FeedItem -> m Text
getFeedItemDescription ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->description) } |]
  stdStringToText res

getFeedItemComment :: MonadIO m =>  FeedItem -> m Text
getFeedItemComment ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->comment) } |]
  stdStringToText res

getFeedItemCategory :: MonadIO m =>  FeedItem -> m Text
getFeedItemCategory ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(feed_item * hoPtr)->category) } |]
  stdStringToText res

getFeedItemSize :: MonadIO m =>  FeedItem -> m C.CSize
getFeedItemSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(feed_item * hoPtr)->size } |]

getFeedItemHandle :: MonadIO m =>  FeedItem -> m TorrentHandle
getFeedItemHandle ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| torrent_handle * { new torrent_handle($(feed_item * hoPtr)->handle) } |]

getFeedItemInfoHash :: MonadIO m =>  FeedItem -> m Sha1Hash
getFeedItemInfoHash ho =
  liftIO . withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(feed_item * hoPtr)->info_hash) } |]
