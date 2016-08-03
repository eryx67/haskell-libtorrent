{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- | The 'AddTorrentParams' is a parameter pack for adding torrents to
-- a 'Network.Libtorrent.Session'.
-- See <http://www.libtorrent.org/reference-Core.html#add-torrent-params add_torrent_params>.

module Network.Libtorrent.Session.AddTorrentParams (AddTorrentParams
                                           , TorrentSrc(..)
                                           , AddTorrentFlags(..)
                                           , StorageMode(..)
                                           , unAddTorrentParams
                                           , newAddTorrentParams
                                           , setFlags
                                           , getFlags
                                           , setTorrentName
                                           , getTorrentName
                                           , setTorrentSavePath
                                           , getTorrentSavePath
                                           , getTrackers
                                           , setTrackers
                                           , getUrlSeeds
                                           , setUrlSeeds
                                           , getResumeData
                                           , setResumeData
                                           , getStorageMode
                                           , setStorageMode
                                           , getFilePriorities
                                           , setFilePriorities
                                           , getTrackerid
                                           , setTrackerid
                                           , getUrl
                                           , setUrl
                                           , getUuid
                                           , setUuid
                                           , getSourceFeedUrl
                                           , setSourceFeedUrl
                                           , getInfoHash
                                           , setInfoHash
                                           , getMaxUploads
                                           , setMaxUploads
                                           , getMaxConnections
                                           , setMaxConnections
                                           , getUploadLimit
                                           , setUploadLimit
                                           , getDownloadLimit
                                           , setDownloadLimit
                                           ) where

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import qualified Data.Text.Foreign              as TF
import           Data.Vector.Storable           (Vector, unsafeFromForeignPtr0)
import           Data.Word                      (Word8)
import           Foreign.Concurrent             (newForeignPtr)
import           Foreign.ForeignPtr             (ForeignPtr, withForeignPtr)
import           GHC.Generics                   (Generic)
import qualified Language.C.Inline              as C
import qualified Language.C.Inline.Cpp          as C
import qualified Language.C.Inline.Unsafe       as CU


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Sha1Hash
import           Network.Libtorrent.String
import           Network.Libtorrent.TorrentInfo
import           Network.Libtorrent.Types
import           Network.Libtorrent.Vectors     ()

C.context libtorrentCtx

C.include "<libtorrent/session.hpp>"
C.include "<libtorrent/add_torrent_params.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

C.verbatim "typedef std::vector<std::string> VectorString;"
C.verbatim "typedef std::vector<uint8_t> VectorUint8;"
C.verbatim "typedef std::vector<char> VectorChar;"

-- | Torrent source for 'AddTorrentParams'
data TorrentSrc =
  TorrentInfoSrc !TorrentInfo
  | UrlSrc !Text
  | InfoHashSrc !Sha1Hash
                deriving Show

-- | Flags for <http://www.libtorrent.org/reference-Core.html#add-torrent-params add_torrent_params>
data AddTorrentFlags =
  SeedMode
  | OverrideResumeData
  | UploadMode
  | ShareMode
  | ApplyIpFilter
  | Paused
  | AutoManaged
  | DuplicateIsError
  | MergeResumeTrackers
  | UpdateSubscribe
  | SuperSeeding
  | SequentialDownload
  | UseResumeSavePath
  | Pinned
  | MergeResumeHttpSeeds
  | StopWhenReady
  deriving (Show, Enum, Bounded, Eq, Ord, Generic)

-- | Flags for <http://www.libtorrent.org/reference-Storage.html#storage_mode_t storage_mode>
data StorageMode =
  StorageModeAllocate
  | StorageModeSparse
  deriving (Show, Enum, Bounded, Eq, Generic)

newtype AddTorrentParams = AddTorrentParams { unAddTorrentParams :: ForeignPtr (CType AddTorrentParams)}

instance Show AddTorrentParams where
  show _ = "AddTorrentParams"

instance Inlinable AddTorrentParams where
  type (CType AddTorrentParams) = C'AddTorrentParams

instance FromPtr AddTorrentParams where
  fromPtr = objFromPtr AddTorrentParams $ \ptr ->
    [CU.exp| void { delete $(add_torrent_params * ptr); } |]

instance WithPtr AddTorrentParams where
  withPtr (AddTorrentParams fptr) = withForeignPtr fptr

-- | Create 'AddTorrentParams' with default parameters.
newAddTorrentParams :: MonadIO m =>  TorrentSrc -> m AddTorrentParams
newAddTorrentParams ts = liftIO $ do
  atp <- fromPtr [CU.exp| add_torrent_params * { new add_torrent_params() } |]
  withPtr atp $ addTorrent ts
  return atp
  where
    addTorrent (TorrentInfoSrc ti) atPtr =
      withPtr ti $ \tiPtr ->
      [C.block| void {
          boost::intrusive_ptr<torrent_info> tip(new torrent_info(*$(torrent_info * tiPtr)));
          $(add_torrent_params * atPtr)->ti = tip;
        }
      |]
    addTorrent (UrlSrc url) atPtr =
      TF.withCStringLen url $ \(ptr, len) -> do
        let csize = fromIntegral len
        [CU.block| void {
            std::string url($(const char * ptr), $(size_t csize));
            $(add_torrent_params * atPtr)->url = url;
          }
        |]
    addTorrent (InfoHashSrc ih) atPtr =
      withPtr ih $ \ihPtr ->
      [CU.exp| void { $(add_torrent_params * atPtr)->info_hash = sha1_hash(*$(sha1_hash * ihPtr)) } |]

setFlags :: MonadIO m =>  AddTorrentParams -> BitFlags AddTorrentFlags -> m ()
setFlags atp flags =
  liftIO . withPtr atp $ \atPtr -> do
  let flags' = fromIntegral $ fromEnum flags
  [CU.exp| void { $(add_torrent_params * atPtr)->flags = $(uint64_t flags')} |]

getFlags :: MonadIO m =>  AddTorrentParams -> m (BitFlags AddTorrentFlags)
getFlags ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(add_torrent_params * hoPtr)->flags } |]


setTorrentName :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setTorrentName atp nm =
  liftIO . withPtr atp $ \atPtr -> do
  TF.withCStringLen nm $ \(ptr, len) -> do
    let csize = fromIntegral len
    [CU.block| void {
        std::string val($(const char * ptr), $(size_t csize));
        $(add_torrent_params * atPtr)->name = val;
      }
    |]

getTorrentName :: MonadIO m =>  AddTorrentParams -> m Text
getTorrentName ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->name) } |]
  stdStringToText res

setTorrentSavePath :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setTorrentSavePath atp path =
  liftIO . withPtr atp $ \atPtr -> do
  TF.withCStringLen path $ \(ptr, len) -> do
    let csize = fromIntegral len
    [CU.block| void {
        std::string val($(const char * ptr), $(size_t csize));
        $(add_torrent_params * atPtr)->save_path = val;
      }
    |]

getTorrentSavePath :: MonadIO m =>  AddTorrentParams -> m Text
getTorrentSavePath ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->save_path) } |]
  stdStringToText res

getTrackers :: MonadIO m =>  AddTorrentParams -> m (StdVector StdString)
getTrackers ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorString * { new VectorString($(add_torrent_params * hoPtr)->trackers) } |]

setTrackers :: MonadIO m =>  AddTorrentParams -> (StdVector StdString) -> m ()
setTrackers ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->trackers = *$(VectorString * valPtr)} |]

getUrlSeeds :: MonadIO m =>  AddTorrentParams -> m (StdVector StdString)
getUrlSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorString * { new VectorString($(add_torrent_params * hoPtr)->url_seeds) } |]

setUrlSeeds :: MonadIO m =>  AddTorrentParams -> (StdVector StdString) -> m ()
setUrlSeeds ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->url_seeds = *$(VectorString * valPtr)} |]

getResumeData :: MonadIO m =>  AddTorrentParams -> m ByteString
getResumeData ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (clen, cstr) <- C.withPtrs_ $ \(clenPtr, cstrPtr) -> do
    [CU.block| void {
        std::vector<char> rd = $(add_torrent_params * hoPtr)->resume_data;
        *$(size_t * clenPtr) = rd.size();
        *$(char ** cstrPtr) = rd.data();
       }
    |]
  BS.packCStringLen (cstr, fromIntegral clen)

setResumeData :: MonadIO m =>  AddTorrentParams -> ByteString -> m ()
setResumeData ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void {
      $(add_torrent_params * hoPtr)->resume_data = std::vector<char>($bs-ptr:val, $bs-ptr:val + $bs-len:val)} |]

getStorageMode :: MonadIO m =>  AddTorrentParams -> m StorageMode
getStorageMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(add_torrent_params * hoPtr)->storage_mode } |]

setStorageMode :: MonadIO m =>  AddTorrentParams -> StorageMode -> m ()
setStorageMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(add_torrent_params * hoPtr)->storage_mode = (storage_mode_t)$(int val')} |]

-- TODO:
-- std::vector<std::pair<std::string, int> > dht_nodes;
-- storage_constructor_type storage;
-- void* userdata;

getFilePriorities :: MonadIO m =>  AddTorrentParams -> m (Vector Word8)
getFilePriorities ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
   [CU.block| void {
       std::vector<uint8_t> v = $(add_torrent_params * hoPtr)->file_priorities;
       *$(uint8_t ** dataPtrPtr) = new uint8_t[v.size()];
       std::copy(v.data(), v.data() + v.size(), *$(uint8_t ** dataPtrPtr));
       *$(int * dataSizePtr) = v.size();
      }
   |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(uint8_t * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

setFilePriorities :: MonadIO m =>  AddTorrentParams -> (Vector Word8) -> m ()
setFilePriorities ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.block| void {
      std::vector<uint8_t> v($vec-ptr:(uint8_t * val), $vec-ptr:(uint8_t * val) + $vec-len:val);
      $(add_torrent_params * hoPtr)->file_priorities = v;
     }
  |]

getTrackerid :: MonadIO m =>  AddTorrentParams -> m Text
getTrackerid ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->trackerid) } |]
  stdStringToText res

setTrackerid :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setTrackerid ho val = liftIO $ do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(add_torrent_params * hoPtr)->trackerid = std::string($(const char * cstr), $(size_t clen))} |]

getUrl :: MonadIO m =>  AddTorrentParams -> m Text
getUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->url) } |]
  stdStringToText res

setUrl :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setUrl ho val = liftIO$ do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(add_torrent_params * hoPtr)->url = std::string($(const char * cstr), $(size_t clen))} |]

getUuid :: MonadIO m =>  AddTorrentParams -> m Text
getUuid ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->uuid) } |]
  stdStringToText res

setUuid :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setUuid ho val = liftIO $ do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(add_torrent_params * hoPtr)->uuid = std::string($(const char * cstr), $(size_t clen))} |]

getSourceFeedUrl :: MonadIO m =>  AddTorrentParams -> m Text
getSourceFeedUrl ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(add_torrent_params * hoPtr)->source_feed_url) } |]
  stdStringToText res

setSourceFeedUrl :: MonadIO m =>  AddTorrentParams -> Text -> m ()
setSourceFeedUrl ho val = liftIO $ do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(add_torrent_params * hoPtr)->source_feed_url = std::string($(const char * cstr), $(size_t clen))} |]

getInfoHash :: MonadIO m =>  AddTorrentParams -> m Sha1Hash
getInfoHash ho =
  liftIO . withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(add_torrent_params * hoPtr)->info_hash) } |]

setInfoHash :: MonadIO m =>  AddTorrentParams -> Sha1Hash -> m ()
setInfoHash ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->info_hash = *$(sha1_hash * valPtr) } |]

getMaxUploads :: MonadIO m =>  AddTorrentParams -> m C.CInt
getMaxUploads ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(add_torrent_params * hoPtr)->max_uploads } |]

setMaxUploads :: MonadIO m =>  AddTorrentParams -> C.CInt -> m ()
setMaxUploads ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->max_uploads = $(int val)} |]

getMaxConnections :: MonadIO m =>  AddTorrentParams -> m C.CInt
getMaxConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(add_torrent_params * hoPtr)->max_connections } |]

setMaxConnections :: MonadIO m =>  AddTorrentParams -> C.CInt -> m ()
setMaxConnections ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->max_connections = $(int val)} |]

getUploadLimit :: MonadIO m =>  AddTorrentParams -> m C.CInt
getUploadLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(add_torrent_params * hoPtr)->upload_limit } |]

setUploadLimit :: MonadIO m =>  AddTorrentParams -> C.CInt -> m ()
setUploadLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->upload_limit = $(int val)} |]

getDownloadLimit :: MonadIO m =>  AddTorrentParams -> m C.CInt
getDownloadLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(add_torrent_params * hoPtr)->download_limit } |]

setDownloadLimit :: MonadIO m =>  AddTorrentParams -> C.CInt -> m ()
setDownloadLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(add_torrent_params * hoPtr)->download_limit = $(int val)} |]
