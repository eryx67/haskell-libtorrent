{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent-handle torrent_handle> structure for "Libtorrent"
-- All functions in this module can throw 'LibtorrentException'.


module Network.Libtorrent.TorrentHandle ( TorrentHandle(..)
                                , StatusFlags(..)
                                , DeadlineFlags(..)
                                , FileProgressFlags(..)
                                , PauseFlags(..)
                                , SaveResumeFlags(..)
                                , newTorrentHandle
                                , addPiece
                                , readPiece
                                , havePiece
                                , getPeerInfo
                                , torrentStatus
                                , getDownloadQueue
                                , resetPieceDeadline
                                , clearPieceDeadlines
                                , setPieceDeadline
                                , setPriority
                                , fileProgress
                                , clearError
                                , trackers
                                , replaceTrackers
                                , addTracker
                                , addUrlSeed
                                , removeUrlSeed
                                , addHttpSeed
                                , removeHttpSeed
                                , setMetadata
                                , isValid
                                , pause
                                , resume
                                , setUploadMode
                                , setShareMode
                                , flushCache
                                , applyIpFilter
                                , forceRecheck
                                , saveResumeData
                                , needSaveResumeData
                                , autoManaged
                                , queuePositionDown
                                , queuePositionTop
                                , queuePosition
                                , queuePositionBottom
                                , queuePositionUp
                                , setSslCertificate
                                , useInterface
                                , pieceAvailability
                                , piecePriority
                                , piecePriorities
                                , setPiecePriority
                                , prioritizePieces
                                , filePriority
                                , filePriorities
                                , setFilePriority
                                , prioritizeFiles
                                , forceReannounce
                                , forceDhtAnnounce
                                , scrapeTracker
                                , torrentHandleUploadLimit
                                , torrentHandleDownloadLimit
                                , setTorrentHandleUploadLimit
                                , setTorrentHandleDownloadLimit
                                , setSequentialDownload
                                , torrentHandleMaxUploads
                                , setTorrentHandleMaxUploads
                                , torrentHandleMaxConnections
                                , setTorrentHandleMaxConnections
                                , moveStorage
                                , renameFile
                                , superSeeding
                                , infoHash
                                , torrentFile
                                ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector.Storable (unsafeFromForeignPtr0, Vector)
import           Foreign.C.String (withCAString)
import           Foreign.C.Types (CInt)
import           Foreign.Concurrent ( newForeignPtr )
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Utils (toBool, fromBool)
import           Foreign.Ptr ( Ptr )
import           Foreign.Storable (peek)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Exceptions
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.PeerInfo
import           Network.Libtorrent.Sha1Hash (Sha1Hash)
import           Network.Libtorrent.String
import           Network.Libtorrent.TorrentHandle.PartialPieceInfo
import           Network.Libtorrent.TorrentHandle.TorrentStatus
import           Network.Libtorrent.TorrentInfo.AnnounceEntry
import           Network.Libtorrent.TorrentInfo (TorrentInfo)
import           Network.Libtorrent.Types
import           Network.Libtorrent.TH (defineStdVector)

C.context libtorrentCtx

C.include "<libtorrent/torrent_handle.hpp>"
C.include "<libtorrent/peer_info.hpp>"

C.include "torrent_handle.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

C.verbatim "typedef std::vector<libtorrent::peer_info> VectorPeerInfo;"
C.verbatim "typedef std::vector<libtorrent::partial_piece_info> VectorPartialPieceInfo;"
C.verbatim "typedef std::vector<libtorrent::announce_entry> VectorAnnounceEntry;"

$(defineStdVector "peer_info" "VectorPeerInfo" ''C'PeerInfo ''C'VectorPeerInfo ''PeerInfo)
$(defineStdVector "partial_piece_info" "VectorPartialPieceInfo" ''C'PartialPieceInfo ''C'VectorPartialPieceInfo ''PartialPieceInfo)

data Flags =
      OverwriteExisting
  deriving (Show, Enum, Bounded, Eq)

data StatusFlags =
  QueryDistributedCopies
  | QueryAccurateDownloadCounters
  | QueryLastSeenComplete
  | QueryPieces
  | QueryVerifiedPieces
  | QueryTorrentFile
  | QueryName
  | QuerySavePath
  deriving (Show, Enum, Bounded, Eq)

data DeadlineFlags =
  AlertWhenAvailable
  deriving (Show, Enum, Bounded, Eq)

data FileProgressFlags =
  PieceGranularity
  deriving (Show, Enum, Bounded, Eq)

data PauseFlags =
  GracefulPause
  deriving (Show, Enum, Bounded, Eq)

data SaveResumeFlags =
  FlushDiskCache
  | SaveInfoDict
  | OnlyIfModified
  deriving (Show, Enum, Bounded, Eq)

newtype TorrentHandle = TorrentHandle { unTorrentHandle :: ForeignPtr (CType TorrentHandle)}

instance Show TorrentHandle where
  show _ = "TorrentHandle"

instance Inlinable TorrentHandle where
  type (CType TorrentHandle) = C'TorrentHandle

instance FromPtr TorrentHandle where
  fromPtr = objFromPtr TorrentHandle $ \ptr ->
    [CU.exp| void { delete $(torrent_handle * ptr) } |]

instance WithPtr TorrentHandle where
  withPtr (TorrentHandle fptr) = withForeignPtr fptr


-- | Create new torrent_handle.
newTorrentHandle :: MonadIO m =>  m TorrentHandle
newTorrentHandle =
  liftIO $ fromPtr [C.exp| torrent_handle * { new torrent_handle() } |]

addPiece :: MonadIO m =>  TorrentHandle -> CInt -> ByteString -> m ()
addPiece th pieceNum pieceData =
  liftIO . withPtr th $ \thPtr ->
  checkError [exceptU| { $(torrent_handle * thPtr)->add_piece($(int pieceNum), $bs-ptr:pieceData); } |]

readPiece :: MonadIO m =>  TorrentHandle -> CInt -> m ()
readPiece th pieceNum =
  liftIO . withPtr th $ \thPtr ->
  checkError [exceptU| { $(torrent_handle * thPtr)->read_piece($(int pieceNum)); } |]

havePiece :: MonadIO m =>  TorrentHandle -> CInt -> m Bool
havePiece th pieceNum =
  liftIO . withPtr th $ \thPtr ->
  fmap toBool . C.withPtr_ $ \vPtr -> do
    checkError [except| { *$(bool * vPtr) = $(torrent_handle * thPtr)->have_piece($(int pieceNum)); } |]

getPeerInfo :: MonadIO m =>  TorrentHandle -> m (StdVector PeerInfo)
getPeerInfo th =
  liftIO . withPtr th $ \thPtr -> do
  fromPtr . C.withPtr_ $ \resPtr ->
    checkError [except| {
                       *$(VectorPeerInfo ** resPtr) = new VectorPeerInfo();
                       $(torrent_handle * thPtr)->get_peer_info(**$(VectorPeerInfo ** resPtr));
                       }
                      |]

torrentStatus :: MonadIO m =>  TorrentHandle -> Maybe (BitFlags StatusFlags) -> m TorrentStatus
torrentStatus ho flags =
  liftIO . withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0xffffffff (fromIntegral . fromEnum <$> flags)
    fromPtr . C.withPtr_ $ \tsPtr ->
      checkError [except| {
                         *$(torrent_status ** tsPtr) =
                         new torrent_status($(torrent_handle * hoPtr)->status($(uint32_t flags')));
                         }
                        |]

getDownloadQueue :: MonadIO m =>  TorrentHandle -> m (StdVector PartialPieceInfo)
getDownloadQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr . C.withPtr_ $ \ppiPtr ->
  checkError [except| {
                     *$(VectorPartialPieceInfo ** ppiPtr) = new VectorPartialPieceInfo();
                     $(torrent_handle * hoPtr)->get_download_queue(**$(VectorPartialPieceInfo ** ppiPtr));
                     }
                    |]

resetPieceDeadline :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
resetPieceDeadline ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->reset_piece_deadline($(int idx)); } |]

clearPieceDeadlines :: MonadIO m =>  TorrentHandle -> m ()
clearPieceDeadlines ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->clear_piece_deadlines(); } |]

setPieceDeadline :: MonadIO m =>  TorrentHandle -> C.CInt -> C.CInt -> Maybe (BitFlags DeadlineFlags) -> m ()
setPieceDeadline ho idx deadline flags =
  liftIO . withPtr ho $ \hoPtr -> do
  let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
  checkError [except|
                     { $(torrent_handle * hoPtr)->set_piece_deadline($(int idx), $(int deadline), $(int flags')); }
                     |]

setPriority :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
setPriority ho prio =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->set_priority($(int prio)); } |]

fileProgress :: MonadIO m =>  TorrentHandle -> Maybe (BitFlags FileProgressFlags) -> m (Vector Int64)
fileProgress ho flags =
  liftIO . withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [except| {
                       std::vector<boost::int64_t> v;
                       $(torrent_handle * hoPtr)->file_progress(v, $(int flags'));
                       *$(int64_t ** dataPtrPtr) = new int64_t[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int64_t ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int64_t * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

-- TODO: defined in libtorrent 1.1.0
-- file_status (std::vector<pool_file_status>& status) const;

clearError :: MonadIO m =>  TorrentHandle -> m ()
clearError ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->clear_error(); } |]

trackers :: MonadIO m =>  TorrentHandle -> m (StdVector AnnounceEntry)
trackers ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr . C.withPtr_ $ \aePtr ->
  checkError [except| {
                     *$(VectorAnnounceEntry ** aePtr) = new VectorAnnounceEntry($(torrent_handle * hoPtr)->trackers());
                     }
                     |]

replaceTrackers :: MonadIO m =>  TorrentHandle -> StdVector AnnounceEntry -> m ()
replaceTrackers ho aes =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr aes $ \aesPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->replace_trackers(*$(VectorAnnounceEntry * aesPtr)); } |]

addTracker :: MonadIO m =>  TorrentHandle -> AnnounceEntry -> m ()
addTracker ho ae =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ae $ \aePtr ->
  checkError [except| { $(torrent_handle * hoPtr)->add_tracker(*$(announce_entry * aePtr)); } |]

addUrlSeed :: MonadIO m =>  TorrentHandle -> Text -> m ()
addUrlSeed ho url = liftIO $ do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->add_url_seed(*$(string * sPtr)); } |]

removeUrlSeed :: MonadIO m =>  TorrentHandle -> Text ->m ()
removeUrlSeed ho url = liftIO $ do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->remove_url_seed(*$(string * sPtr)); } |]

--    std::set<std::string> url_seeds () const;

addHttpSeed :: MonadIO m =>  TorrentHandle -> Text -> m ()
addHttpSeed ho url = liftIO $ do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->add_http_seed(*$(string * sPtr)); } |]

removeHttpSeed :: MonadIO m =>  TorrentHandle -> Text ->m ()
removeHttpSeed ho url = liftIO $ do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->remove_http_seed(*$(string * sPtr)); } |]

--    std::set<std::string> http_seeds () const;

--    void add_extension (
--       boost::function<boost::shared_ptr<torrent_plugin>(torrent_handle const&, void*)> const& ext
--       , void* userdata = 0);

setMetadata :: MonadIO m =>  TorrentHandle -> ByteString -> m Bool
setMetadata ho md =
  liftIO . withPtr ho $ \hoPtr ->
  fmap toBool . C.withPtr_ $ \ptr ->
    checkError [except| { *$(bool * ptr) = $(torrent_handle * hoPtr)->set_metadata($bs-ptr:md, $bs-len:md); } |]

isValid :: MonadIO m =>  TorrentHandle -> m Bool
isValid ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_handle * hoPtr)->is_valid() } |]

pause :: MonadIO m =>  TorrentHandle -> Maybe (BitFlags PauseFlags) -> m ()
pause ho flags =
  liftIO . withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [except| { $(torrent_handle * hoPtr)->pause($(int flags')); } |]

resume :: MonadIO m =>  TorrentHandle -> m ()
resume ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->resume(); } |]

-- TODO in libtorrent 1.1.0
-- stopWhenReady :: MonadIO m =>  TorrentHandle -> Bool -> m ()
-- stopWhenReady ho v = liftIO $ do
--   let b = fromBool v
--   withPtr ho $ \hoPtr ->
--     checkError [except| void { $(torrent_handle * hoPtr)->stop_when_ready($(bool b)) } |]

setUploadMode :: MonadIO m =>  TorrentHandle -> Bool -> m ()
setUploadMode ho v = liftIO $ do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->set_upload_mode($(bool b)); } |]

setShareMode :: MonadIO m =>  TorrentHandle -> Bool -> m ()
setShareMode ho v = liftIO $ do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->set_share_mode($(bool b)); } |]

flushCache :: MonadIO m =>  TorrentHandle -> m ()
flushCache ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->flush_cache(); } |]

applyIpFilter :: MonadIO m =>  TorrentHandle -> Bool -> m ()
applyIpFilter ho v = liftIO $ do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->apply_ip_filter($(bool b)); } |]

forceRecheck :: MonadIO m =>  TorrentHandle -> m ()
forceRecheck ho =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->force_recheck(); } |]

saveResumeData :: MonadIO m =>  TorrentHandle -> Maybe (SaveResumeFlags) -> m ()
saveResumeData ho flags =
  liftIO . withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [except| { $(torrent_handle * hoPtr)->save_resume_data($(int flags')); } |]

needSaveResumeData :: MonadIO m =>  TorrentHandle -> m Bool
needSaveResumeData ho =
  liftIO . withPtr ho $ \hoPtr ->
  fmap toBool . C.withPtr_ $ \ptr ->
  checkError [except| { *$(bool * ptr) = $(torrent_handle * hoPtr)->need_save_resume_data(); } |]

autoManaged :: MonadIO m =>  TorrentHandle -> Bool -> m ()
autoManaged ho v = liftIO $ do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->auto_managed($(bool b)); } |]

queuePositionDown :: MonadIO m =>  TorrentHandle -> m ()
queuePositionDown ho =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->queue_position_down(); } |]

queuePositionTop :: MonadIO m =>  TorrentHandle -> m ()
queuePositionTop ho =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->queue_position_top(); } |]

queuePosition :: MonadIO m =>  TorrentHandle -> m CInt
queuePosition ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except|  { *$(int * ptr) = $(torrent_handle * hoPtr)->queue_position(); } |]

queuePositionBottom :: MonadIO m =>  TorrentHandle -> m ()
queuePositionBottom ho =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->queue_position_bottom(); } |]

queuePositionUp :: MonadIO m =>  TorrentHandle -> m ()
queuePositionUp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->queue_position_up(); } |]

setSslCertificate :: MonadIO m =>  TorrentHandle -> Text -> Text -> Text -> m ()
setSslCertificate ho private_key dh_params passphrase = liftIO $ do
  pks <- textToStdString private_key
  ds <- textToStdString dh_params
  pas <- textToStdString passphrase
  withPtr ho $ \hoPtr ->
    withPtr pks $ \pksPtr ->
    withPtr ds $ \dsPtr ->
    withPtr pas $ \pasPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->set_ssl_certificate(*$(string * pksPtr), *$(string * dsPtr), *$(string * pasPtr)); } |]

-- TODO:
--    void set_ssl_certificate_buffer (std::string const& certificate
--       , std::string const& private_key
--       , std::string const& dh_params);
--    storage_interface* get_storage_impl () const;

torrentFile :: MonadIO m => TorrentHandle -> m TorrentInfo
torrentFile ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| torrent_info * { new torrent_info(*$(torrent_handle * hoPtr)->torrent_file().get()) } |]

useInterface :: MonadIO m =>  TorrentHandle -> [Text] -> m ()
useInterface ho ifs = liftIO $ do
  withCAString (T.unpack $ T.intercalate "," ifs) $ \ifsPtr ->
    withPtr ho $ \hoPtr ->
    checkError [except| { $(torrent_handle * hoPtr)->use_interface($(char * ifsPtr)); } |]

pieceAvailability :: MonadIO m =>  TorrentHandle -> m (Vector C.CInt)
pieceAvailability ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [except| {
                       std::vector<int> v;
                       $(torrent_handle * hoPtr)->piece_availability(v);
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

piecePriority :: MonadIO m =>  TorrentHandle -> C.CInt -> m C.CInt
piecePriority ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->piece_priority($(int idx)); } |]

piecePriorities :: MonadIO m =>  TorrentHandle -> m (Vector C.CInt)
piecePriorities ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [except| {
                       std::vector<int> v = $(torrent_handle * hoPtr)->piece_priorities();
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

setPiecePriority :: MonadIO m =>  TorrentHandle -> C.CInt -> C.CInt -> m ()
setPiecePriority ho idx priority=
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->piece_priority($(int idx), $(int priority)); } |]


prioritizePieces :: MonadIO m =>  TorrentHandle -> Vector C.CInt -> m ()
prioritizePieces ho ps =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| {
      std::vector<int> v($vec-ptr:(int * ps), $vec-ptr:(int * ps) + $vec-len:ps);
      $(torrent_handle * hoPtr)->prioritize_pieces(v);
     }
  |]

--    void prioritize_pieces (std::vector<std::pair<int, int> > const& pieces) const;

filePriority :: MonadIO m =>  TorrentHandle -> C.CInt -> m C.CInt
filePriority ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->file_priority($(int idx)); } |]

filePriorities :: MonadIO m =>  TorrentHandle -> m (Vector C.CInt)
filePriorities ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [except| {
                       std::vector<int> v = $(torrent_handle * hoPtr)->file_priorities();
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

setFilePriority :: MonadIO m =>  TorrentHandle -> C.CInt -> C.CInt -> m ()
setFilePriority ho idx priority=
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->file_priority($(int idx), $(int priority)); } |]


prioritizeFiles :: MonadIO m =>  TorrentHandle -> Vector C.CInt -> m ()
prioritizeFiles ho ps =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| {
      std::vector<int> v($vec-ptr:(int * ps), $vec-ptr:(int * ps) + $vec-len:ps);
      $(torrent_handle * hoPtr)->prioritize_files(v);
     }
  |]

forceReannounce :: MonadIO m =>  TorrentHandle -> Maybe C.CInt -> Maybe C.CInt -> m ()
forceReannounce ho seconds tracker_index =
  liftIO . withPtr ho $ \hoPtr -> do
  let sec = fromMaybe 0 seconds
      tidx = fromMaybe (-1) tracker_index
  checkError [except| { $(torrent_handle * hoPtr)->force_reannounce($(int sec), $(int tidx)); } |]

forceDhtAnnounce :: MonadIO m =>  TorrentHandle -> m ()
forceDhtAnnounce ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->force_dht_announce(); } |]

scrapeTracker :: MonadIO m =>  TorrentHandle -> m ()
scrapeTracker ho =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->scrape_tracker(); } |]

torrentHandleUploadLimit :: MonadIO m =>  TorrentHandle -> m CInt
torrentHandleUploadLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->upload_limit(); } |]

torrentHandleDownloadLimit :: MonadIO m =>  TorrentHandle -> m CInt
torrentHandleDownloadLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->download_limit(); } |]

setTorrentHandleUploadLimit :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
setTorrentHandleUploadLimit ho limit =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->set_upload_limit($(int limit)); } |]

setTorrentHandleDownloadLimit :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
setTorrentHandleDownloadLimit ho limit =
  liftIO . withPtr ho $ \hoPtr ->
                 checkError [except| { $(torrent_handle * hoPtr)->set_download_limit($(int limit)); } |]


-- TODO in libtorrent 1.1.0
-- setPinned :: MonadIO m =>  TorrentHandle -> Bool -> m ()
-- setPinned ho sd =
--   liftIO . withPtr ho $ \hoPtr -> do
--     let sd' = fromBool sd
--     checkError [except| void { $(torrent_handle * hoPtr)->set_pinned($(bool sd')) } |]

setSequentialDownload :: MonadIO m =>  TorrentHandle -> Bool -> m ()
setSequentialDownload ho sd =
  liftIO . withPtr ho $ \hoPtr -> do
    let sd' = fromBool sd
    checkError [except| { $(torrent_handle * hoPtr)->set_sequential_download($(bool sd')); } |]

--  void connect_peer (tcp::endpoint const& adr, int source = 0, int flags = 0x1 + 0x4 + 0x8) const;

torrentHandleMaxUploads :: MonadIO m =>  TorrentHandle -> m C.CInt
torrentHandleMaxUploads ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->max_uploads(); } |]

setTorrentHandleMaxUploads :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
setTorrentHandleMaxUploads ho max_uploads =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->set_max_uploads($(int max_uploads)); } |]

torrentHandleMaxConnections :: MonadIO m =>  TorrentHandle -> m C.CInt
torrentHandleMaxConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [except| { *$(int * ptr) = $(torrent_handle * hoPtr)->max_connections(); } |]

setTorrentHandleMaxConnections :: MonadIO m =>  TorrentHandle -> C.CInt -> m ()
setTorrentHandleMaxConnections ho max_connections =
  liftIO . withPtr ho $ \hoPtr ->
  checkError [except| { $(torrent_handle * hoPtr)->set_max_connections($(int max_connections)); } |]

moveStorage :: MonadIO m =>  TorrentHandle -> Text -> C.CInt -> m ()
moveStorage ho save_path flags =
  liftIO . withPtr ho $ \hoPtr -> do
    sp <- textToStdString save_path
    withPtr sp $ \sPtr ->
      checkError [except| { $(torrent_handle * hoPtr)->move_storage(*$(string * sPtr), $(int flags)); } |]

renameFile :: MonadIO m =>  TorrentHandle -> C.CInt -> Text -> m ()
renameFile ho idx new_name =
  liftIO . withPtr ho $ \hoPtr -> do
    nns <- textToStdString new_name
    withPtr nns $ \nnPtr ->
      checkError [except| { $(torrent_handle * hoPtr)->rename_file($(int idx), *$(string * nnPtr)); } |]

superSeeding :: MonadIO m =>  TorrentHandle -> Bool -> m ()
superSeeding ho on =
  liftIO . withPtr ho $ \hoPtr -> do
  let on' = fromBool on
  checkError [except| { $(torrent_handle * hoPtr)->super_seeding($(bool on')); } |]


infoHash :: MonadIO m =>  TorrentHandle -> m Sha1Hash
infoHash ho =
  liftIO . withPtr ho $ \hoPtr ->
  alloca $ \shPtrPtr -> do
  checkError [except|
                     {*$(sha1_hash ** shPtrPtr) = new sha1_hash($(torrent_handle * hoPtr)->info_hash());}
                     |]
  fromPtr $ peek shPtrPtr

-- TODO in libtorrent 1.1.0
-- getId :: MonadIO m =>  TorrentHandle -> m Word32
-- getId ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   checkError [except| uint32_t { $(torrent_handle * hoPtr)->id() } |]

--    boost::shared_ptr<torrent> native_handle () const;

checkError :: IO (Ptr C'ErrorCode) -> IO ()
checkError =
  withLibtorrentException TorrentHandleError
