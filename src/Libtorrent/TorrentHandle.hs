{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent-handle torrent_handle> structure for "Libtorrent"
-- All functions in this module can throw 'LibtorrentException'.


module Libtorrent.TorrentHandle ( TorrentHandle(..)
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
                                , uploadLimit
                                , downloadLimit
                                , setUploadLimit
                                , setDownloadLimit
                                , setSequentialDownload
                                , maxUploads
                                , setMaxUploads
                                , maxConnections
                                , setMaxConnections
                                , moveStorage
                                , renameFile
                                , superSeeding
                                , infoHash
                                ) where

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

import           Libtorrent.Exceptions
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.PeerInfo
import           Libtorrent.Sha1Hash (Sha1Hash)
import           Libtorrent.String
import           Libtorrent.TorrentHandle.PartialPieceInfo
import           Libtorrent.TorrentHandle.TorrentStatus
import           Libtorrent.TorrentInfo.AnnounceEntry
import           Libtorrent.TorrentInfo ()
import           Libtorrent.Types
import           Libtorrent.TH (defineStdVector)

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
  deriving (Show, Enum, Bounded)

data StatusFlags =
  QueryDistributedCopies
  | QueryAccurateDownloadCounters
  | QueryLastSeenComplete
  | QueryPieces
  | QueryVerifiedPieces
  | QueryTorrentFile
  | QueryName
  | QuerySavePath
  deriving (Show, Enum, Bounded)

data DeadlineFlags =
  AlertWhenAvailable
  deriving (Show, Enum, Bounded)

data FileProgressFlags =
  PieceGranularity
  deriving (Show, Enum, Bounded)

data PauseFlags =
  GracefulPause
  deriving (Show, Enum, Bounded)

data SaveResumeFlags =
  FlushDiskCache
  | SaveInfoDict
  | OnlyIfModified
  deriving (Show, Enum, Bounded)

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
newTorrentHandle :: IO TorrentHandle
newTorrentHandle =
  fromPtr [C.exp| torrent_handle * { new torrent_handle() } |]

addPiece :: TorrentHandle -> CInt -> ByteString -> IO ()
addPiece th pieceNum pieceData =
  withPtr th $ \thPtr ->
  checkError [exceptU| { $(torrent_handle * thPtr)->add_piece($(int pieceNum), $bs-ptr:pieceData); } |]

readPiece :: TorrentHandle -> CInt -> IO ()
readPiece th pieceNum =
  withPtr th $ \thPtr ->
  checkError [exceptU| { $(torrent_handle * thPtr)->read_piece($(int pieceNum)); } |]

havePiece :: TorrentHandle -> CInt -> IO Bool
havePiece th pieceNum =
  withPtr th $ \thPtr ->
  fmap toBool . C.withPtr_ $ \vPtr -> do
    checkError [exceptU| { *$(bool * vPtr) = $(torrent_handle * thPtr)->have_piece($(int pieceNum)); } |]

getPeerInfo :: TorrentHandle -> IO (StdVector PeerInfo)
getPeerInfo th =
  withPtr th $ \thPtr -> do
  fromPtr . C.withPtr_ $ \resPtr ->
    checkError [exceptU| {
                       *$(VectorPeerInfo ** resPtr) = new VectorPeerInfo();
                       $(torrent_handle * thPtr)->get_peer_info(**$(VectorPeerInfo ** resPtr));
                       }
                       |]

torrentStatus :: TorrentHandle -> Maybe (BitFlags StatusFlags) -> IO TorrentStatus
torrentStatus ho flags =
  withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0xffffffff (fromIntegral . fromEnum <$> flags)
    fromPtr . C.withPtr_ $ \tsPtr ->
      checkError [exceptU| {
                         *$(torrent_status ** tsPtr) =
                         new torrent_status($(torrent_handle * hoPtr)->status($(uint32_t flags')));
                         }
                         |]

getDownloadQueue :: TorrentHandle -> IO (StdVector PartialPieceInfo)
getDownloadQueue ho =
  withPtr ho $ \hoPtr ->
  fromPtr . C.withPtr_ $ \ppiPtr ->
  checkError [exceptU| {
                     *$(VectorPartialPieceInfo ** ppiPtr) = new VectorPartialPieceInfo();
                     $(torrent_handle * hoPtr)->get_download_queue(**$(VectorPartialPieceInfo ** ppiPtr));
                     }
                     |]

resetPieceDeadline :: TorrentHandle -> C.CInt -> IO ()
resetPieceDeadline ho idx =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->reset_piece_deadline($(int idx)); } |]

clearPieceDeadlines :: TorrentHandle -> IO ()
clearPieceDeadlines ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->clear_piece_deadlines(); } |]

setPieceDeadline :: TorrentHandle -> C.CInt -> C.CInt -> Maybe (BitFlags DeadlineFlags) -> IO ()
setPieceDeadline ho idx deadline flags =
  withPtr ho $ \hoPtr -> do
  let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
  checkError [exceptU|
                     { $(torrent_handle * hoPtr)->set_piece_deadline($(int idx), $(int deadline), $(int flags')); }
                     |]

setPriority :: TorrentHandle -> C.CInt -> IO ()
setPriority ho prio =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->set_priority($(int prio)); } |]

fileProgress :: TorrentHandle -> Maybe (BitFlags FileProgressFlags) -> IO (Vector Int64)
fileProgress ho flags =
  withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [exceptU| {
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

clearError :: TorrentHandle -> IO ()
clearError ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->clear_error(); } |]

trackers :: TorrentHandle -> IO (StdVector AnnounceEntry)
trackers ho =
  withPtr ho $ \hoPtr ->
  fromPtr . C.withPtr_ $ \aePtr ->
  checkError [exceptU| {
                     *$(VectorAnnounceEntry ** aePtr) = new VectorAnnounceEntry($(torrent_handle * hoPtr)->trackers());
                     }
                     |]

replaceTrackers :: TorrentHandle -> StdVector AnnounceEntry -> IO ()
replaceTrackers ho aes =
  withPtr ho $ \hoPtr ->
  withPtr aes $ \aesPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->replace_trackers(*$(VectorAnnounceEntry * aesPtr)); } |]

addTracker :: TorrentHandle -> AnnounceEntry -> IO ()
addTracker ho ae =
  withPtr ho $ \hoPtr ->
  withPtr ae $ \aePtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->add_tracker(*$(announce_entry * aePtr)); } |]

addUrlSeed :: TorrentHandle -> Text -> IO ()
addUrlSeed ho url = do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->add_url_seed(*$(string * sPtr)); } |]

removeUrlSeed :: TorrentHandle -> Text ->IO ()
removeUrlSeed ho url = do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->remove_url_seed(*$(string * sPtr)); } |]

--    std::set<std::string> url_seeds () const;

addHttpSeed :: TorrentHandle -> Text -> IO ()
addHttpSeed ho url = do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->add_http_seed(*$(string * sPtr)); } |]

removeHttpSeed :: TorrentHandle -> Text ->IO ()
removeHttpSeed ho url = do
  s <- textToStdString url
  withPtr ho $ \hoPtr ->
    withPtr s $ \sPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->remove_http_seed(*$(string * sPtr)); } |]

--    std::set<std::string> http_seeds () const;

--    void add_extension (
--       boost::function<boost::shared_ptr<torrent_plugin>(torrent_handle const&, void*)> const& ext
--       , void* userdata = 0);

setMetadata :: TorrentHandle -> ByteString -> IO Bool
setMetadata ho md =
  withPtr ho $ \hoPtr ->
  fmap toBool . C.withPtr_ $ \ptr ->
    checkError [exceptU| { *$(bool * ptr) = $(torrent_handle * hoPtr)->set_metadata($bs-ptr:md, $bs-len:md); } |]

isValid :: TorrentHandle -> IO Bool
isValid ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_handle * hoPtr)->is_valid() } |]

pause :: TorrentHandle -> Maybe (BitFlags PauseFlags) -> IO ()
pause ho flags =
  withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [exceptU| { $(torrent_handle * hoPtr)->pause($(int flags')); } |]

resume :: TorrentHandle -> IO ()
resume ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->resume(); } |]

-- TODO in libtorrent 1.1.0
-- stopWhenReady :: TorrentHandle -> Bool -> IO ()
-- stopWhenReady ho v = do
--   let b = fromBool v
--   withPtr ho $ \hoPtr ->
--     checkError [exceptU| void { $(torrent_handle * hoPtr)->stop_when_ready($(bool b)) } |]

setUploadMode :: TorrentHandle -> Bool -> IO ()
setUploadMode ho v = do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->set_upload_mode($(bool b)); } |]

setShareMode :: TorrentHandle -> Bool -> IO ()
setShareMode ho v = do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->set_share_mode($(bool b)); } |]

flushCache :: TorrentHandle -> IO ()
flushCache ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->flush_cache(); } |]

applyIpFilter :: TorrentHandle -> Bool -> IO ()
applyIpFilter ho v = do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->apply_ip_filter($(bool b)); } |]

forceRecheck :: TorrentHandle -> IO ()
forceRecheck ho =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->force_recheck(); } |]

saveResumeData :: TorrentHandle -> Maybe (SaveResumeFlags) -> IO ()
saveResumeData ho flags =
  withPtr ho $ \hoPtr -> do
    let flags' = fromMaybe 0 (fromIntegral . fromEnum <$> flags)
    checkError [exceptU| { $(torrent_handle * hoPtr)->save_resume_data($(int flags')); } |]

needSaveResumeData :: TorrentHandle -> IO Bool
needSaveResumeData ho =
  withPtr ho $ \hoPtr ->
  fmap toBool . C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(bool * ptr) = $(torrent_handle * hoPtr)->need_save_resume_data(); } |]

autoManaged :: TorrentHandle -> Bool -> IO ()
autoManaged ho v = do
  let b = fromBool v
  withPtr ho $ \hoPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->auto_managed($(bool b)); } |]

queuePositionDown :: TorrentHandle -> IO ()
queuePositionDown ho =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->queue_position_down(); } |]

queuePositionTop :: TorrentHandle -> IO ()
queuePositionTop ho =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->queue_position_top(); } |]

queuePosition :: TorrentHandle -> IO CInt
queuePosition ho =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU|  { *$(int * ptr) = $(torrent_handle * hoPtr)->queue_position(); } |]

queuePositionBottom :: TorrentHandle -> IO ()
queuePositionBottom ho =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->queue_position_bottom(); } |]

queuePositionUp :: TorrentHandle -> IO ()
queuePositionUp ho =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->queue_position_up(); } |]

setSslCertificate :: TorrentHandle -> Text -> Text -> Text -> IO ()
setSslCertificate ho private_key dh_params passphrase = do
  pks <- textToStdString private_key
  ds <- textToStdString dh_params
  pas <- textToStdString passphrase
  withPtr ho $ \hoPtr ->
    withPtr pks $ \pksPtr ->
    withPtr ds $ \dsPtr ->
    withPtr pas $ \pasPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->set_ssl_certificate(*$(string * pksPtr), *$(string * dsPtr), *$(string * pasPtr)); } |]

--    void set_ssl_certificate_buffer (std::string const& certificate
--       , std::string const& private_key
--       , std::string const& dh_params);
--    storage_interface* get_storage_impl () const;
--    boost::shared_ptr<const torrent_info> torrent_file () const;

useInterface :: TorrentHandle -> [Text] -> IO ()
useInterface ho ifs = do
  withCAString (T.unpack $ T.intercalate "," ifs) $ \ifsPtr ->
    withPtr ho $ \hoPtr ->
    checkError [exceptU| { $(torrent_handle * hoPtr)->use_interface($(char * ifsPtr)); } |]

pieceAvailability :: TorrentHandle -> IO (Vector C.CInt)
pieceAvailability ho =
  withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [exceptU| {
                       std::vector<int> v;
                       $(torrent_handle * hoPtr)->piece_availability(v);
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

piecePriority :: TorrentHandle -> C.CInt -> IO C.CInt
piecePriority ho idx =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->piece_priority($(int idx)); } |]

piecePriorities :: TorrentHandle -> IO (Vector C.CInt)
piecePriorities ho =
  withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [exceptU| {
                       std::vector<int> v = $(torrent_handle * hoPtr)->piece_priorities();
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

setPiecePriority :: TorrentHandle -> C.CInt -> C.CInt -> IO ()
setPiecePriority ho idx priority=
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->piece_priority($(int idx), $(int priority)); } |]


prioritizePieces :: TorrentHandle -> Vector C.CInt -> IO ()
prioritizePieces ho ps =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| {
      std::vector<int> v($vec-ptr:(int * ps), $vec-ptr:(int * ps) + $vec-len:ps);
      $(torrent_handle * hoPtr)->prioritize_pieces(v);
     }
  |]

--    void prioritize_pieces (std::vector<std::pair<int, int> > const& pieces) const;

filePriority :: TorrentHandle -> C.CInt -> IO C.CInt
filePriority ho idx =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->file_priority($(int idx)); } |]

filePriorities :: TorrentHandle -> IO (Vector C.CInt)
filePriorities ho =
  withPtr ho $ \hoPtr -> do
  (dataSize, dataPtr) <- C.withPtrs_ $ \(dataSizePtr, dataPtrPtr) -> do
    checkError [exceptU| {
                       std::vector<int> v = $(torrent_handle * hoPtr)->file_priorities();
                       *$(int ** dataPtrPtr) = new int[v.size()];
                       std::copy(v.data(), v.data() + v.size(), *$(int ** dataPtrPtr));
                       *$(int * dataSizePtr) = v.size();
                       }
                       |]
  dataFPtr <- newForeignPtr dataPtr $ [CU.exp| void {delete $(int * dataPtr)} |]
  return $ unsafeFromForeignPtr0 dataFPtr (fromIntegral dataSize)

setFilePriority :: TorrentHandle -> C.CInt -> C.CInt -> IO ()
setFilePriority ho idx priority=
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->file_priority($(int idx), $(int priority)); } |]


prioritizeFiles :: TorrentHandle -> Vector C.CInt -> IO ()
prioritizeFiles ho ps =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| {
      std::vector<int> v($vec-ptr:(int * ps), $vec-ptr:(int * ps) + $vec-len:ps);
      $(torrent_handle * hoPtr)->prioritize_files(v);
     }
  |]

forceReannounce :: TorrentHandle -> Maybe C.CInt -> Maybe C.CInt -> IO ()
forceReannounce ho seconds tracker_index =
  withPtr ho $ \hoPtr -> do
  let sec = fromMaybe 0 seconds
      tidx = fromMaybe (-1) tracker_index
  checkError [exceptU| { $(torrent_handle * hoPtr)->force_reannounce($(int sec), $(int tidx)); } |]

forceDhtAnnounce :: TorrentHandle -> IO ()
forceDhtAnnounce ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->force_dht_announce(); } |]

scrapeTracker :: TorrentHandle -> IO ()
scrapeTracker ho =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->scrape_tracker(); } |]

uploadLimit :: TorrentHandle -> IO CInt
uploadLimit ho =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->upload_limit(); } |]

downloadLimit :: TorrentHandle -> IO CInt
downloadLimit ho =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->download_limit(); } |]

setUploadLimit :: TorrentHandle -> C.CInt -> IO ()
setUploadLimit ho limit =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->set_upload_limit($(int limit)); } |]

setDownloadLimit :: TorrentHandle -> C.CInt -> IO ()
setDownloadLimit ho limit =
  withPtr ho $ \hoPtr ->
                 checkError [exceptU| { $(torrent_handle * hoPtr)->set_download_limit($(int limit)); } |]


-- TODO in libtorrent 1.1.0
-- setPinned :: TorrentHandle -> Bool -> IO ()
-- setPinned ho sd =
--   withPtr ho $ \hoPtr -> do
--     let sd' = fromBool sd
--     checkError [exceptU| void { $(torrent_handle * hoPtr)->set_pinned($(bool sd')) } |]

setSequentialDownload :: TorrentHandle -> Bool -> IO ()
setSequentialDownload ho sd =
  withPtr ho $ \hoPtr -> do
    let sd' = fromBool sd
    checkError [exceptU| { $(torrent_handle * hoPtr)->set_sequential_download($(bool sd')); } |]

--  void connect_peer (tcp::endpoint const& adr, int source = 0, int flags = 0x1 + 0x4 + 0x8) const;

maxUploads :: TorrentHandle -> IO C.CInt
maxUploads ho =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->max_uploads(); } |]

setMaxUploads :: TorrentHandle -> C.CInt -> IO ()
setMaxUploads ho max_uploads =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->set_max_uploads($(int max_uploads)); } |]

maxConnections :: TorrentHandle -> IO C.CInt
maxConnections ho =
  withPtr ho $ \hoPtr ->
  C.withPtr_ $ \ptr ->
  checkError [exceptU| { *$(int * ptr) = $(torrent_handle * hoPtr)->max_connections(); } |]

setMaxConnections :: TorrentHandle -> C.CInt -> IO ()
setMaxConnections ho max_connections =
  withPtr ho $ \hoPtr ->
  checkError [exceptU| { $(torrent_handle * hoPtr)->set_max_connections($(int max_connections)); } |]

moveStorage :: TorrentHandle -> Text -> C.CInt -> IO ()
moveStorage ho save_path flags =
  withPtr ho $ \hoPtr -> do
    sp <- textToStdString save_path
    withPtr sp $ \sPtr ->
      checkError [exceptU| { $(torrent_handle * hoPtr)->move_storage(*$(string * sPtr), $(int flags)); } |]

renameFile :: TorrentHandle -> C.CInt -> Text -> IO ()
renameFile ho idx new_name =
  withPtr ho $ \hoPtr -> do
    nns <- textToStdString new_name
    withPtr nns $ \nnPtr ->
      checkError [exceptU| { $(torrent_handle * hoPtr)->rename_file($(int idx), *$(string * nnPtr)); } |]

superSeeding :: TorrentHandle -> Bool -> IO ()
superSeeding ho on =
  withPtr ho $ \hoPtr -> do
  let on' = fromBool on
  checkError [exceptU| { $(torrent_handle * hoPtr)->super_seeding($(bool on')); } |]


infoHash :: TorrentHandle -> IO Sha1Hash
infoHash ho =
  withPtr ho $ \hoPtr ->
  alloca $ \shPtrPtr -> do
  checkError [exceptU|
                     {*$(sha1_hash ** shPtrPtr) = new sha1_hash($(torrent_handle * hoPtr)->info_hash());}
                     |]
  fromPtr $ peek shPtrPtr

-- TODO in libtorrent 1.1.0
-- getId :: TorrentHandle -> IO Word32
-- getId ho =
--   withPtr ho $ \hoPtr ->
--   checkError [exceptU| uint32_t { $(torrent_handle * hoPtr)->id() } |]

--    boost::shared_ptr<torrent> native_handle () const;

checkError :: IO (Ptr C'ErrorCode) -> IO ()
checkError =
  withLibtorrentException TorrentHandleError
