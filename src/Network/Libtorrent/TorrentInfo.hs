{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent-info torrent_info> structure for "Libtorrent"

module Network.Libtorrent.TorrentInfo( TorrentInfo(..)
                             , newTorrentInfo
                             , torrentInfoFromBuffer
                             , torrentInfoFromInfoHash
                             , files
                             , origFiles
                             , torrentInfoRenameFile
                             , remapFiles
                             , torrentInfoTrackers
                             , torrentInfoAddTracker
                             -- , similarTorrents
                             -- , collections
                             , totalSize
                             , numPieces
                             , pieceLength
                             , torrentInfoInfoHash
                             , numFiles
                             , mapBlock
                             , mapFile
                             -- , unload
                             -- , load
                             , sslCert
                             , torrentInfoIsValid
                             , priv
                             , isI2p
                             , hashForPiece
                             , pieceSize
                             -- , isLoaded
                             , merkleTree
                             , setMerkleTree
                             , creationDate
                             , torrentInfoName
                             , torrentInfoComment
                             , torrentInfoCreator
                             , torrentInfoMetadata
                             , isMerkleTorrent
                             ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Exceptions
import           Network.Libtorrent.FileStorage (FileStorage, FileSlice)
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.PeerRequest (PeerRequest)
import           Network.Libtorrent.Sha1Hash
import           Network.Libtorrent.String
import           Network.Libtorrent.TH (defineStdVector)
import           Network.Libtorrent.TorrentInfo.AnnounceEntry (AnnounceEntry)
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/torrent_info.hpp>"

C.verbatim "typedef std::vector<libtorrent::sha1_hash> VectorSha1Hash;"
C.verbatim "typedef std::vector<libtorrent::announce_entry> VectorAnnounceEntry;"
C.verbatim "typedef std::vector<libtorrent::file_slice> VectorFileSlice;"

C.using "namespace libtorrent"
C.using "namespace std"

$(defineStdVector "sha1_hash" "VectorSha1Hash" ''C'Sha1Hash ''C'VectorSha1Hash ''Sha1Hash)

$(defineStdVector "announce_entry" "VectorAnnounceEntry" ''C'AnnounceEntry ''C'VectorAnnounceEntry ''AnnounceEntry)

newtype TorrentInfo = TorrentInfo { unTorrentInfo :: ForeignPtr (CType TorrentInfo)}

instance Show TorrentInfo where
  show _ = "TorrentInfo"

instance Inlinable TorrentInfo where
  type (CType TorrentInfo) = C'TorrentInfo

instance FromPtr TorrentInfo where
  fromPtr = objFromPtr TorrentInfo $ \ptr ->
    [CU.exp| void { delete $(torrent_info * ptr); } |]

instance WithPtr TorrentInfo where
  withPtr (TorrentInfo fptr) = withForeignPtr fptr

-- | Can throw 'LibtorrentException'.
newTorrentInfo :: MonadIO m =>  Text -> m TorrentInfo
newTorrentInfo fname =
  liftIO . TF.withCStringLen fname $ \(ptr, len) -> do
  let csize = fromIntegral len
  withErrorCode TorrentInfoError $ \ecPtr ->
    fromPtr [CU.block|
              torrent_info * {
                error_code *ec = $(error_code * ecPtr);
                return new torrent_info(std::string($(const char * ptr), $(size_t csize)), *ec, 0);
              }
            |]

-- | Can throw 'LibtorrentException'.
torrentInfoFromBuffer :: MonadIO m =>  ByteString -> m TorrentInfo
torrentInfoFromBuffer bs =
  liftIO . withErrorCode TorrentInfoError $ \ePtr ->
  fromPtr [CU.exp| torrent_info * { new torrent_info($bs-ptr:bs, $bs-len:bs, *$(error_code * ePtr)) } |]

torrentInfoFromInfoHash :: MonadIO m =>  Sha1Hash -> m TorrentInfo
torrentInfoFromInfoHash ih =
  liftIO . withPtr ih $ \ihPtr ->
  fromPtr [CU.exp| torrent_info * { new torrent_info(*$(sha1_hash * ihPtr)) } |]


files :: MonadIO m =>  TorrentInfo -> m FileStorage
files ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| file_storage * { new file_storage($(torrent_info * hoPtr)->files()) } |]

origFiles :: MonadIO m =>  TorrentInfo -> m FileStorage
origFiles ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| file_storage * { new file_storage($(torrent_info * hoPtr)->orig_files()) } |]

torrentInfoRenameFile :: MonadIO m =>  TorrentInfo -> C.CInt -> Text -> m ()
torrentInfoRenameFile ho idx fname =
  liftIO . withPtr ho $ \hoPtr -> do
  fname' <- textToStdString fname
  withPtr fname' $ \fnamePtr ->
    [CU.exp| void { $(torrent_info * hoPtr)->rename_file($(int idx), *$(string * fnamePtr)) } |]

remapFiles :: MonadIO m =>  TorrentInfo -> FileStorage -> m ()
remapFiles ho fs =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  [CU.exp| void { $(torrent_info * hoPtr)->remap_files(*$(file_storage * fsPtr)) } |]

torrentInfoTrackers :: MonadIO m =>  TorrentInfo -> m (StdVector AnnounceEntry)
torrentInfoTrackers ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorAnnounceEntry * { new VectorAnnounceEntry($(torrent_info * hoPtr)->trackers()) } |]

torrentInfoAddTracker :: MonadIO m =>  TorrentInfo -> Text -> (Maybe C.CInt) -> m ()
torrentInfoAddTracker ho url tier =
  liftIO . withPtr ho $ \hoPtr -> do
  let tier' = fromMaybe 0 tier
  surl <- textToStdString url
  withPtr surl $ \surlPtr ->
    [CU.exp| void { $(torrent_info * hoPtr)->add_tracker(*$(string * surlPtr), $(int tier')) } |]

-- TODO: libtorrent 1.1
-- similarTorrents :: MonadIO m =>  TorrentInfo -> m (StdVector Sha1Hash)
-- similarTorrents ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   fromPtr [CU.exp| VectorSha1Hash * { new VectorSha1Hash($(torrent_info * hoPtr)->similar_torrents()) } |]

-- TODO: libtorrent 1.1
-- collections :: MonadIO m =>  TorrentInfo -> m [Text]
-- collections ho =
--   liftIO . withPtr ho $ \hoPtr -> do
--   cs :: (StdVector StdString) <- fromPtr [CU.exp| VectorString * { new VectorString($(torrent_info * hoPtr)->collections()) } |]
--   toList cs >>= mapM stdStringToText

-- TODO:
--    void add_url_seed (std::string const& url
--       , std::string const& extern_auth = std::string()
--       , web_seed_entry::headers_t const& extra_headers = web_seed_entry::headers_t());
--    std::vector<web_seed_entry> const& web_seeds () const;
--    void set_web_seeds (std::vector<web_seed_entry> seeds);
--    void add_http_seed (std::string const& url
--       , std::string const& extern_auth = std::string()
--       , web_seed_entry::headers_t const& extra_headers = web_seed_entry::headers_t());

totalSize :: MonadIO m =>  TorrentInfo -> m Int64
totalSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_info * hoPtr)->total_size() } |]

numPieces :: MonadIO m =>  TorrentInfo -> m C.CInt
numPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->num_pieces() } |]

pieceLength :: MonadIO m =>  TorrentInfo -> m C.CInt
pieceLength ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->piece_length() } |]

torrentInfoInfoHash :: MonadIO m =>  TorrentInfo -> m ByteString
torrentInfoInfoHash ho =
  liftIO . withPtr ho $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_info * hoPtr)->info_hash()) } |]
  sha1HashToByteString ih

numFiles :: MonadIO m =>  TorrentInfo -> m C.CInt
numFiles ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->num_files() } |]

mapBlock :: MonadIO m =>  TorrentInfo -> C.CInt -> Int64 -> C.CInt -> m (StdVector FileSlice)
mapBlock ho piece offset size =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorFileSlice * { new VectorFileSlice($(torrent_info * hoPtr)->map_block($(int piece), $(int64_t offset), $(int size))) } |]

mapFile :: MonadIO m =>  TorrentInfo -> C.CInt -> Int64 -> C.CInt -> m PeerRequest
mapFile ho file offset size =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| peer_request * { new peer_request($(torrent_info * hoPtr)->map_file($(int file), $(int64_t offset), $(int size))) } |]

-- TODO: libtorrent 1.1
-- unload :: MonadIO m =>  TorrentInfo -> m ()
-- unload ho =
--   liftIO . withPtr ho $ \hoPtr ->
--                  [CU.exp| void { $(torrent_info * hoPtr)->unload() } |]

-- TODO: libtorrent 1.1

-- -- | Can throw 'LibtorrentException'
-- load :: MonadIO m =>  TorrentInfo -> ByteString ->m ()
-- load ho buf =
--   liftIO . withPtr ho $ \hoPtr ->
--   withErrorCode TorrentInfoError $ \ePtr ->
--   [CU.exp| void { $(torrent_info * hoPtr)->load($bs-ptr:buf, $bs-len:buf, *$(error_code * ePtr)) } |]

sslCert :: MonadIO m =>  TorrentInfo -> m ByteString
sslCert ho =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->ssl_cert()) } |]
  stdStringToByteString str

torrentInfoIsValid :: MonadIO m =>  TorrentInfo -> m Bool
torrentInfoIsValid ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_valid() } |]

priv :: MonadIO m =>  TorrentInfo -> m Bool
priv ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->priv() } |]

isI2p :: MonadIO m =>  TorrentInfo -> m Bool
isI2p ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_i2p() } |]

hashForPiece :: MonadIO m =>  TorrentInfo -> C.CInt -> m ByteString
hashForPiece ho idx =
  liftIO . withPtr ho $ \hoPtr -> do
  h <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_info * hoPtr)->hash_for_piece($(int idx))) } |]
  sha1HashToByteString h

pieceSize :: MonadIO m =>  TorrentInfo -> C.CInt -> m C.CInt
pieceSize ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_info * hoPtr)->piece_size($(int idx)) } |]

-- TODO: libtorrent 1.1
-- isLoaded :: MonadIO m =>  TorrentInfo -> m Bool
-- isLoaded ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_loaded() } |]

merkleTree :: MonadIO m =>  TorrentInfo -> m (StdVector Sha1Hash)
merkleTree ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorSha1Hash * { new VectorSha1Hash($(torrent_info * hoPtr)->merkle_tree()) } |]

setMerkleTree :: MonadIO m =>  TorrentInfo -> StdVector Sha1Hash -> m ()
setMerkleTree ho mt =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr mt $ \mtPtr ->
  [CU.exp| void { $(torrent_info * hoPtr)->set_merkle_tree(*$(VectorSha1Hash * mtPtr)) } |]

creationDate :: MonadIO m =>  TorrentInfo -> m (Maybe C.CTime)
creationDate ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- [CU.block| time_t {
             boost::optional<time_t> v = $(torrent_info * hoPtr)->creation_date();
             return (v ? *v : 0);
            }
         |]
  if res == 0
  then return Nothing
  else return $ Just res

torrentInfoName :: MonadIO m =>  TorrentInfo -> m Text
torrentInfoName ho =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->name()) } |]
  stdStringToText str

torrentInfoComment :: MonadIO m =>  TorrentInfo -> m Text
torrentInfoComment ho =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->comment()) } |]
  stdStringToText str
  
torrentInfoCreator :: MonadIO m =>  TorrentInfo -> m Text
torrentInfoCreator ho =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->creator()) } |]
  stdStringToText str

-- TODO:
--    nodes_t const& nodes () const;
--    void add_node (std::pair<std::string, int> const& node);
--    bool parse_info_section (bdecode_node const& e, error_code& ec, int flags);
--    bdecode_node info (char const* key) const;

torrentInfoMetadata :: MonadIO m =>  TorrentInfo -> m ByteString 
torrentInfoMetadata ho =
  liftIO . withPtr ho $ \hoPtr -> do
  (len, buf) <- C.withPtrs_ $ \(lPtr, bufPtr) -> do
    [CU.exp| void {
      *$(char ** bufPtr) = $(torrent_info * hoPtr)->metadata().get();
      *$(int * lPtr) = $(torrent_info * hoPtr)->metadata_size();
     }
    |]
  BS.packCStringLen (buf, fromIntegral len)

isMerkleTorrent :: MonadIO m =>  TorrentInfo -> m Bool
isMerkleTorrent ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_merkle_torrent() } |]

-- TODO:
--    bool parse_torrent_file (bdecode_node const& libtorrent, error_code& ec, int flags);


