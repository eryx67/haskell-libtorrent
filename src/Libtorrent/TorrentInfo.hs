{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent-info torrent_info> structure for "Libtorrent"

module Libtorrent.TorrentInfo( TorrentInfo(..)
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

import           Libtorrent.Exceptions
import           Libtorrent.FileStorage (FileStorage, FileSlice)
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.PeerRequest (PeerRequest)
import           Libtorrent.Sha1Hash
import           Libtorrent.String
import           Libtorrent.TH (defineStdVector)
import           Libtorrent.TorrentInfo.AnnounceEntry (AnnounceEntry)
import           Libtorrent.Types


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
newTorrentInfo :: Text -> IO TorrentInfo
newTorrentInfo fname =
  TF.withCStringLen fname $ \(ptr, len) -> do
  let csize = fromIntegral len
  withErrorCode TorrentInfoError $ \ecPtr ->
    fromPtr [CU.block|
              torrent_info * {
                error_code *ec = $(error_code * ecPtr);
                return new torrent_info(std::string($(const char * ptr), $(size_t csize)), *ec, 0);
              }
            |]

-- | Can throw 'LibtorrentException'.
torrentInfoFromBuffer :: ByteString -> IO TorrentInfo
torrentInfoFromBuffer bs =
  withErrorCode TorrentInfoError $ \ePtr ->
  fromPtr [CU.exp| torrent_info * { new torrent_info($bs-ptr:bs, $bs-len:bs, *$(error_code * ePtr)) } |]

torrentInfoFromInfoHash :: Sha1Hash -> IO TorrentInfo
torrentInfoFromInfoHash ih =
  withPtr ih $ \ihPtr ->
  fromPtr [CU.exp| torrent_info * { new torrent_info(*$(sha1_hash * ihPtr)) } |]


files :: TorrentInfo -> IO FileStorage
files ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| file_storage * { new file_storage($(torrent_info * hoPtr)->files()) } |]

origFiles :: TorrentInfo -> IO FileStorage
origFiles ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| file_storage * { new file_storage($(torrent_info * hoPtr)->orig_files()) } |]

torrentInfoRenameFile :: TorrentInfo -> C.CInt -> Text -> IO ()
torrentInfoRenameFile ho idx fname =
  withPtr ho $ \hoPtr -> do
  fname' <- textToStdString fname
  withPtr fname' $ \fnamePtr ->
    [CU.exp| void { $(torrent_info * hoPtr)->rename_file($(int idx), *$(string * fnamePtr)) } |]

remapFiles :: TorrentInfo -> FileStorage -> IO ()
remapFiles ho fs =
  withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  [CU.exp| void { $(torrent_info * hoPtr)->remap_files(*$(file_storage * fsPtr)) } |]

torrentInfoTrackers :: TorrentInfo -> IO (StdVector AnnounceEntry)
torrentInfoTrackers ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorAnnounceEntry * { new VectorAnnounceEntry($(torrent_info * hoPtr)->trackers()) } |]

torrentInfoAddTracker :: TorrentInfo -> Text -> (Maybe C.CInt) -> IO ()
torrentInfoAddTracker ho url tier =
  withPtr ho $ \hoPtr -> do
  let tier' = fromMaybe 0 tier
  surl <- textToStdString url
  withPtr surl $ \surlPtr ->
    [CU.exp| void { $(torrent_info * hoPtr)->add_tracker(*$(string * surlPtr), $(int tier')) } |]

-- TODO: libtorrent 1.1
-- similarTorrents :: TorrentInfo -> IO (StdVector Sha1Hash)
-- similarTorrents ho =
--   withPtr ho $ \hoPtr ->
--   fromPtr [CU.exp| VectorSha1Hash * { new VectorSha1Hash($(torrent_info * hoPtr)->similar_torrents()) } |]

-- TODO: libtorrent 1.1
-- collections :: TorrentInfo -> IO [Text]
-- collections ho =
--   withPtr ho $ \hoPtr -> do
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

totalSize :: TorrentInfo -> IO Int64
totalSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_info * hoPtr)->total_size() } |]

numPieces :: TorrentInfo -> IO C.CInt
numPieces ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->num_pieces() } |]

pieceLength :: TorrentInfo -> IO C.CInt
pieceLength ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->piece_length() } |]

torrentInfoInfoHash :: TorrentInfo -> IO ByteString
torrentInfoInfoHash ho =
  withPtr ho $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_info * hoPtr)->info_hash()) } |]
  sha1HashToByteString ih

numFiles :: TorrentInfo -> IO C.CInt
numFiles ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_info * hoPtr)->num_files() } |]

mapBlock :: TorrentInfo -> C.CInt -> Int64 -> C.CInt -> IO (StdVector FileSlice)
mapBlock ho piece offset size =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorFileSlice * { new VectorFileSlice($(torrent_info * hoPtr)->map_block($(int piece), $(int64_t offset), $(int size))) } |]

mapFile :: TorrentInfo -> C.CInt -> Int64 -> C.CInt -> IO PeerRequest
mapFile ho file offset size =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| peer_request * { new peer_request($(torrent_info * hoPtr)->map_file($(int file), $(int64_t offset), $(int size))) } |]

-- TODO: libtorrent 1.1
-- unload :: TorrentInfo -> IO ()
-- unload ho =
--   withPtr ho $ \hoPtr ->
--                  [CU.exp| void { $(torrent_info * hoPtr)->unload() } |]

-- TODO: libtorrent 1.1

-- -- | Can throw 'LibtorrentException'
-- load :: TorrentInfo -> ByteString ->IO ()
-- load ho buf =
--   withPtr ho $ \hoPtr ->
--   withErrorCode TorrentInfoError $ \ePtr ->
--   [CU.exp| void { $(torrent_info * hoPtr)->load($bs-ptr:buf, $bs-len:buf, *$(error_code * ePtr)) } |]

sslCert :: TorrentInfo -> IO ByteString
sslCert ho =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->ssl_cert()) } |]
  stdStringToByteString str

torrentInfoIsValid :: TorrentInfo -> IO Bool
torrentInfoIsValid ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_valid() } |]

priv :: TorrentInfo -> IO Bool
priv ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->priv() } |]

isI2p :: TorrentInfo -> IO Bool
isI2p ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_i2p() } |]

hashForPiece :: TorrentInfo -> C.CInt -> IO ByteString
hashForPiece ho idx =
  withPtr ho $ \hoPtr -> do
  h <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_info * hoPtr)->hash_for_piece($(int idx))) } |]
  sha1HashToByteString h

pieceSize :: TorrentInfo -> C.CInt -> IO C.CInt
pieceSize ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_info * hoPtr)->piece_size($(int idx)) } |]

-- TODO: libtorrent 1.1
-- isLoaded :: TorrentInfo -> IO Bool
-- isLoaded ho =
--   withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_loaded() } |]

merkleTree :: TorrentInfo -> IO (StdVector Sha1Hash)
merkleTree ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorSha1Hash * { new VectorSha1Hash($(torrent_info * hoPtr)->merkle_tree()) } |]

setMerkleTree :: TorrentInfo -> StdVector Sha1Hash -> IO ()
setMerkleTree ho mt =
  withPtr ho $ \hoPtr ->
  withPtr mt $ \mtPtr ->
  [CU.exp| void { $(torrent_info * hoPtr)->set_merkle_tree(*$(VectorSha1Hash * mtPtr)) } |]

creationDate :: TorrentInfo -> IO (Maybe C.CTime)
creationDate ho =
  withPtr ho $ \hoPtr -> do
  res <- [CU.block| time_t {
             boost::optional<time_t> v = $(torrent_info * hoPtr)->creation_date();
             return (v ? *v : 0);
            }
         |]
  if res == 0
  then return Nothing
  else return $ Just res

torrentInfoName :: TorrentInfo -> IO Text
torrentInfoName ho =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->name()) } |]
  stdStringToText str

torrentInfoComment :: TorrentInfo -> IO Text
torrentInfoComment ho =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->comment()) } |]
  stdStringToText str
  
torrentInfoCreator :: TorrentInfo -> IO Text
torrentInfoCreator ho =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(torrent_info * hoPtr)->creator()) } |]
  stdStringToText str

-- TODO:
--    nodes_t const& nodes () const;
--    void add_node (std::pair<std::string, int> const& node);
--    bool parse_info_section (bdecode_node const& e, error_code& ec, int flags);
--    bdecode_node info (char const* key) const;

torrentInfoMetadata :: TorrentInfo -> IO ByteString 
torrentInfoMetadata ho =
  withPtr ho $ \hoPtr -> do
  (len, buf) <- C.withPtrs_ $ \(lPtr, bufPtr) -> do
    [CU.exp| void {
      *$(char ** bufPtr) = $(torrent_info * hoPtr)->metadata().get();
      *$(int * lPtr) = $(torrent_info * hoPtr)->metadata_size();
     }
    |]
  BS.packCStringLen (buf, fromIntegral len)

isMerkleTorrent :: TorrentInfo -> IO Bool
isMerkleTorrent ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_info * hoPtr)->is_merkle_torrent() } |]

-- TODO:
--    bool parse_torrent_file (bdecode_node const& libtorrent, error_code& ec, int flags);


