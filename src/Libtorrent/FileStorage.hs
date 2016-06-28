{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Storage.html#file_storage file_storage> structure for "Libtorrent"
module Libtorrent.FileStorage (FileEntry(..)
                              , FileSlice(..)
                              , FileStorage(..)
                              , FileStorageFlags(..)
                              , FileFlags(..)
                              , getPath
                              , getSymlinkPath
                              , getOffset
                              , getSize
                              , getFileBase
                              , getMtime
                              , getFilehash
                              , getPadFile
                              , getHiddenAttribute
                              , getExecutableAttribute
                              , getSymlinkAttribute
                              , getFileIndex
                              , getFileSliceOffset
                              , getFileSliceSize
                              , fileStorageIsValid
                              , reserve
                              , addFile
                              , addFileFromEntry
                              , fileStorageRenameFile
                              , fileStorageMapBlock
                              , fileStorageMapFile
                              , fileStorageNumFiles
                              , fileEntryAt
                              , fileStorageTotalSize
                              , setFileStorageNumPieces
                              , fileStorgaeNumPieces
                              , setFileStoragePieceLength
                              , fileStoragePieceLength
                              , fileStoragePieceSize
                              , setFileStorageName
                              , fileStorageName
                              , fileStorageOptimize
                              , fileSize
                              , fileStorageHash
                              , fileName
                              , fileOffset
                              , fileStorageMtime
                              , padFileAt
                              , symlink
                              , filePath
                              , fileFlags
                              , setFileBase
                              , fileBase
                              , fileIndexAtOffset
                              ) where

import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.FileStorage.FileSlice
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.PeerRequest (PeerRequest)
import           Libtorrent.Sha1Hash
import           Libtorrent.String
import           Libtorrent.TH (defineStdVector)
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/file_storage.hpp>"

C.verbatim "typedef std::vector<libtorrent::file_slice> VectorFileSlice;"

C.using "namespace libtorrent"
C.using "namespace std"

$(defineStdVector "file_slice" "VectorFileSlice" ''C'FileSlice ''C'VectorFileSlice ''FileSlice)

data FileStorageFlags =
  PadFile
  | AttributeHidden
  | AttributeExecutable
  | AttributeSymlink
  deriving (Show, Enum, Bounded)

data FileFlags =
  FlagPadFile
  | FlagHidden
  | FlagExecutable
  | FlagSymlink
  deriving (Show, Enum, Bounded)

newtype FileEntry = FileEntry { unFileEntry :: ForeignPtr (CType FileEntry)}

instance Show FileEntry where
  show _ = "FileEntry"

instance Inlinable FileEntry where
  type (CType FileEntry) = C'FileEntry

instance FromPtr FileEntry where
  fromPtr = objFromPtr FileEntry $ \ptr ->
    [CU.exp| void { delete $(file_entry * ptr); } |]

instance WithPtr FileEntry where
  withPtr (FileEntry fptr) = withForeignPtr fptr

getPath :: FileEntry -> IO Text
getPath ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(file_entry * hoPtr)->path) } |]
  stdStringToText res

getSymlinkPath :: FileEntry -> IO Text
getSymlinkPath ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(file_entry * hoPtr)->symlink_path) } |]
  stdStringToText res

getOffset :: FileEntry -> IO C.CSize
getOffset ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->offset } |]

getSize :: FileEntry -> IO C.CSize
getSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->size } |]

getFileBase :: FileEntry -> IO C.CSize
getFileBase ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->file_base } |]

getMtime :: FileEntry -> IO C.CTime
getMtime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(file_entry * hoPtr)->mtime } |]

getFilehash :: FileEntry -> IO ByteString
getFilehash ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(file_entry * hoPtr)->filehash) } |]
  sha1HashToByteString res

getPadFile :: FileEntry -> IO CInt
getPadFile ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_entry * hoPtr)->pad_file } |]

getHiddenAttribute :: FileEntry -> IO Bool
getHiddenAttribute ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_entry * hoPtr)->hidden_attribute } |]

getExecutableAttribute :: FileEntry -> IO Bool
getExecutableAttribute ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_entry * hoPtr)->executable_attribute } |]

getSymlinkAttribute :: FileEntry -> IO Bool
getSymlinkAttribute ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_entry * hoPtr)->symlink_attribute } |]

newtype FileStorage = FileStorage { unFileStorage :: ForeignPtr (CType FileStorage)}

instance Show FileStorage where
  show _ = "FileStorage"

instance Inlinable FileStorage where
  type (CType FileStorage) = C'FileStorage

instance FromPtr FileStorage where
  fromPtr = objFromPtr FileStorage $ \ptr ->
    [CU.exp| void { delete $(file_storage * ptr); } |]

instance WithPtr FileStorage where
  withPtr (FileStorage fptr) = withForeignPtr fptr

fileStorageIsValid :: FileStorage -> IO Bool
fileStorageIsValid ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_storage * hoPtr)->is_valid() } |]

reserve :: FileStorage -> CInt -> IO ()
reserve ho num_files =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(file_storage * hoPtr)->reserve($(int num_files)) } |]

addFile :: FileStorage -> Text -> C.CSize -> BitFlags FileStorageFlags -> Maybe C.CTime -> IO ()
addFile ho fpath size flags mtime =
  withPtr ho $ \hoPtr -> do
  fpath' <- textToStdString fpath
  let flags' = fromIntegral $ fromEnum flags
      mtime' = fromMaybe 0 mtime
  withPtr fpath' $ \fpathPtr ->
    [CU.exp| void { $(file_storage * hoPtr)->add_file(*$(string * fpathPtr), $(size_t size), $(int flags'), $(time_t mtime')) } |]

addFileFromEntry :: FileStorage -> FileEntry -> IO ()
addFileFromEntry ho fe =
  withPtr ho $ \hoPtr ->
  withPtr fe $ \fePtr ->
  [CU.exp| void { $(file_storage * hoPtr)->add_file(*$(file_entry * fePtr)) } |]

fileStorageRenameFile :: FileStorage -> CInt -> Text -> IO ()
fileStorageRenameFile ho idx fname =
  withPtr ho $ \hoPtr -> do
  fname' <- textToStdString fname
  withPtr fname' $ \fnamePtr ->
    [CU.exp| void { $(file_storage * hoPtr)->rename_file($(int idx), *$(string * fnamePtr)) } |]

fileStorageMapBlock :: FileStorage -> CInt -> C.CSize -> CInt -> IO (StdVector FileSlice)
fileStorageMapBlock ho piece offset size =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorFileSlice * { new VectorFileSlice($(file_storage * hoPtr)->map_block($(int piece), $(size_t offset), $(int size))) } |]

fileStorageMapFile :: FileStorage -> CInt -> C.CSize -> CInt -> IO PeerRequest
fileStorageMapFile ho file offset size =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| peer_request * { new peer_request($(file_storage * hoPtr)->map_file($(int file), $(size_t offset), $(int size))) } |]

fileStorageNumFiles :: FileStorage -> IO CInt
fileStorageNumFiles ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_storage * hoPtr)->num_files() } |]

fileEntryAt :: FileStorage -> CInt ->IO FileEntry
fileEntryAt ho idx =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| file_entry * { new file_entry($(file_storage * hoPtr)->at($(int idx))) } |]

fileStorageTotalSize :: FileStorage -> IO C.CSize
fileStorageTotalSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_storage * hoPtr)->total_size() } |]

setFileStorageNumPieces :: FileStorage -> CInt -> IO ()
setFileStorageNumPieces ho n =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(file_storage * hoPtr)->set_num_pieces($(int n)) } |]

fileStorgaeNumPieces :: FileStorage -> IO CInt
fileStorgaeNumPieces ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_storage * hoPtr)->num_pieces() } |]

setFileStoragePieceLength :: FileStorage -> CInt -> IO ()
setFileStoragePieceLength ho l =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(file_storage * hoPtr)->set_piece_length($(int l)) } |]

fileStoragePieceLength :: FileStorage -> IO CInt
fileStoragePieceLength ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_storage * hoPtr)->piece_length() } |]

fileStoragePieceSize :: FileStorage -> CInt -> IO CInt
fileStoragePieceSize ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_storage * hoPtr)->piece_size($(int idx)) } |]

setFileStorageName :: FileStorage -> Text -> IO ()
setFileStorageName ho n =
  withPtr ho $ \hoPtr -> do
  sn <- textToStdString n
  withPtr sn $ \snPtr ->
    [CU.exp| void { $(file_storage * hoPtr)->set_name(*$(string * snPtr)) } |]

fileStorageName :: FileStorage -> IO Text
fileStorageName ho =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(file_storage * hoPtr)->name()) } |]
  stdStringToText str

fileStorageOptimize :: FileStorage -> IO ()
fileStorageOptimize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(file_storage * hoPtr)->optimize() } |]

fileSize :: FileStorage -> CInt -> IO C.CSize
fileSize ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_storage * hoPtr)->file_size($(int idx)) } |]

fileStorageHash :: FileStorage -> CInt -> IO ByteString
fileStorageHash ho idx =
  withPtr ho $ \hoPtr -> do
  h <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(file_storage * hoPtr)->hash($(int idx))) } |]
  sha1HashToByteString h
  
fileName :: FileStorage -> CInt -> IO Text
fileName ho idx =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(file_storage * hoPtr)->file_name($(int idx))) } |]
  stdStringToText str

fileOffset :: FileStorage -> CInt -> IO C.CSize
fileOffset ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_storage * hoPtr)->file_offset($(int idx)) } |]

fileStorageMtime :: FileStorage -> CInt -> IO C.CTime
fileStorageMtime ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(file_storage * hoPtr)->mtime($(int idx)) } |]

padFileAt :: FileStorage -> CInt -> IO Bool
padFileAt ho idx =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_storage * hoPtr)->pad_file_at($(int idx)) } |]

symlink :: FileStorage -> CInt -> IO Text
symlink ho idx =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(file_storage * hoPtr)->symlink($(int idx))) } |]
  stdStringToText str

filePath :: FileStorage -> CInt -> IO Text
filePath ho idx =
  withPtr ho $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(file_storage * hoPtr)->file_path($(int idx))) } |]
  stdStringToText str

fileFlags :: FileStorage -> CInt -> IO (BitFlags FileFlags)
fileFlags ho idx =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(file_storage * hoPtr)->file_flags($(int idx)) } |]

setFileBase :: FileStorage -> CInt -> C.CSize -> IO ()
setFileBase ho idx off =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(file_storage * hoPtr)->set_file_base($(int idx), $(size_t off)) } |]

fileBase :: FileStorage -> CInt -> IO C.CSize
fileBase ho idx =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_storage * hoPtr)->file_base($(int idx)) } |]

fileIndexAtOffset :: FileStorage -> C.CSize -> IO CInt
fileIndexAtOffset ho offset =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_storage * hoPtr)->file_index_at_offset($(size_t offset)) } |]
