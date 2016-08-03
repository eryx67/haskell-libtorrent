{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Storage.html#file_storage file_storage> structure for "Libtorrent"
module Network.Libtorrent.FileStorage (FileEntry(..)
                              , FileSlice(..)
                              , FileStorage(..)
                              , FileStorageFlags(..)
                              , FileFlags(..)
                              , newFileStorage
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
                              , fileStorageNumPieces
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

import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Data.ByteString                          (ByteString)
import           Data.Maybe                               (fromMaybe)
import           Data.Text                                (Text)
import           Foreign.C.Types                          (CInt)
import           Foreign.ForeignPtr                       (ForeignPtr,
                                                           withForeignPtr)
import           Foreign.Marshal.Utils                    (toBool)
import qualified Language.C.Inline                        as C
import qualified Language.C.Inline.Cpp                    as C
import qualified Language.C.Inline.Unsafe                 as CU

import           Network.Libtorrent.FileStorage.FileSlice
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.PeerRequest           (PeerRequest)
import           Network.Libtorrent.Sha1Hash
import           Network.Libtorrent.String
import           Network.Libtorrent.TH                    (defineStdVector)
import           Network.Libtorrent.Types


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
  deriving (Show, Enum, Bounded, Eq, Ord)

data FileFlags =
  FlagPadFile
  | FlagHidden
  | FlagExecutable
  | FlagSymlink
  deriving (Show, Enum, Bounded, Eq, Ord)

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

getPath :: MonadIO m =>  FileEntry -> m Text
getPath ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(file_entry * hoPtr)->path) } |]
  stdStringToText res

getSymlinkPath :: MonadIO m =>  FileEntry -> m Text
getSymlinkPath ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(file_entry * hoPtr)->symlink_path) } |]
  stdStringToText res

getOffset :: MonadIO m =>  FileEntry -> m C.CSize
getOffset ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->offset } |]

getSize :: MonadIO m =>  FileEntry -> m C.CSize
getSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->size } |]

getFileBase :: MonadIO m =>  FileEntry -> m C.CSize
getFileBase ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(file_entry * hoPtr)->file_base } |]

getMtime :: MonadIO m =>  FileEntry -> m C.CTime
getMtime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(file_entry * hoPtr)->mtime } |]

getFilehash :: MonadIO m =>  FileEntry -> m ByteString
getFilehash ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(file_entry * hoPtr)->filehash) } |]
  sha1HashToByteString res

getPadFile :: MonadIO m =>  FileEntry -> m CInt
getPadFile ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(file_entry * hoPtr)->pad_file } |]

getHiddenAttribute :: MonadIO m =>  FileEntry -> m Bool
getHiddenAttribute ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_entry * hoPtr)->hidden_attribute } |]

getExecutableAttribute :: MonadIO m =>  FileEntry -> m Bool
getExecutableAttribute ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(file_entry * hoPtr)->executable_attribute } |]

getSymlinkAttribute :: MonadIO m =>  FileEntry -> m Bool
getSymlinkAttribute ho =
  liftIO . withPtr ho $ \hoPtr ->
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

newFileStorage :: MonadIO m =>  m FileStorage
newFileStorage =
  liftIO $ fromPtr [C.exp| file_storage * { new file_storage()} |]

fileStorageIsValid :: MonadIO m =>  FileStorage -> m Bool
fileStorageIsValid ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [C.exp| bool { $(file_storage * hoPtr)->is_valid() } |]

reserve :: MonadIO m =>  FileStorage -> CInt -> m ()
reserve ho num_files =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(file_storage * hoPtr)->reserve($(int num_files)) } |]

addFile :: MonadIO m =>  FileStorage -> Text -> C.CSize -> BitFlags FileStorageFlags -> Maybe C.CTime -> Maybe Text -> m ()
addFile ho fpath size flags mtime slink =
  liftIO . withPtr ho $ \hoPtr -> do
  fpath' <- textToStdString fpath
  slink' <- textToStdString $ fromMaybe "" slink
  let flags' = fromIntegral $ fromEnum flags
      mtime' = fromMaybe 0 mtime
  withPtr fpath' $ \fpathPtr ->
    withPtr slink' $ \slinkPtr ->
    [C.exp| void { $(file_storage * hoPtr)->add_file(*$(string * fpathPtr), $(size_t size), $(int flags'), $(time_t mtime'), *$(string * slinkPtr)) } |]

addFileFromEntry :: MonadIO m =>  FileStorage -> FileEntry -> m ()
addFileFromEntry ho fe =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr fe $ \fePtr ->
  [C.exp| void { $(file_storage * hoPtr)->add_file(*$(file_entry * fePtr)) } |]

fileStorageRenameFile :: MonadIO m =>  FileStorage -> CInt -> Text -> m ()
fileStorageRenameFile ho idx fname =
  liftIO . withPtr ho $ \hoPtr -> do
  fname' <- textToStdString fname
  withPtr fname' $ \fnamePtr ->
    [C.exp| void { $(file_storage * hoPtr)->rename_file($(int idx), *$(string * fnamePtr)) } |]

fileStorageMapBlock :: MonadIO m =>  FileStorage -> CInt -> C.CSize -> CInt -> m (StdVector FileSlice)
fileStorageMapBlock ho piece offset size =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| VectorFileSlice * { new VectorFileSlice($(file_storage * hoPtr)->map_block($(int piece), $(size_t offset), $(int size))) } |]

fileStorageMapFile :: MonadIO m =>  FileStorage -> CInt -> C.CSize -> CInt -> m PeerRequest
fileStorageMapFile ho file offset size =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| peer_request * { new peer_request($(file_storage * hoPtr)->map_file($(int file), $(size_t offset), $(int size))) } |]

fileStorageNumFiles :: MonadIO m =>  FileStorage -> m CInt
fileStorageNumFiles ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(file_storage * hoPtr)->num_files() } |]

fileEntryAt :: MonadIO m =>  FileStorage -> CInt ->m FileEntry
fileEntryAt ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| file_entry * { new file_entry($(file_storage * hoPtr)->at($(int idx))) } |]

fileStorageTotalSize :: MonadIO m =>  FileStorage -> m C.CSize
fileStorageTotalSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| size_t { $(file_storage * hoPtr)->total_size() } |]

setFileStorageNumPieces :: MonadIO m =>  FileStorage -> CInt -> m ()
setFileStorageNumPieces ho n =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(file_storage * hoPtr)->set_num_pieces($(int n)) } |]

fileStorageNumPieces :: MonadIO m =>  FileStorage -> m CInt
fileStorageNumPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(file_storage * hoPtr)->num_pieces() } |]

setFileStoragePieceLength :: MonadIO m =>  FileStorage -> CInt -> m ()
setFileStoragePieceLength ho l =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(file_storage * hoPtr)->set_piece_length($(int l)) } |]

fileStoragePieceLength :: MonadIO m =>  FileStorage -> m CInt
fileStoragePieceLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(file_storage * hoPtr)->piece_length() } |]

fileStoragePieceSize :: MonadIO m =>  FileStorage -> CInt -> m CInt
fileStoragePieceSize ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(file_storage * hoPtr)->piece_size($(int idx)) } |]

setFileStorageName :: MonadIO m =>  FileStorage -> Text -> m ()
setFileStorageName ho n =
  liftIO . withPtr ho $ \hoPtr -> do
  sn <- textToStdString n
  withPtr sn $ \snPtr ->
    [C.exp| void { $(file_storage * hoPtr)->set_name(*$(string * snPtr)) } |]

fileStorageName :: MonadIO m =>  FileStorage -> m Text
fileStorageName ho =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [C.exp| string * { new std::string($(file_storage * hoPtr)->name()) } |]
  stdStringToText str

fileStorageOptimize :: MonadIO m =>  FileStorage -> m ()
fileStorageOptimize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(file_storage * hoPtr)->optimize() } |]

fileSize :: MonadIO m =>  FileStorage -> CInt -> m C.CSize
fileSize ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| size_t { $(file_storage * hoPtr)->file_size($(int idx)) } |]

fileStorageHash :: MonadIO m =>  FileStorage -> CInt -> m ByteString
fileStorageHash ho idx =
  liftIO . withPtr ho $ \hoPtr -> do
  h <- fromPtr [C.exp| sha1_hash * { new sha1_hash($(file_storage * hoPtr)->hash($(int idx))) } |]
  sha1HashToByteString h

fileName :: MonadIO m =>  FileStorage -> CInt -> m Text
fileName ho idx =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [C.exp| string * { new std::string($(file_storage * hoPtr)->file_name($(int idx))) } |]
  stdStringToText str

fileOffset :: MonadIO m =>  FileStorage -> CInt -> m C.CSize
fileOffset ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| size_t { $(file_storage * hoPtr)->file_offset($(int idx)) } |]

fileStorageMtime :: MonadIO m =>  FileStorage -> CInt -> m C.CTime
fileStorageMtime ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| time_t { $(file_storage * hoPtr)->mtime($(int idx)) } |]

padFileAt :: MonadIO m =>  FileStorage -> CInt -> m Bool
padFileAt ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [C.exp| bool { $(file_storage * hoPtr)->pad_file_at($(int idx)) } |]

symlink :: MonadIO m =>  FileStorage -> CInt -> m Text
symlink ho idx =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [C.exp| string * { new std::string($(file_storage * hoPtr)->symlink($(int idx))) } |]
  stdStringToText str

filePath :: MonadIO m =>  FileStorage -> CInt -> m Text
filePath ho idx =
  liftIO . withPtr ho $ \hoPtr -> do
  str <- fromPtr [C.exp| string * { new std::string($(file_storage * hoPtr)->file_path($(int idx))) } |]
  stdStringToText str

fileFlags :: MonadIO m =>  FileStorage -> CInt -> m (BitFlags FileFlags)
fileFlags ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [C.exp| int { $(file_storage * hoPtr)->file_flags($(int idx)) } |]

setFileBase :: MonadIO m =>  FileStorage -> CInt -> C.CSize -> m ()
setFileBase ho idx off =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(file_storage * hoPtr)->set_file_base($(int idx), $(size_t off)) } |]

fileBase :: MonadIO m =>  FileStorage -> CInt -> m C.CSize
fileBase ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| size_t { $(file_storage * hoPtr)->file_base($(int idx)) } |]

fileIndexAtOffset :: MonadIO m =>  FileStorage -> C.CSize -> m CInt
fileIndexAtOffset ho offset =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(file_storage * hoPtr)->file_index_at_offset($(size_t offset)) } |]
