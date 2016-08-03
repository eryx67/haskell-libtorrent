{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Create_Torrents.html#create_torrent create_torrent> structure for "Libtorrent"

module Network.Libtorrent.CreateTorrent( CreateTorrent(..)
                               , createTorrent
                               , createTorrentFromTorrentInfo
                               , createTorrentGenerate
                               , createTorrentFiles
                               , createTorrentSetComment
                               , createTorrentSetCreator
                               , createTorrentSetHash
                               , createTorrentSetFileHash
                               , createTorrentAddUrlSeed
                               , createTorrentAddHttpSeed
                               , createTorrentAddNode
                               , createTorrentAddTracker
                               , createTorrentSetRootCert
                               , createTorrentPriv
                               , createTorrentSetPriv
                               , createTorrentNumPieces
                               , createTorrentPieceLength
                               , createTorrentPieceSize
                               , createTorrentMerkleTree
                               , createTorrentSetPieceHashes
                               ) where

import           Control.Monad                  (void)
import           Control.Monad.Catch            (bracket)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Foreign.C.String               (withCString)
import           Foreign.ForeignPtr             (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils          (fromBool, toBool)
import           Foreign.Ptr                    (freeHaskellFunPtr)
import qualified Language.C.Inline              as C
import qualified Language.C.Inline.Cpp          as C
import           System.Mem.Weak                (mkWeak)


import           Network.Libtorrent.Bencode
import           Network.Libtorrent.Exceptions
import           Network.Libtorrent.FileStorage (FileStorage)
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Sha1Hash
import           Network.Libtorrent.String
import           Network.Libtorrent.TorrentInfo (TorrentInfo)
import           Network.Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/create_torrent.hpp>"

C.verbatim "typedef std::vector<libtorrent::sha1_hash> VectorSha1Hash;"
C.verbatim "typedef void SetPieceHashesCb (int);"

C.using "namespace libtorrent"
C.using "namespace std"

data CreateTorrentFlags =
      Optimize
      | Merkle
      | ModificationTime
      | Symlinks
      | CalculateFileHashes
  deriving (Show, Enum, Bounded, Eq, Ord)

newtype CreateTorrent = CreateTorrent { unCreateTorrent :: ForeignPtr (CType CreateTorrent) }

instance Show CreateTorrent where
  show _ = "CreateTorrent"

instance Inlinable CreateTorrent where
  type (CType CreateTorrent) = C'CreateTorrent

instance FromPtr CreateTorrent where
  fromPtr = objFromPtr CreateTorrent $ \ptr ->
    [C.exp| void { delete $(create_torrent * ptr); } |]

instance WithPtr CreateTorrent where
  withPtr (CreateTorrent fptr) = withForeignPtr fptr

createTorrent :: MonadIO m =>  FileStorage
              -> Maybe C.CInt -- ^ piece size
              -> Maybe C.CInt -- ^ pad file limit
              -> Maybe (BitFlags CreateTorrentFlags)
              -> m CreateTorrent
createTorrent fs mps mpfl mflags =
  liftIO . withPtr fs $ \fsPtr -> do
  let ps = fromMaybe 0 mps
      pfl = fromMaybe (-1) mpfl
      flags = fromIntegral . fromEnum $ fromMaybe ([Optimize]) mflags
  res <- fromPtr [C.exp| create_torrent * { new create_torrent(*$(file_storage * fsPtr)
                                                              , $(int ps)
                                                              , $(int pfl)
                                                              , $(int flags))
                   }
                |]
  -- FileStorage object must be kept during CreateTorrent life
  void $ mkWeak res fs Nothing
  return res

createTorrentFromTorrentInfo :: MonadIO m =>  TorrentInfo -> m CreateTorrent
createTorrentFromTorrentInfo ti =
  liftIO . withPtr ti $ \tiPtr -> do
  fromPtr [C.exp| create_torrent * { new create_torrent(*$(torrent_info * tiPtr)) } |]

createTorrentGenerate :: MonadIO m => CreateTorrent -> m Bencoded
createTorrentGenerate ho =
  liftIO . withPtr ho $ \hoPtr -> do
  entry <- [C.exp| entry * { new entry($(create_torrent * hoPtr)->generate()) } |]
  entryToBencoded entry

createTorrentFiles :: MonadIO m => CreateTorrent -> m FileStorage
createTorrentFiles ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| file_storage * { new file_storage($(create_torrent * hoPtr)->files()) } |]

createTorrentSetComment :: MonadIO m => CreateTorrent -> Text -> m ()
createTorrentSetComment ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withCString (T.unpack val) $ \ valPtr ->
  [C.exp| void { $(create_torrent * hoPtr)->set_comment($(char const * valPtr)) } |]

createTorrentSetCreator :: MonadIO m => CreateTorrent -> Text -> m ()
createTorrentSetCreator ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withCString (T.unpack val) $ \ valPtr ->
  [C.exp| void { $(create_torrent * hoPtr)->set_creator($(char const * valPtr)) } |]

createTorrentSetHash :: MonadIO m => CreateTorrent -> C.CInt -> Sha1Hash -> m ()
createTorrentSetHash ho idx h =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr h $ \hPtr ->
  [C.exp| void { $(create_torrent * hoPtr)->set_hash($(int idx), *$(sha1_hash * hPtr)) } |]

createTorrentSetFileHash :: MonadIO m => CreateTorrent -> C.CInt -> Sha1Hash -> m ()
createTorrentSetFileHash ho idx h =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr h $ \hPtr ->
  [C.exp| void { $(create_torrent * hoPtr)->set_file_hash($(int idx), *$(sha1_hash * hPtr)) } |]

createTorrentAddUrlSeed :: MonadIO m => CreateTorrent -> Text -> m ()
createTorrentAddUrlSeed ho url =
  liftIO . withPtr ho $ \hoPtr -> do
  url' <- textToStdString url
  withPtr url' $ \urlPtr ->
    [C.exp| void { $(create_torrent * hoPtr)->add_url_seed(*$(string * urlPtr)) } |]

createTorrentAddHttpSeed :: MonadIO m => CreateTorrent -> Text -> m ()
createTorrentAddHttpSeed ho url =
  liftIO . withPtr ho $ \hoPtr -> do
  url' <- textToStdString url
  withPtr url' $ \urlPtr ->
    [C.exp| void { $(create_torrent * hoPtr)->add_http_seed(*$(string * urlPtr)) } |]

createTorrentAddNode :: MonadIO m => CreateTorrent -> (Text, C.CInt) -> m ()
createTorrentAddNode ho (txt, i) =
  liftIO . withPtr ho $ \hoPtr -> do
  txt' <- textToStdString txt
  withPtr txt' $ \txtPtr ->
    [C.exp| void { $(create_torrent * hoPtr)->add_node(pair<string, int>(*$(string * txtPtr), $(int i))) } |]

createTorrentAddTracker :: MonadIO m => CreateTorrent -> Text -> Maybe C.CInt -> m ()
createTorrentAddTracker ho url mtier =
  liftIO . withPtr ho $ \hoPtr -> do
  let tier = fromMaybe 0 mtier
  url' <- textToStdString url
  withPtr url' $ \urlPtr ->
    [C.exp| void { $(create_torrent * hoPtr)->add_tracker(*$(string * urlPtr), $(int tier)) } |]

createTorrentSetRootCert :: MonadIO m => CreateTorrent -> Text -> m ()
createTorrentSetRootCert ho pem =
  liftIO . withPtr ho $ \hoPtr -> do
  pem' <- textToStdString pem
  withPtr pem' $ \pemPtr ->
    [C.exp| void { $(create_torrent * hoPtr)->set_root_cert(*$(string * pemPtr)) } |]

createTorrentPriv :: MonadIO m => CreateTorrent -> m Bool
createTorrentPriv ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [C.exp| bool { $(create_torrent * hoPtr)->priv() } |]

createTorrentSetPriv :: MonadIO m => CreateTorrent -> Bool -> m ()
createTorrentSetPriv ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [C.exp| void { $(create_torrent * hoPtr)->set_priv($(bool val')) } |]

createTorrentNumPieces :: MonadIO m => CreateTorrent -> m C.CInt
createTorrentNumPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(create_torrent * hoPtr)->num_pieces() } |]

createTorrentPieceLength :: MonadIO m => CreateTorrent -> m C.CInt
createTorrentPieceLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(create_torrent * hoPtr)->piece_length() } |]

createTorrentPieceSize :: MonadIO m => CreateTorrent -> C.CInt -> m C.CInt
createTorrentPieceSize ho idx =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| int { $(create_torrent * hoPtr)->piece_size($(int idx)) } |]

createTorrentMerkleTree :: MonadIO m => CreateTorrent -> m (StdVector Sha1Hash)
createTorrentMerkleTree ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| VectorSha1Hash * { new VectorSha1Hash($(create_torrent * hoPtr)->merkle_tree()) } |]

-- | Can throw 'LibtorrentException'.
createTorrentSetPieceHashes :: MonadIO m => CreateTorrent -> Text -> (C.CInt -> IO ()) -> m ()
createTorrentSetPieceHashes ho fp cb =
  liftIO . withPtr ho $ \hoPtr -> do
  fpStr <- textToStdString fp
  withPtr fpStr $ \fpPtr ->
    withErrorCode CreateTorrentError $ \ePtr ->
    bracket
    ($(C.mkFunPtr [t| C.CInt -> IO () |]) cb)
    freeHaskellFunPtr $ \cbPtr ->
    [C.block| void {
         boost::function<void(int)> cb = $(SetPieceHashesCb cbPtr);
        set_piece_hashes(*$(create_torrent * hoPtr), *$(string * fpPtr), cb, *$(error_code * ePtr)) ;
      }
    |]

