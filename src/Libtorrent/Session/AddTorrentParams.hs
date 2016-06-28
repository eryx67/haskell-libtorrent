{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | The 'AddTorrentParams' is a parameter pack for adding torrents to
-- a 'Libtorrent.Session'.
-- See <http://www.libtorrent.org/reference-Core.html#add-torrent-params add_torrent_params>.

module Libtorrent.Session.AddTorrentParams (AddTorrentParams
                                           , TorrentSrc(..)
                                           , AddTorrentFlags(..)
                                           , unAddTorrentParams
                                           , newAddTorrentParams
                                           , setFlags
                                           , setName
                                           , setSavePath
                                           ) where

import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types
import           Libtorrent.TorrentInfo
import           Libtorrent.Sha1Hash (Sha1Hash)

C.context libtorrentCtx

C.include "<libtorrent/session.hpp>"
C.include "<libtorrent/add_torrent_params.hpp>"

C.using "namespace libtorrent"

-- | Torrent source for 'AddTorrentParams'
data TorrentSrc =
  TorrentInfoSrc !TorrentInfo
  | UrlSrc !Text
  | InfoHashSrc !Sha1Hash
                deriving Show

-- | Flags for <http://www.libtorrent.org/reference-Core.html#add-torrent-params add_torrent_params>
data AddTorrentFlags =
  FlagSeedMode
  | FlagOverrideResumeData
  | FlagUploadMode
  | FlagShareMode
  | FlagApplyIpFilter
  | FlagPaused
  | FlagAutoManaged
  | FlagDuplicateIsError
  | FlagMergeResumeTrackers
  | FlagUpdateSubscribe
  | FlagSuperSeeding
  | FlagSequentialDownload
  | FlagUseResumeSavePath
  | FlagPinned
  | FlagMergeResumeHttpSeeds
  | FlagStopWhenReady
  deriving (Show, Enum, Bounded)

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
newAddTorrentParams :: TorrentSrc -> IO AddTorrentParams
newAddTorrentParams ts = do
  atp <- fromPtr [CU.exp| add_torrent_params * { new add_torrent_params() } |]
  withPtr atp $ addTorrent ts
  return atp
  where
    addTorrent (TorrentInfoSrc ti) atPtr =
      withPtr ti $ \tiPtr ->
      [CU.block| void {
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

setFlags :: AddTorrentParams -> BitFlags AddTorrentFlags -> IO ()
setFlags atp flags =
  withPtr atp $ \atPtr -> do
  let flags' = fromIntegral $ fromEnum flags
  [CU.exp| void { $(add_torrent_params * atPtr)->flags = $(uint64_t flags')} |]

setName :: AddTorrentParams -> Text -> IO ()
setName atp nm =
  withPtr atp $ \atPtr -> do
  TF.withCStringLen nm $ \(ptr, len) -> do
    let csize = fromIntegral len
    [CU.block| void {
        std::string val($(const char * ptr), $(size_t csize));
        $(add_torrent_params * atPtr)->name = val;
      }
    |]

setSavePath :: AddTorrentParams -> Text -> IO ()
setSavePath atp path =
  withPtr atp $ \atPtr -> do
  TF.withCStringLen path $ \(ptr, len) -> do
    let csize = fromIntegral len
    [CU.block| void {
        std::string val($(const char * ptr), $(size_t csize));
        $(add_torrent_params * atPtr)->save_path = val;
      }
    |]
