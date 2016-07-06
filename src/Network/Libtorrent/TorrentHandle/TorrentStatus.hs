{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent_status torrent_status> for "Libtorrent"

module Network.Libtorrent.TorrentHandle.TorrentStatus (TorrentState(..)
                                              , TorrentStatus(..)
                                              , getError
                                              , getSavePath
                                              , getName
                                              , getTorrentStatusNextAnnounce
                                              , getCurrentTracker
                                              , getTotalDownload
                                              , getTotalUpload
                                              , getTotalPayloadDownload
                                              , getTotalPayloadUpload
                                              , getTotalFailedBytes
                                              , getTotalRedundantBytes
                                              , getPieces
                                              , getVerifiedPieces
                                              , getTotalDone
                                              , getTotalWantedDone
                                              , getTotalWanted
                                              , getAllTimeUpload
                                              , getAllTimeDownload
                                              , getAddedTime
                                              , getCompletedTime
                                              , getLastSeenComplete
                                              , getProgress
                                              , getProgressPpm
                                              , getQueuePosition
                                              , getDownloadRate
                                              , getUploadRate
                                              , getDownloadPayloadRate
                                              , getUploadPayloadRate
                                              , getNumSeeds
                                              , getNumPeers
                                              , getNumComplete
                                              , getNumIncomplete
                                              , getListSeeds
                                              , getListPeers
                                              , getConnectCandidates
                                              , getNumPieces
                                              , getDistributedFullCopies
                                              , getDistributedFraction
                                              , getDistributedCopies
                                              , getBlockSize
                                              , getNumUploads
                                              , getNumConnections
                                              , getUploadsLimit
                                              , getConnectionsLimit
                                              , getUpBandwidthQueue
                                              , getDownBandwidthQueue
                                              , getTimeSinceUpload
                                              , getTimeSinceDownload
                                              , getActiveTime
                                              , getFinishedTime
                                              , getSeedingTime
                                              , getSeedRank
                                              , getLastScrape
                                              , getPriority
                                              , getState
                                              , getNeedSaveResume
                                              , getIpFilterApplies
                                              , getUploadMode
                                              , getShareMode
                                              , getSuperSeeding
                                              , getPaused
                                              , getAutoManaged
                                              , getSequentialDownload
                                              , getIsSeeding
                                              , getIsFinished
                                              , getHasMetadata
                                              , getHasIncoming
                                              , getSeedMode
                                              , getMovingStorage
                                              , getTorrentStatusInfoHash
                                              ) where


import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Array.BitArray (BitArray)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types
import           Network.Libtorrent.String
import           Network.Libtorrent.Sha1Hash (Sha1Hash)
import           Network.Libtorrent.Bitfield

C.context libtorrentCtx

C.include "<libtorrent/torrent_handle.hpp>"

C.include "torrent_handle.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

data TorrentState =
  QueuedForChecking
  | CheckingFiles
  | DownloadingMetadata
  | Downloading
  | Finished
  | Seeding
  | Allocating
  | CheckingResumeData
  deriving (Show, Enum, Bounded, Eq)

newtype TorrentStatus = TorrentStatus { unTorrentStatus :: ForeignPtr (CType TorrentStatus)}

instance Show TorrentStatus where
  show _ = "TorrentStatus"

instance Inlinable TorrentStatus where
  type (CType TorrentStatus) = C'TorrentStatus

instance FromPtr TorrentStatus where
  fromPtr = objFromPtr TorrentStatus $ \ptr ->
    [CU.exp| void { delete $(torrent_status * ptr); } |]

instance WithPtr TorrentStatus where
  withPtr (TorrentStatus fptr) = withForeignPtr fptr

getError :: MonadIO m =>  TorrentStatus -> m Text
getError ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->error) } |]
  stdStringToText res

getSavePath :: MonadIO m =>  TorrentStatus -> m Text
getSavePath ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->save_path) } |]
  stdStringToText res

getName :: MonadIO m =>  TorrentStatus -> m Text
getName ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->name) } |]
  stdStringToText res

getTorrentStatusNextAnnounce :: MonadIO m =>  TorrentStatus -> m C.CLong
getTorrentStatusNextAnnounce ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| long { $(torrent_status * hoPtr)->next_announce.total_seconds() } |]

getCurrentTracker :: MonadIO m =>  TorrentStatus -> m Text
getCurrentTracker ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->current_tracker) } |]
  stdStringToText res

getTotalDownload :: MonadIO m =>  TorrentStatus -> m Int64
getTotalDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_download } |]

getTotalUpload :: MonadIO m =>  TorrentStatus -> m Int64
getTotalUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_upload } |]

getTotalPayloadDownload :: MonadIO m =>  TorrentStatus -> m Int64
getTotalPayloadDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_payload_download } |]

getTotalPayloadUpload :: MonadIO m =>  TorrentStatus -> m Int64
getTotalPayloadUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_payload_upload } |]

getTotalFailedBytes :: MonadIO m =>  TorrentStatus -> m Int64
getTotalFailedBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_failed_bytes } |]

getTotalRedundantBytes :: MonadIO m =>  TorrentStatus -> m Int64
getTotalRedundantBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_redundant_bytes } |]

getPieces :: MonadIO m =>  TorrentStatus -> m (BitArray Int)
getPieces ho =
  liftIO . withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(torrent_status * hoPtr)->pieces) } |]
  bitfieldToBitArray bf

getVerifiedPieces :: MonadIO m =>  TorrentStatus -> m (BitArray Int)
getVerifiedPieces ho =
  liftIO . withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(torrent_status * hoPtr)->verified_pieces) } |]
  bitfieldToBitArray bf

getTotalDone :: MonadIO m =>  TorrentStatus -> m Int64
getTotalDone ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_done } |]

getTotalWantedDone :: MonadIO m =>  TorrentStatus -> m Int64
getTotalWantedDone ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_wanted_done } |]

getTotalWanted :: MonadIO m =>  TorrentStatus -> m Int64
getTotalWanted ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_wanted } |]

getAllTimeUpload :: MonadIO m =>  TorrentStatus -> m Int64
getAllTimeUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->all_time_upload } |]

getAllTimeDownload :: MonadIO m =>  TorrentStatus -> m Int64
getAllTimeDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->all_time_download } |]

getAddedTime :: MonadIO m =>  TorrentStatus -> m C.CTime
getAddedTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->added_time } |]

getCompletedTime :: MonadIO m =>  TorrentStatus -> m C.CTime
getCompletedTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->completed_time } |]

getLastSeenComplete :: MonadIO m =>  TorrentStatus -> m C.CTime
getLastSeenComplete ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->last_seen_complete } |]

getProgress :: MonadIO m =>  TorrentStatus -> m C.CFloat
getProgress ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| float { $(torrent_status * hoPtr)->progress } |]

getProgressPpm :: MonadIO m =>  TorrentStatus -> m CInt
getProgressPpm ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->progress_ppm } |]

getQueuePosition :: MonadIO m =>  TorrentStatus -> m CInt
getQueuePosition ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->queue_position } |]

getDownloadRate :: MonadIO m =>  TorrentStatus -> m CInt
getDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->download_rate } |]

getUploadRate :: MonadIO m =>  TorrentStatus -> m CInt
getUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_status * hoPtr)->upload_rate } |]

getDownloadPayloadRate :: MonadIO m =>  TorrentStatus -> m CInt
getDownloadPayloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->download_payload_rate } |]

getUploadPayloadRate :: MonadIO m =>  TorrentStatus -> m CInt
getUploadPayloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->upload_payload_rate } |]

getNumSeeds :: MonadIO m =>  TorrentStatus -> m CInt
getNumSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_seeds } |]

getNumPeers :: MonadIO m =>  TorrentStatus -> m CInt
getNumPeers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_status * hoPtr)->num_peers } |]

getNumComplete :: MonadIO m =>  TorrentStatus -> m CInt
getNumComplete ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_complete } |]

getNumIncomplete :: MonadIO m =>  TorrentStatus -> m CInt
getNumIncomplete ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_incomplete } |]

getListSeeds :: MonadIO m =>  TorrentStatus -> m CInt
getListSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->list_seeds } |]

getListPeers :: MonadIO m =>  TorrentStatus -> m CInt
getListPeers ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->list_peers } |]

getConnectCandidates :: MonadIO m =>  TorrentStatus -> m CInt
getConnectCandidates ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->connect_candidates } |]

getNumPieces :: MonadIO m =>  TorrentStatus -> m CInt
getNumPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_pieces } |]

getDistributedFullCopies :: MonadIO m =>  TorrentStatus -> m CInt
getDistributedFullCopies ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->distributed_full_copies } |]

getDistributedFraction :: MonadIO m =>  TorrentStatus -> m CInt
getDistributedFraction ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->distributed_fraction } |]

getDistributedCopies :: MonadIO m =>  TorrentStatus -> m C.CFloat
getDistributedCopies ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| float { $(torrent_status * hoPtr)->distributed_copies } |]

getBlockSize :: MonadIO m =>  TorrentStatus -> m CInt
getBlockSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->block_size } |]

getNumUploads :: MonadIO m =>  TorrentStatus -> m CInt
getNumUploads ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_uploads } |]

getNumConnections :: MonadIO m =>  TorrentStatus -> m CInt
getNumConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_connections } |]

getUploadsLimit :: MonadIO m =>  TorrentStatus -> m CInt
getUploadsLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->uploads_limit } |]

getConnectionsLimit :: MonadIO m =>  TorrentStatus -> m CInt
getConnectionsLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->connections_limit } |]

getUpBandwidthQueue :: MonadIO m =>  TorrentStatus -> m CInt
getUpBandwidthQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->up_bandwidth_queue } |]

getDownBandwidthQueue :: MonadIO m =>  TorrentStatus -> m CInt
getDownBandwidthQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->down_bandwidth_queue } |]

getTimeSinceUpload :: MonadIO m =>  TorrentStatus -> m CInt
getTimeSinceUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->time_since_upload } |]

getTimeSinceDownload :: MonadIO m =>  TorrentStatus -> m CInt
getTimeSinceDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->time_since_download } |]

getActiveTime :: MonadIO m =>  TorrentStatus -> m CInt
getActiveTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->active_time } |]

getFinishedTime :: MonadIO m =>  TorrentStatus -> m CInt
getFinishedTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->finished_time } |]

getSeedingTime :: MonadIO m =>  TorrentStatus -> m CInt
getSeedingTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->seeding_time } |]

getSeedRank :: MonadIO m =>  TorrentStatus -> m CInt
getSeedRank ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->seed_rank } |]

getLastScrape :: MonadIO m =>  TorrentStatus -> m CInt
getLastScrape ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->last_scrape } |]

getPriority :: MonadIO m =>  TorrentStatus -> m CInt
getPriority ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->priority } |]

getState :: MonadIO m =>  TorrentStatus -> m TorrentState
getState ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(torrent_status * hoPtr)->state } |]

getNeedSaveResume :: MonadIO m =>  TorrentStatus -> m Bool
getNeedSaveResume ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->need_save_resume } |]

getIpFilterApplies :: MonadIO m =>  TorrentStatus -> m Bool
getIpFilterApplies ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->ip_filter_applies } |]

getUploadMode :: MonadIO m =>  TorrentStatus -> m Bool
getUploadMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->upload_mode } |]

getShareMode :: MonadIO m =>  TorrentStatus -> m Bool
getShareMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->share_mode } |]

getSuperSeeding :: MonadIO m =>  TorrentStatus -> m Bool
getSuperSeeding ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->super_seeding } |]

getPaused :: MonadIO m =>  TorrentStatus -> m Bool
getPaused ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->paused } |]

getAutoManaged :: MonadIO m =>  TorrentStatus -> m Bool
getAutoManaged ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->auto_managed } |]

getSequentialDownload :: MonadIO m =>  TorrentStatus -> m Bool
getSequentialDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->sequential_download } |]

getIsSeeding :: MonadIO m =>  TorrentStatus -> m Bool
getIsSeeding ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_seeding } |]

getIsFinished :: MonadIO m =>  TorrentStatus -> m Bool
getIsFinished ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_finished } |]

getHasMetadata :: MonadIO m =>  TorrentStatus -> m Bool
getHasMetadata ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->has_metadata } |]

getHasIncoming :: MonadIO m =>  TorrentStatus -> m Bool
getHasIncoming ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->has_incoming } |]

getSeedMode :: MonadIO m =>  TorrentStatus -> m Bool
getSeedMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->seed_mode } |]

getMovingStorage :: MonadIO m =>  TorrentStatus -> m Bool
getMovingStorage ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->moving_storage } |]

-- TODO: Will be available in libtorrent 1.1.0
-- getIsLoaded :: MonadIO m =>  TorrentStatus -> m Bool
-- getIsLoaded ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_loaded } |]

-- getAnnouncingToTrackers :: MonadIO m =>  TorrentStatus -> m Bool
-- getAnnouncingToTrackers ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_trackers } |]

-- getAnnouncingToLsd :: MonadIO m =>  TorrentStatus -> m Bool
-- getAnnouncingToLsd ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_lsd } |]

-- getAnnouncingToDht :: MonadIO m =>  TorrentStatus -> m Bool
-- getAnnouncingToDht ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_dht } |]

-- getStopWhenReady :: MonadIO m =>  TorrentStatus -> m Bool
-- getStopWhenReady ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   toBool <$> [CU.exp| bool { $(torrent_status * hoPtr)->stop_when_ready } |]

getTorrentStatusInfoHash :: MonadIO m =>  TorrentStatus -> m Sha1Hash
getTorrentStatusInfoHash ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_status * hoPtr)->info_hash) } |]


