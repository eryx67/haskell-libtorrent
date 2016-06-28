{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#torrent_status torrent_status> for "Libtorrent"

module Libtorrent.TorrentHandle.TorrentStatus (TorrentState(..)
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
                                              , getInfoHash
                                              ) where


import           Data.Array.BitArray (BitArray)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types
import           Libtorrent.String
import           Libtorrent.Sha1Hash (Sha1Hash)
import           Libtorrent.Bitfield

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
  deriving (Show, Enum, Bounded)

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

getError :: TorrentStatus -> IO Text
getError ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->error) } |]
  stdStringToText res

getSavePath :: TorrentStatus -> IO Text
getSavePath ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->save_path) } |]
  stdStringToText res

getName :: TorrentStatus -> IO Text
getName ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->name) } |]
  stdStringToText res

getTorrentStatusNextAnnounce :: TorrentStatus -> IO C.CLong
getTorrentStatusNextAnnounce ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| long { $(torrent_status * hoPtr)->next_announce.total_seconds() } |]

getCurrentTracker :: TorrentStatus -> IO Text
getCurrentTracker ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(torrent_status * hoPtr)->current_tracker) } |]
  stdStringToText res

getTotalDownload :: TorrentStatus -> IO Int64
getTotalDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_download } |]

getTotalUpload :: TorrentStatus -> IO Int64
getTotalUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_upload } |]

getTotalPayloadDownload :: TorrentStatus -> IO Int64
getTotalPayloadDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_payload_download } |]

getTotalPayloadUpload :: TorrentStatus -> IO Int64
getTotalPayloadUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_payload_upload } |]

getTotalFailedBytes :: TorrentStatus -> IO Int64
getTotalFailedBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_failed_bytes } |]

getTotalRedundantBytes :: TorrentStatus -> IO Int64
getTotalRedundantBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_redundant_bytes } |]

getPieces :: TorrentStatus -> IO (BitArray Int)
getPieces ho =
  withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(torrent_status * hoPtr)->pieces) } |]
  bitfieldToBitArray bf

getVerifiedPieces :: TorrentStatus -> IO (BitArray Int)
getVerifiedPieces ho =
  withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(torrent_status * hoPtr)->verified_pieces) } |]
  bitfieldToBitArray bf

getTotalDone :: TorrentStatus -> IO Int64
getTotalDone ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_done } |]

getTotalWantedDone :: TorrentStatus -> IO Int64
getTotalWantedDone ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_wanted_done } |]

getTotalWanted :: TorrentStatus -> IO Int64
getTotalWanted ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->total_wanted } |]

getAllTimeUpload :: TorrentStatus -> IO Int64
getAllTimeUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->all_time_upload } |]

getAllTimeDownload :: TorrentStatus -> IO Int64
getAllTimeDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(torrent_status * hoPtr)->all_time_download } |]

getAddedTime :: TorrentStatus -> IO C.CTime
getAddedTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->added_time } |]

getCompletedTime :: TorrentStatus -> IO C.CTime
getCompletedTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->completed_time } |]

getLastSeenComplete :: TorrentStatus -> IO C.CTime
getLastSeenComplete ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| time_t { $(torrent_status * hoPtr)->last_seen_complete } |]

getProgress :: TorrentStatus -> IO C.CFloat
getProgress ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| float { $(torrent_status * hoPtr)->progress } |]

getProgressPpm :: TorrentStatus -> IO CInt
getProgressPpm ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->progress_ppm } |]

getQueuePosition :: TorrentStatus -> IO CInt
getQueuePosition ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->queue_position } |]

getDownloadRate :: TorrentStatus -> IO CInt
getDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->download_rate } |]

getUploadRate :: TorrentStatus -> IO CInt
getUploadRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_status * hoPtr)->upload_rate } |]

getDownloadPayloadRate :: TorrentStatus -> IO CInt
getDownloadPayloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->download_payload_rate } |]

getUploadPayloadRate :: TorrentStatus -> IO CInt
getUploadPayloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->upload_payload_rate } |]

getNumSeeds :: TorrentStatus -> IO CInt
getNumSeeds ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_seeds } |]

getNumPeers :: TorrentStatus -> IO CInt
getNumPeers ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(torrent_status * hoPtr)->num_peers } |]

getNumComplete :: TorrentStatus -> IO CInt
getNumComplete ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_complete } |]

getNumIncomplete :: TorrentStatus -> IO CInt
getNumIncomplete ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_incomplete } |]

getListSeeds :: TorrentStatus -> IO CInt
getListSeeds ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->list_seeds } |]

getListPeers :: TorrentStatus -> IO CInt
getListPeers ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->list_peers } |]

getConnectCandidates :: TorrentStatus -> IO CInt
getConnectCandidates ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->connect_candidates } |]

getNumPieces :: TorrentStatus -> IO CInt
getNumPieces ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_pieces } |]

getDistributedFullCopies :: TorrentStatus -> IO CInt
getDistributedFullCopies ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->distributed_full_copies } |]

getDistributedFraction :: TorrentStatus -> IO CInt
getDistributedFraction ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->distributed_fraction } |]

getDistributedCopies :: TorrentStatus -> IO C.CFloat
getDistributedCopies ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| float { $(torrent_status * hoPtr)->distributed_copies } |]

getBlockSize :: TorrentStatus -> IO CInt
getBlockSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->block_size } |]

getNumUploads :: TorrentStatus -> IO CInt
getNumUploads ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_uploads } |]

getNumConnections :: TorrentStatus -> IO CInt
getNumConnections ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->num_connections } |]

getUploadsLimit :: TorrentStatus -> IO CInt
getUploadsLimit ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->uploads_limit } |]

getConnectionsLimit :: TorrentStatus -> IO CInt
getConnectionsLimit ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->connections_limit } |]

getUpBandwidthQueue :: TorrentStatus -> IO CInt
getUpBandwidthQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->up_bandwidth_queue } |]

getDownBandwidthQueue :: TorrentStatus -> IO CInt
getDownBandwidthQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->down_bandwidth_queue } |]

getTimeSinceUpload :: TorrentStatus -> IO CInt
getTimeSinceUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->time_since_upload } |]

getTimeSinceDownload :: TorrentStatus -> IO CInt
getTimeSinceDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->time_since_download } |]

getActiveTime :: TorrentStatus -> IO CInt
getActiveTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->active_time } |]

getFinishedTime :: TorrentStatus -> IO CInt
getFinishedTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->finished_time } |]

getSeedingTime :: TorrentStatus -> IO CInt
getSeedingTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->seeding_time } |]

getSeedRank :: TorrentStatus -> IO CInt
getSeedRank ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->seed_rank } |]

getLastScrape :: TorrentStatus -> IO CInt
getLastScrape ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->last_scrape } |]

getPriority :: TorrentStatus -> IO CInt
getPriority ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(torrent_status * hoPtr)->priority } |]

getState :: TorrentStatus -> IO TorrentState
getState ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(torrent_status * hoPtr)->state } |]

getNeedSaveResume :: TorrentStatus -> IO Bool
getNeedSaveResume ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->need_save_resume } |]

getIpFilterApplies :: TorrentStatus -> IO Bool
getIpFilterApplies ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->ip_filter_applies } |]

getUploadMode :: TorrentStatus -> IO Bool
getUploadMode ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->upload_mode } |]

getShareMode :: TorrentStatus -> IO Bool
getShareMode ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->share_mode } |]

getSuperSeeding :: TorrentStatus -> IO Bool
getSuperSeeding ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->super_seeding } |]

getPaused :: TorrentStatus -> IO Bool
getPaused ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->paused } |]

getAutoManaged :: TorrentStatus -> IO Bool
getAutoManaged ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->auto_managed } |]

getSequentialDownload :: TorrentStatus -> IO Bool
getSequentialDownload ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->sequential_download } |]

getIsSeeding :: TorrentStatus -> IO Bool
getIsSeeding ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_seeding } |]

getIsFinished :: TorrentStatus -> IO Bool
getIsFinished ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_finished } |]

getHasMetadata :: TorrentStatus -> IO Bool
getHasMetadata ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->has_metadata } |]

getHasIncoming :: TorrentStatus -> IO Bool
getHasIncoming ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->has_incoming } |]

getSeedMode :: TorrentStatus -> IO Bool
getSeedMode ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->seed_mode } |]

getMovingStorage :: TorrentStatus -> IO Bool
getMovingStorage ho =
  withPtr ho $ \hoPtr ->
  ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->moving_storage } |]

-- TODO: Will be available in libtorrent 1.1.0
-- getIsLoaded :: TorrentStatus -> IO Bool
-- getIsLoaded ho =
--   withPtr ho $ \hoPtr ->
--   ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->is_loaded } |]

-- getAnnouncingToTrackers :: TorrentStatus -> IO Bool
-- getAnnouncingToTrackers ho =
--   withPtr ho $ \hoPtr ->
--   ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_trackers } |]

-- getAnnouncingToLsd :: TorrentStatus -> IO Bool
-- getAnnouncingToLsd ho =
--   withPtr ho $ \hoPtr ->
--   ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_lsd } |]

-- getAnnouncingToDht :: TorrentStatus -> IO Bool
-- getAnnouncingToDht ho =
--   withPtr ho $ \hoPtr ->
--   ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->announcing_to_dht } |]

-- getStopWhenReady :: TorrentStatus -> IO Bool
-- getStopWhenReady ho =
--   withPtr ho $ \hoPtr ->
--   ( > 0) <$> [CU.exp| bool { $(torrent_status * hoPtr)->stop_when_ready } |]

getInfoHash :: TorrentStatus -> IO Sha1Hash
getInfoHash ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_status * hoPtr)->info_hash) } |]


