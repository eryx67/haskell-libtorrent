{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#session_status session_status> for "Libtorrent"

module Network.Libtorrent.Session.SessionStatus (SessionStatus
                                        , unSessionStatus
                                        , UtpStatus
                                        , unUtpStatus
                                        , getHasIncomingConnections
                                        , getSessionUploadRate
                                        , getSessionDownloadRate
                                        , getSessionTotalDownload
                                        , getSessionTotalUpload
                                        , getSessionPayloadUploadRate
                                        , getSessionPayloadDownloadRate
                                        , getSessionTotalPayloadDownload
                                        , getSessionTotalPayloadUpload
                                        , getIpOverheadUploadRate
                                        , getIpOverheadDownloadRate
                                        , getTotalIpOverheadDownload
                                        , getTotalIpOverheadUpload
                                        , getDhtUploadRate
                                        , getDhtDownloadRate
                                        , getTotalDhtDownload
                                        , getTotalDhtUpload
                                        , getTrackerUploadRate
                                        , getTrackerDownloadRate
                                        , getTotalTrackerDownload
                                        , getTotalTrackerUpload
                                        , getSessionTotalRedundantBytes
                                        , getSessionTotalFailedBytes
                                        , getSessionNumPeers
                                        , getNumUnchoked
                                        , getAllowedUploadSlots
                                        , getSessionUpBandwidthQueue
                                        , getSessionDownBandwidthQueue
                                        , getUpBandwidthBytesQueue
                                        , getDownBandwidthBytesQueue
                                        , getOptimisticUnchokeCounter
                                        , getUnchokeCounter
                                        , getDiskWriteQueue
                                        , getDiskReadQueue
                                        , getDhtNodes
                                        , getDhtNodeCache
                                        , getDhtTorrents
                                        , getDhtGlobalNodes
                                        , getDhtTotalAllocations
                                        , getPeerlistSize
                                        -- UtpStatus
                                        , getNumIdle
                                        , getNumSynSent
                                        , getNumConnected
                                        , getNumFinSent
                                        , getNumCloseWait
                                        , getPacketLoss
                                        , getTimeout
                                        , getPacketsIn
                                        , getPacketsOut
                                        , getFastRetransmit
                                        , getPacketResend
                                        , getSamplesAboveTarget
                                        , getSamplesBelowTarget
                                        , getPayloadPktsIn
                                        , getPayloadPktsOut
                                        , getInvalidPktsIn
                                        , getRedundantPktsIn
                                        , getUtpStats
                                        ) where


import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Word (Word64)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/session.hpp>"

C.include "session.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

newtype SessionStatus = SessionStatus { unSessionStatus :: ForeignPtr (CType SessionStatus)}

instance Show SessionStatus where
  show _ = "SessionStatus"

instance Inlinable SessionStatus where
  type (CType SessionStatus) = C'SessionStatus

instance FromPtr SessionStatus where
  fromPtr = objFromPtr SessionStatus $ \ptr ->
    [CU.exp| void { delete $(session_status * ptr); } |]

instance WithPtr SessionStatus where
  withPtr (SessionStatus fptr) = withForeignPtr fptr

newtype UtpStatus = UtpStatus { unUtpStatus :: ForeignPtr (CType UtpStatus)}

instance Show UtpStatus where
  show _ = "UtpStatus"

instance Inlinable UtpStatus where
  type (CType UtpStatus) = C'UtpStatus

instance FromPtr UtpStatus where
  fromPtr = objFromPtr UtpStatus $ \ptr ->
    [CU.exp| void { delete $(utp_status * ptr); } |]

instance WithPtr UtpStatus where
  withPtr (UtpStatus fptr) = withForeignPtr fptr

getHasIncomingConnections :: MonadIO m =>  SessionStatus -> m Bool
getHasIncomingConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session_status * hoPtr)->has_incoming_connections } |]

getSessionUploadRate :: MonadIO m =>  SessionStatus -> m CInt
getSessionUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->upload_rate } |]

getSessionDownloadRate :: MonadIO m =>  SessionStatus -> m CInt
getSessionDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->download_rate } |]

getSessionTotalDownload :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_download } |]

getSessionTotalUpload :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_upload } |]

getSessionPayloadUploadRate :: MonadIO m =>  SessionStatus -> m CInt
getSessionPayloadUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->payload_upload_rate } |]

getSessionPayloadDownloadRate :: MonadIO m =>  SessionStatus -> m CInt
getSessionPayloadDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->payload_download_rate } |]

getSessionTotalPayloadDownload :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalPayloadDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_payload_download } |]

getSessionTotalPayloadUpload :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalPayloadUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_payload_upload } |]

getIpOverheadUploadRate :: MonadIO m =>  SessionStatus -> m CInt
getIpOverheadUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->ip_overhead_upload_rate } |]

getIpOverheadDownloadRate :: MonadIO m =>  SessionStatus -> m CInt
getIpOverheadDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->ip_overhead_download_rate } |]

getTotalIpOverheadDownload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalIpOverheadDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_ip_overhead_download } |]

getTotalIpOverheadUpload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalIpOverheadUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_ip_overhead_upload } |]

getDhtUploadRate :: MonadIO m =>  SessionStatus -> m CInt
getDhtUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_upload_rate } |]

getDhtDownloadRate :: MonadIO m =>  SessionStatus -> m CInt
getDhtDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_download_rate } |]

getTotalDhtDownload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalDhtDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_dht_download } |]

getTotalDhtUpload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalDhtUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_dht_upload } |]

getTrackerUploadRate :: MonadIO m =>  SessionStatus -> m CInt
getTrackerUploadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->tracker_upload_rate } |]

getTrackerDownloadRate :: MonadIO m =>  SessionStatus -> m CInt
getTrackerDownloadRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->tracker_download_rate } |]

getTotalTrackerDownload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalTrackerDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_tracker_download } |]

getTotalTrackerUpload :: MonadIO m =>  SessionStatus -> m C.CSize
getTotalTrackerUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_tracker_upload } |]

getSessionTotalRedundantBytes :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalRedundantBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_redundant_bytes } |]

getSessionTotalFailedBytes :: MonadIO m =>  SessionStatus -> m C.CSize
getSessionTotalFailedBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_failed_bytes } |]

getSessionNumPeers :: MonadIO m =>  SessionStatus -> m CInt
getSessionNumPeers ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->num_peers } |]

getNumUnchoked :: MonadIO m =>  SessionStatus -> m CInt
getNumUnchoked ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->num_unchoked } |]

getAllowedUploadSlots :: MonadIO m =>  SessionStatus -> m CInt
getAllowedUploadSlots ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->allowed_upload_slots } |]

getSessionUpBandwidthQueue :: MonadIO m =>  SessionStatus -> m CInt
getSessionUpBandwidthQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->up_bandwidth_queue } |]

getSessionDownBandwidthQueue :: MonadIO m =>  SessionStatus -> m CInt
getSessionDownBandwidthQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->down_bandwidth_queue } |]

getUpBandwidthBytesQueue :: MonadIO m =>  SessionStatus -> m CInt
getUpBandwidthBytesQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->up_bandwidth_bytes_queue } |]

getDownBandwidthBytesQueue :: MonadIO m =>  SessionStatus -> m CInt
getDownBandwidthBytesQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->down_bandwidth_bytes_queue } |]

getOptimisticUnchokeCounter :: MonadIO m =>  SessionStatus -> m CInt
getOptimisticUnchokeCounter ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->optimistic_unchoke_counter } |]

getUnchokeCounter :: MonadIO m =>  SessionStatus -> m CInt
getUnchokeCounter ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->unchoke_counter } |]

getDiskWriteQueue :: MonadIO m =>  SessionStatus -> m CInt
getDiskWriteQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->disk_write_queue } |]

getDiskReadQueue :: MonadIO m =>  SessionStatus -> m CInt
getDiskReadQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->disk_read_queue } |]

getDhtNodes :: MonadIO m =>  SessionStatus -> m CInt
getDhtNodes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_nodes } |]

getDhtNodeCache :: MonadIO m =>  SessionStatus -> m CInt
getDhtNodeCache ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_node_cache } |]

getDhtTorrents :: MonadIO m =>  SessionStatus -> m CInt
getDhtTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_torrents } |]

getDhtGlobalNodes :: MonadIO m =>  SessionStatus -> m C.CSize
getDhtGlobalNodes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->dht_global_nodes } |]

-- TODO:
-- std::vector<dht_lookup> active_requests;
-- std::vector<dht_routing_bucket> dht_routing_table;

getDhtTotalAllocations :: MonadIO m =>  SessionStatus -> m CInt
getDhtTotalAllocations ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_total_allocations } |]


getUtpStats :: MonadIO m =>  SessionStatus -> m UtpStatus
getUtpStats ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| utp_status * { new utp_status($(session_status * hoPtr)->utp_stats) } |]

getPeerlistSize :: MonadIO m =>  SessionStatus -> m CInt
getPeerlistSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->peerlist_size } |]

-- Utp status
getNumIdle :: MonadIO m =>  UtpStatus -> m CInt
getNumIdle ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_idle } |]

getNumSynSent :: MonadIO m =>  UtpStatus -> m CInt
getNumSynSent ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_syn_sent } |]

getNumConnected :: MonadIO m =>  UtpStatus -> m CInt
getNumConnected ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_connected } |]

getNumFinSent :: MonadIO m =>  UtpStatus -> m CInt
getNumFinSent ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_fin_sent } |]

getNumCloseWait :: MonadIO m =>  UtpStatus -> m CInt
getNumCloseWait ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_close_wait } |]

getPacketLoss :: MonadIO m =>  UtpStatus -> m Word64
getPacketLoss ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packet_loss } |]

getTimeout :: MonadIO m =>  UtpStatus -> m Word64
getTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->timeout } |]

getPacketsIn :: MonadIO m =>  UtpStatus -> m Word64
getPacketsIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packets_in } |]

getPacketsOut :: MonadIO m =>  UtpStatus -> m Word64
getPacketsOut ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packets_out } |]

getFastRetransmit :: MonadIO m =>  UtpStatus -> m Word64
getFastRetransmit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->fast_retransmit } |]

getPacketResend :: MonadIO m =>  UtpStatus -> m Word64
getPacketResend ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packet_resend } |]

getSamplesAboveTarget :: MonadIO m =>  UtpStatus -> m Word64
getSamplesAboveTarget ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->samples_above_target } |]

getSamplesBelowTarget :: MonadIO m =>  UtpStatus -> m Word64
getSamplesBelowTarget ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->samples_below_target } |]

getPayloadPktsIn :: MonadIO m =>  UtpStatus -> m Word64
getPayloadPktsIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->payload_pkts_in } |]

getPayloadPktsOut :: MonadIO m =>  UtpStatus -> m Word64
getPayloadPktsOut ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->payload_pkts_out } |]

getInvalidPktsIn :: MonadIO m =>  UtpStatus -> m Word64
getInvalidPktsIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->invalid_pkts_in } |]

getRedundantPktsIn :: MonadIO m =>  UtpStatus -> m Word64
getRedundantPktsIn ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->redundant_pkts_in } |]

