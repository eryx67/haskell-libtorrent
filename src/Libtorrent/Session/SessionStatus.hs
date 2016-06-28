{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#session_status session_status> for "Libtorrent"

module Libtorrent.Session.SessionStatus (SessionStatus
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


import           Data.Word (Word64)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types


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

getHasIncomingConnections :: SessionStatus -> IO Bool
getHasIncomingConnections ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session_status * hoPtr)->has_incoming_connections } |]

getSessionUploadRate :: SessionStatus -> IO CInt
getSessionUploadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->upload_rate } |]

getSessionDownloadRate :: SessionStatus -> IO CInt
getSessionDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->download_rate } |]

getSessionTotalDownload :: SessionStatus -> IO C.CSize
getSessionTotalDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_download } |]

getSessionTotalUpload :: SessionStatus -> IO C.CSize
getSessionTotalUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_upload } |]

getSessionPayloadUploadRate :: SessionStatus -> IO CInt
getSessionPayloadUploadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->payload_upload_rate } |]

getSessionPayloadDownloadRate :: SessionStatus -> IO CInt
getSessionPayloadDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->payload_download_rate } |]

getSessionTotalPayloadDownload :: SessionStatus -> IO C.CSize
getSessionTotalPayloadDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_payload_download } |]

getSessionTotalPayloadUpload :: SessionStatus -> IO C.CSize
getSessionTotalPayloadUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_payload_upload } |]

getIpOverheadUploadRate :: SessionStatus -> IO CInt
getIpOverheadUploadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->ip_overhead_upload_rate } |]

getIpOverheadDownloadRate :: SessionStatus -> IO CInt
getIpOverheadDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->ip_overhead_download_rate } |]

getTotalIpOverheadDownload :: SessionStatus -> IO C.CSize
getTotalIpOverheadDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_ip_overhead_download } |]

getTotalIpOverheadUpload :: SessionStatus -> IO C.CSize
getTotalIpOverheadUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_ip_overhead_upload } |]

getDhtUploadRate :: SessionStatus -> IO CInt
getDhtUploadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_upload_rate } |]

getDhtDownloadRate :: SessionStatus -> IO CInt
getDhtDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_download_rate } |]

getTotalDhtDownload :: SessionStatus -> IO C.CSize
getTotalDhtDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_dht_download } |]

getTotalDhtUpload :: SessionStatus -> IO C.CSize
getTotalDhtUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_dht_upload } |]

getTrackerUploadRate :: SessionStatus -> IO CInt
getTrackerUploadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->tracker_upload_rate } |]

getTrackerDownloadRate :: SessionStatus -> IO CInt
getTrackerDownloadRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->tracker_download_rate } |]

getTotalTrackerDownload :: SessionStatus -> IO C.CSize
getTotalTrackerDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_tracker_download } |]

getTotalTrackerUpload :: SessionStatus -> IO C.CSize
getTotalTrackerUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_tracker_upload } |]

getSessionTotalRedundantBytes :: SessionStatus -> IO C.CSize
getSessionTotalRedundantBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_redundant_bytes } |]

getSessionTotalFailedBytes :: SessionStatus -> IO C.CSize
getSessionTotalFailedBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->total_failed_bytes } |]

getSessionNumPeers :: SessionStatus -> IO CInt
getSessionNumPeers ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->num_peers } |]

getNumUnchoked :: SessionStatus -> IO CInt
getNumUnchoked ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->num_unchoked } |]

getAllowedUploadSlots :: SessionStatus -> IO CInt
getAllowedUploadSlots ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->allowed_upload_slots } |]

getSessionUpBandwidthQueue :: SessionStatus -> IO CInt
getSessionUpBandwidthQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->up_bandwidth_queue } |]

getSessionDownBandwidthQueue :: SessionStatus -> IO CInt
getSessionDownBandwidthQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->down_bandwidth_queue } |]

getUpBandwidthBytesQueue :: SessionStatus -> IO CInt
getUpBandwidthBytesQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->up_bandwidth_bytes_queue } |]

getDownBandwidthBytesQueue :: SessionStatus -> IO CInt
getDownBandwidthBytesQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->down_bandwidth_bytes_queue } |]

getOptimisticUnchokeCounter :: SessionStatus -> IO CInt
getOptimisticUnchokeCounter ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->optimistic_unchoke_counter } |]

getUnchokeCounter :: SessionStatus -> IO CInt
getUnchokeCounter ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->unchoke_counter } |]

getDiskWriteQueue :: SessionStatus -> IO CInt
getDiskWriteQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->disk_write_queue } |]

getDiskReadQueue :: SessionStatus -> IO CInt
getDiskReadQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->disk_read_queue } |]

getDhtNodes :: SessionStatus -> IO CInt
getDhtNodes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_nodes } |]

getDhtNodeCache :: SessionStatus -> IO CInt
getDhtNodeCache ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_node_cache } |]

getDhtTorrents :: SessionStatus -> IO CInt
getDhtTorrents ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_torrents } |]

getDhtGlobalNodes :: SessionStatus -> IO C.CSize
getDhtGlobalNodes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| size_t { $(session_status * hoPtr)->dht_global_nodes } |]

-- TODO:
-- std::vector<dht_lookup> active_requests;
-- std::vector<dht_routing_bucket> dht_routing_table;

getDhtTotalAllocations :: SessionStatus -> IO CInt
getDhtTotalAllocations ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->dht_total_allocations } |]


getUtpStats :: SessionStatus -> IO UtpStatus
getUtpStats ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| utp_status * { new utp_status($(session_status * hoPtr)->utp_stats) } |]

getPeerlistSize :: SessionStatus -> IO CInt
getPeerlistSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_status * hoPtr)->peerlist_size } |]

-- Utp status
getNumIdle :: UtpStatus -> IO CInt
getNumIdle ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_idle } |]

getNumSynSent :: UtpStatus -> IO CInt
getNumSynSent ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_syn_sent } |]

getNumConnected :: UtpStatus -> IO CInt
getNumConnected ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_connected } |]

getNumFinSent :: UtpStatus -> IO CInt
getNumFinSent ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_fin_sent } |]

getNumCloseWait :: UtpStatus -> IO CInt
getNumCloseWait ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(utp_status * hoPtr)->num_close_wait } |]

getPacketLoss :: UtpStatus -> IO Word64
getPacketLoss ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packet_loss } |]

getTimeout :: UtpStatus -> IO Word64
getTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->timeout } |]

getPacketsIn :: UtpStatus -> IO Word64
getPacketsIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packets_in } |]

getPacketsOut :: UtpStatus -> IO Word64
getPacketsOut ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packets_out } |]

getFastRetransmit :: UtpStatus -> IO Word64
getFastRetransmit ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->fast_retransmit } |]

getPacketResend :: UtpStatus -> IO Word64
getPacketResend ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->packet_resend } |]

getSamplesAboveTarget :: UtpStatus -> IO Word64
getSamplesAboveTarget ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->samples_above_target } |]

getSamplesBelowTarget :: UtpStatus -> IO Word64
getSamplesBelowTarget ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->samples_below_target } |]

getPayloadPktsIn :: UtpStatus -> IO Word64
getPayloadPktsIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->payload_pkts_in } |]

getPayloadPktsOut :: UtpStatus -> IO Word64
getPayloadPktsOut ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->payload_pkts_out } |]

getInvalidPktsIn :: UtpStatus -> IO Word64
getInvalidPktsIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->invalid_pkts_in } |]

getRedundantPktsIn :: UtpStatus -> IO Word64
getRedundantPktsIn ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(utp_status * hoPtr)->redundant_pkts_in } |]

