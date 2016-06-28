{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#session_settings session_settings> for "Libtorrent"

module Libtorrent.Session.SessionSettings (SessionSettings
                                        , unSessionSettings
                                        , newSessionSettings
                                        , SuggestMode(..)
                                        , ChokingAlgorithm(..)
                                        , SeedChokingAlgorithm(..)
                                        , IoBufferMode(..)
                                        , DiskCacheAlgo(..)
                                        , BandwidthMixedAlgo(..)
                                        , getVersion
                                        , setVersion
                                        , getUserAgent
                                        , setUserAgent
                                        , getTrackerCompletionTimeout
                                        , setTrackerCompletionTimeout
                                        , getTrackerReceiveTimeout
                                        , setTrackerReceiveTimeout
                                        , getStopTrackerTimeout
                                        , setStopTrackerTimeout
                                        , getTrackerMaximumResponseLength
                                        , setTrackerMaximumResponseLength
                                        , getPieceTimeout
                                        , setPieceTimeout
                                        , getRequestTimeout
                                        , setRequestTimeout
                                        , getRequestQueueTime
                                        , setRequestQueueTime
                                        , getMaxAllowedInRequestQueue
                                        , setMaxAllowedInRequestQueue
                                        , getMaxOutRequestQueue
                                        , setMaxOutRequestQueue
                                        , getWholePiecesThreshold
                                        , setWholePiecesThreshold
                                        , getPeerTimeout
                                        , setPeerTimeout
                                        , getUrlseedTimeout
                                        , setUrlseedTimeout
                                        , getUrlseedPipelineSize
                                        , setUrlseedPipelineSize
                                        , getUrlseedWaitRetry
                                        , setUrlseedWaitRetry
                                        , getFilePoolSize
                                        , setFilePoolSize
                                        , getAllowMultipleConnectionsPerIp
                                        , setAllowMultipleConnectionsPerIp
                                        , getMaxFailcount
                                        , setMaxFailcount
                                        , getMinReconnectTime
                                        , setMinReconnectTime
                                        , getPeerConnectTimeout
                                        , setPeerConnectTimeout
                                        , getIgnoreLimitsOnLocalNetwork
                                        , setIgnoreLimitsOnLocalNetwork
                                        , getConnectionSpeed
                                        , setConnectionSpeed
                                        , getSendRedundantHave
                                        , setSendRedundantHave
                                        , getLazyBitfields
                                        , setLazyBitfields
                                        , getInactivityTimeout
                                        , setInactivityTimeout
                                        , getUnchokeInterval
                                        , setUnchokeInterval
                                        , getOptimisticUnchokeInterval
                                        , setOptimisticUnchokeInterval
                                        , getAnnounceIp
                                        , setAnnounceIp
                                        , getNumWant
                                        , setNumWant
                                        , getInitialPickerThreshold
                                        , setInitialPickerThreshold
                                        , getAllowedFastSetSize
                                        , setAllowedFastSetSize
                                        , getSuggestMode
                                        , setSuggestMode
                                        , getMaxQueuedDiskBytes
                                        , setMaxQueuedDiskBytes
                                        , getMaxQueuedDiskBytesLowWatermark
                                        , setMaxQueuedDiskBytesLowWatermark
                                        , getHandshakeTimeout
                                        , setHandshakeTimeout
                                        , getUseDhtAsFallback
                                        , setUseDhtAsFallback
                                        , getFreeTorrentHashes
                                        , setFreeTorrentHashes
                                        , getUpnpIgnoreNonrouters
                                        , setUpnpIgnoreNonrouters
                                        , getSendBufferLowWatermark
                                        , setSendBufferLowWatermark
                                        , getSendBufferWatermark
                                        , setSendBufferWatermark
                                        , getSendBufferWatermarkFactor
                                        , setSendBufferWatermarkFactor
                                        , getChokingAlgorithm
                                        , setChokingAlgorithm
                                        , getSeedChokingAlgorithm
                                        , setSeedChokingAlgorithm
                                        , getUseParoleMode
                                        , setUseParoleMode
                                        , getCacheSize
                                        , setCacheSize
                                        , getCacheBufferChunkSize
                                        , setCacheBufferChunkSize
                                        , getCacheExpiry
                                        , setCacheExpiry
                                        , getUseReadCache
                                        , setUseReadCache
                                        , getExplicitReadCache
                                        , setExplicitReadCache
                                        , getExplicitCacheInterval
                                        , setExplicitCacheInterval
                                        , getDiskIoWriteMode
                                        , setDiskIoWriteMode
                                        , getDiskIoReadMode
                                        , setDiskIoReadMode
                                        , getCoalesceReads
                                        , setCoalesceReads
                                        , getCoalesceWrites
                                        , setCoalesceWrites
                                        , getOutgoingPorts
                                        , setOutgoingPorts
                                        , getPeerTos
                                        , setPeerTos
                                        , getActiveDownloads
                                        , setActiveDownloads
                                        , getActiveSeeds
                                        , setActiveSeeds
                                        , getActiveDhtLimit
                                        , setActiveDhtLimit
                                        , getActiveTrackerLimit
                                        , setActiveTrackerLimit
                                        , getActiveLsdLimit
                                        , setActiveLsdLimit
                                        , getActiveLimit
                                        , setActiveLimit
                                        , getAutoManagePreferSeeds
                                        , setAutoManagePreferSeeds
                                        , getDontCountSlowTorrents
                                        , setDontCountSlowTorrents
                                        , getAutoManageInterval
                                        , setAutoManageInterval
                                        , getShareRatioLimit
                                        , setShareRatioLimit
                                        , getSeedTimeRatioLimit
                                        , setSeedTimeRatioLimit
                                        , getSeedTimeLimit
                                        , setSeedTimeLimit
                                        , getPeerTurnoverInterval
                                        , setPeerTurnoverInterval
                                        , getPeerTurnover
                                        , setPeerTurnover
                                        , getPeerTurnoverCutoff
                                        , setPeerTurnoverCutoff
                                        , getCloseRedundantConnections
                                        , setCloseRedundantConnections
                                        , getAutoScrapeInterval
                                        , setAutoScrapeInterval
                                        , getAutoScrapeMinInterval
                                        , setAutoScrapeMinInterval
                                        , getMaxPeerlistSize
                                        , setMaxPeerlistSize
                                        , getMaxPausedPeerlistSize
                                        , setMaxPausedPeerlistSize
                                        , getMinAnnounceInterval
                                        , setMinAnnounceInterval
                                        , getPrioritizePartialPieces
                                        , setPrioritizePartialPieces
                                        , getAutoManageStartup
                                        , setAutoManageStartup
                                        , getRateLimitIpOverhead
                                        , setRateLimitIpOverhead
                                        , getAnnounceToAllTrackers
                                        , setAnnounceToAllTrackers
                                        , getAnnounceToAllTiers
                                        , setAnnounceToAllTiers
                                        , getPreferUdpTrackers
                                        , setPreferUdpTrackers
                                        , getStrictSuperSeeding
                                        , setStrictSuperSeeding
                                        , getSeedingPieceQuota
                                        , setSeedingPieceQuota
                                        , getMaxSparseRegions
                                        , setMaxSparseRegions
                                        , getLockDiskCache
                                        , setLockDiskCache
                                        , getMaxRejects
                                        , setMaxRejects
                                        , getRecvSocketBufferSize
                                        , setRecvSocketBufferSize
                                        , getSendSocketBufferSize
                                        , setSendSocketBufferSize
                                        , getOptimizeHashingForSpeed
                                        , setOptimizeHashingForSpeed
                                        , getFileChecksDelayPerBlock
                                        , setFileChecksDelayPerBlock
                                        , getDiskCacheAlgorithm
                                        , setDiskCacheAlgorithm
                                        , getReadCacheLineSize
                                        , setReadCacheLineSize
                                        , getWriteCacheLineSize
                                        , setWriteCacheLineSize
                                        , getOptimisticDiskRetry
                                        , setOptimisticDiskRetry
                                        , getDisableHashChecks
                                        , setDisableHashChecks
                                        , getAllowReorderedDiskOperations
                                        , setAllowReorderedDiskOperations
                                        , getAllowI2pMixed
                                        , setAllowI2pMixed
                                        , getMaxSuggestPieces
                                        , setMaxSuggestPieces
                                        , getDropSkippedRequests
                                        , setDropSkippedRequests
                                        , getLowPrioDisk
                                        , setLowPrioDisk
                                        , getLocalServiceAnnounceInterval
                                        , setLocalServiceAnnounceInterval
                                        , getDhtAnnounceInterval
                                        , setDhtAnnounceInterval
                                        , getUdpTrackerTokenExpiry
                                        , setUdpTrackerTokenExpiry
                                        , getVolatileReadCache
                                        , setVolatileReadCache
                                        , getGuidedReadCache
                                        , setGuidedReadCache
                                        , getDefaultCacheMinAge
                                        , setDefaultCacheMinAge
                                        , getNumOptimisticUnchokeSlots
                                        , setNumOptimisticUnchokeSlots
                                        , getNoAtimeStorage
                                        , setNoAtimeStorage
                                        , getDefaultEstReciprocationRate
                                        , setDefaultEstReciprocationRate
                                        , getIncreaseEstReciprocationRate
                                        , setIncreaseEstReciprocationRate
                                        , getDecreaseEstReciprocationRate
                                        , setDecreaseEstReciprocationRate
                                        , getIncomingStartsQueuedTorrents
                                        , setIncomingStartsQueuedTorrents
                                        , getReportTrueDownloaded
                                        , setReportTrueDownloaded
                                        , getStrictEndGameMode
                                        , setStrictEndGameMode
                                        , getBroadcastLsd
                                        , setBroadcastLsd
                                        , getEnableOutgoingUtp
                                        , setEnableOutgoingUtp
                                        , getEnableIncomingUtp
                                        , setEnableIncomingUtp
                                        , getEnableOutgoingTcp
                                        , setEnableOutgoingTcp
                                        , getEnableIncomingTcp
                                        , setEnableIncomingTcp
                                        , getMaxPexPeers
                                        , setMaxPexPeers
                                        , getIgnoreResumeTimestamps
                                        , setIgnoreResumeTimestamps
                                        , getNoRecheckIncompleteResume
                                        , setNoRecheckIncompleteResume
                                        , getAnonymousMode
                                        , setAnonymousMode
                                        , getForceProxy
                                        , setForceProxy
                                        , getTickInterval
                                        , setTickInterval
                                        , getReportWebSeedDownloads
                                        , setReportWebSeedDownloads
                                        , getShareModeTarget
                                        , setShareModeTarget
                                        , getUploadRateLimit
                                        , setUploadRateLimit
                                        , getDownloadRateLimit
                                        , setDownloadRateLimit
                                        , getLocalUploadRateLimit
                                        , setLocalUploadRateLimit
                                        , getLocalDownloadRateLimit
                                        , setLocalDownloadRateLimit
                                        , getDhtUploadRateLimit
                                        , setDhtUploadRateLimit
                                        , getUnchokeSlotsLimit
                                        , setUnchokeSlotsLimit
                                        , getHalfOpenLimit
                                        , setHalfOpenLimit
                                        , getSessionConnectionsLimit
                                        , setSessionConnectionsLimit
                                        , getConnectionsSlack
                                        , setConnectionsSlack
                                        , getUtpTargetDelay
                                        , setUtpTargetDelay
                                        , getUtpGainFactor
                                        , setUtpGainFactor
                                        , getUtpMinTimeout
                                        , setUtpMinTimeout
                                        , getUtpSynResends
                                        , setUtpSynResends
                                        , getUtpFinResends
                                        , setUtpFinResends
                                        , getUtpNumResends
                                        , setUtpNumResends
                                        , getUtpConnectTimeout
                                        , setUtpConnectTimeout
                                        , getUtpDynamicSockBuf
                                        , setUtpDynamicSockBuf
                                        , getUtpLossMultiplier
                                        , setUtpLossMultiplier
                                        , getMixedModeAlgorithm
                                        , setMixedModeAlgorithm
                                        , getRateLimitUtp
                                        , setRateLimitUtp
                                        , getListenQueueSize
                                        , setListenQueueSize
                                        , getAnnounceDoubleNat
                                        , setAnnounceDoubleNat
                                        , getTorrentConnectBoost
                                        , setTorrentConnectBoost
                                        , getSeedingOutgoingConnections
                                        , setSeedingOutgoingConnections
                                        , getNoConnectPrivilegedPorts
                                        , setNoConnectPrivilegedPorts
                                        , getAlertQueueSize
                                        , setAlertQueueSize
                                        , getMaxMetadataSize
                                        , setMaxMetadataSize
                                        , getSmoothConnects
                                        , setSmoothConnects
                                        , getAlwaysSendUserAgent
                                        , setAlwaysSendUserAgent
                                        , getApplyIpFilterToTrackers
                                        , setApplyIpFilterToTrackers
                                        , getReadJobEvery
                                        , setReadJobEvery
                                        , getUseDiskReadAhead
                                        , setUseDiskReadAhead
                                        , getLockFiles
                                        , setLockFiles
                                        , getSslListen
                                        , setSslListen
                                        , getTrackerBackoff
                                        , setTrackerBackoff
                                        , getBanWebSeeds
                                        , setBanWebSeeds
                                        , getMaxHttpRecvBufferSize
                                        , setMaxHttpRecvBufferSize
                                        , getSupportShareMode
                                        , setSupportShareMode
                                        , getSupportMerkleTorrents
                                        , setSupportMerkleTorrents
                                        , getReportRedundantBytes
                                        , setReportRedundantBytes
                                        , getHandshakeClientVersion
                                        , setHandshakeClientVersion
                                        , getUseDiskCachePool
                                        , setUseDiskCachePool
                                        , getInactiveDownRate
                                        , setInactiveDownRate
                                        , getInactiveUpRate
                                        , setInactiveUpRate
                                        ) where

import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types
import           Libtorrent.String


C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data SuggestMode =
  NoPieceSuggestions
  | SuggestReadCache
  deriving (Show, Enum, Bounded)

data ChokingAlgorithm =
  FixedSlotsChoker
  | AutoExpandChoker
  | RateBasedChoker
  | BittyrantChoker
  deriving (Show, Enum, Bounded)

data SeedChokingAlgorithm =
  RoundRobin
  | FastestUpload
  | AntiLeech
  deriving (Show, Enum, Bounded)

data IoBufferMode =
  EnableOsCache
  | DisableOsCacheForAlignedFiles
  | DisableOsCache
  deriving (Show, Enum, Bounded)

data DiskCacheAlgo =
  Lru
  | LargestContiguous
  | AvoidReadback
  deriving (Show, Enum, Bounded)

data BandwidthMixedAlgo =
  PreferTcp
  | PeerProportional
  deriving (Show, Enum, Bounded)


newtype SessionSettings = SessionSettings { unSessionSettings :: ForeignPtr (CType SessionSettings)}

instance Show SessionSettings where
  show _ = "SessionSettings"

instance Inlinable SessionSettings where
  type (CType SessionSettings) = C'SessionSettings

instance FromPtr SessionSettings where
  fromPtr = objFromPtr SessionSettings $ \ptr ->
    [CU.exp| void { delete $(session_settings * ptr); } |]

instance WithPtr SessionSettings where
  withPtr (SessionSettings fptr) = withForeignPtr fptr

newSessionSettings :: IO SessionSettings
newSessionSettings =
  fromPtr [CU.exp| session_settings * { new session_settings() }|]


getVersion :: SessionSettings -> IO CInt
getVersion ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->version } |]

setVersion :: SessionSettings -> CInt -> IO ()
setVersion ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->version = $(int val)} |]

getUserAgent :: SessionSettings -> IO Text
getUserAgent ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->user_agent) } |]
  stdStringToText res

setUserAgent :: SessionSettings -> Text -> IO ()
setUserAgent ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->user_agent = std::string($(const char * cstr), $(size_t clen))} |]

getTrackerCompletionTimeout :: SessionSettings -> IO CInt
getTrackerCompletionTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_completion_timeout } |]

setTrackerCompletionTimeout :: SessionSettings -> CInt -> IO ()
setTrackerCompletionTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_completion_timeout = $(int val)} |]

getTrackerReceiveTimeout :: SessionSettings -> IO CInt
getTrackerReceiveTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_receive_timeout } |]

setTrackerReceiveTimeout :: SessionSettings -> CInt -> IO ()
setTrackerReceiveTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_receive_timeout = $(int val)} |]

getStopTrackerTimeout :: SessionSettings -> IO CInt
getStopTrackerTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->stop_tracker_timeout } |]

setStopTrackerTimeout :: SessionSettings -> CInt -> IO ()
setStopTrackerTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->stop_tracker_timeout = $(int val)} |]

getTrackerMaximumResponseLength :: SessionSettings -> IO CInt
getTrackerMaximumResponseLength ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_maximum_response_length } |]

setTrackerMaximumResponseLength :: SessionSettings -> CInt -> IO ()
setTrackerMaximumResponseLength ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_maximum_response_length = $(int val)} |]

getPieceTimeout :: SessionSettings -> IO CInt
getPieceTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->piece_timeout } |]

setPieceTimeout :: SessionSettings -> CInt -> IO ()
setPieceTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->piece_timeout = $(int val)} |]

getRequestTimeout :: SessionSettings -> IO CInt
getRequestTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->request_timeout } |]

setRequestTimeout :: SessionSettings -> CInt -> IO ()
setRequestTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->request_timeout = $(int val)} |]

getRequestQueueTime :: SessionSettings -> IO CInt
getRequestQueueTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->request_queue_time } |]

setRequestQueueTime :: SessionSettings -> CInt -> IO ()
setRequestQueueTime ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->request_queue_time = $(int val)} |]

getMaxAllowedInRequestQueue :: SessionSettings -> IO CInt
getMaxAllowedInRequestQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_allowed_in_request_queue } |]

setMaxAllowedInRequestQueue :: SessionSettings -> CInt -> IO ()
setMaxAllowedInRequestQueue ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_allowed_in_request_queue = $(int val)} |]

getMaxOutRequestQueue :: SessionSettings -> IO CInt
getMaxOutRequestQueue ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_out_request_queue } |]

setMaxOutRequestQueue :: SessionSettings -> CInt -> IO ()
setMaxOutRequestQueue ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_out_request_queue = $(int val)} |]

getWholePiecesThreshold :: SessionSettings -> IO CInt
getWholePiecesThreshold ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->whole_pieces_threshold } |]

setWholePiecesThreshold :: SessionSettings -> CInt -> IO ()
setWholePiecesThreshold ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->whole_pieces_threshold = $(int val)} |]

getPeerTimeout :: SessionSettings -> IO CInt
getPeerTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->peer_timeout } |]

setPeerTimeout :: SessionSettings -> CInt -> IO ()
setPeerTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->peer_timeout = $(int val)} |]

getUrlseedTimeout :: SessionSettings -> IO CInt
getUrlseedTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_timeout } |]

setUrlseedTimeout :: SessionSettings -> CInt -> IO ()
setUrlseedTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_timeout = $(int val)} |]

getUrlseedPipelineSize :: SessionSettings -> IO CInt
getUrlseedPipelineSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_pipeline_size } |]

setUrlseedPipelineSize :: SessionSettings -> CInt -> IO ()
setUrlseedPipelineSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_pipeline_size = $(int val)} |]

getUrlseedWaitRetry :: SessionSettings -> IO CInt
getUrlseedWaitRetry ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_wait_retry } |]

setUrlseedWaitRetry :: SessionSettings -> CInt -> IO ()
setUrlseedWaitRetry ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_wait_retry = $(int val)} |]

getFilePoolSize :: SessionSettings -> IO CInt
getFilePoolSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->file_pool_size } |]

setFilePoolSize :: SessionSettings -> CInt -> IO ()
setFilePoolSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->file_pool_size = $(int val)} |]

getAllowMultipleConnectionsPerIp :: SessionSettings -> IO Bool
getAllowMultipleConnectionsPerIp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_multiple_connections_per_ip } |]

setAllowMultipleConnectionsPerIp :: SessionSettings -> Bool -> IO ()
setAllowMultipleConnectionsPerIp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_multiple_connections_per_ip = $(bool val')} |]

getMaxFailcount :: SessionSettings -> IO CInt
getMaxFailcount ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_failcount } |]

setMaxFailcount :: SessionSettings -> CInt -> IO ()
setMaxFailcount ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_failcount = $(int val)} |]

getMinReconnectTime :: SessionSettings -> IO CInt
getMinReconnectTime ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->min_reconnect_time } |]

setMinReconnectTime :: SessionSettings -> CInt -> IO ()
setMinReconnectTime ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->min_reconnect_time = $(int val)} |]

getPeerConnectTimeout :: SessionSettings -> IO CInt
getPeerConnectTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->peer_connect_timeout } |]

setPeerConnectTimeout :: SessionSettings -> CInt -> IO ()
setPeerConnectTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->peer_connect_timeout = $(int val)} |]

getIgnoreLimitsOnLocalNetwork :: SessionSettings -> IO Bool
getIgnoreLimitsOnLocalNetwork ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ignore_limits_on_local_network } |]

setIgnoreLimitsOnLocalNetwork :: SessionSettings -> Bool -> IO ()
setIgnoreLimitsOnLocalNetwork ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ignore_limits_on_local_network = $(bool val')} |]

getConnectionSpeed :: SessionSettings -> IO CInt
getConnectionSpeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->connection_speed } |]

setConnectionSpeed :: SessionSettings -> CInt -> IO ()
setConnectionSpeed ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->connection_speed = $(int val)} |]

getSendRedundantHave :: SessionSettings -> IO Bool
getSendRedundantHave ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->send_redundant_have } |]

setSendRedundantHave :: SessionSettings -> Bool -> IO ()
setSendRedundantHave ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->send_redundant_have = $(bool val')} |]

getLazyBitfields :: SessionSettings -> IO Bool
getLazyBitfields ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lazy_bitfields } |]

setLazyBitfields :: SessionSettings -> Bool -> IO ()
setLazyBitfields ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lazy_bitfields = $(bool val')} |]

getInactivityTimeout :: SessionSettings -> IO CInt
getInactivityTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->inactivity_timeout } |]

setInactivityTimeout :: SessionSettings -> CInt -> IO ()
setInactivityTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->inactivity_timeout = $(int val)} |]

getUnchokeInterval :: SessionSettings -> IO CInt
getUnchokeInterval ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->unchoke_interval } |]

setUnchokeInterval :: SessionSettings -> CInt -> IO ()
setUnchokeInterval ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->unchoke_interval = $(int val)} |]

getOptimisticUnchokeInterval :: SessionSettings -> IO CInt
getOptimisticUnchokeInterval ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->optimistic_unchoke_interval } |]

setOptimisticUnchokeInterval :: SessionSettings -> CInt -> IO ()
setOptimisticUnchokeInterval ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->optimistic_unchoke_interval = $(int val)} |]

getAnnounceIp :: SessionSettings -> IO Text
getAnnounceIp ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->announce_ip) } |]
  stdStringToText res

setAnnounceIp :: SessionSettings -> Text -> IO ()
setAnnounceIp ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->announce_ip = std::string($(const char * cstr), $(size_t clen))} |]

getNumWant :: SessionSettings -> IO CInt
getNumWant ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->num_want } |]

setNumWant :: SessionSettings -> CInt -> IO ()
setNumWant ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->num_want = $(int val)} |]

getInitialPickerThreshold :: SessionSettings -> IO CInt
getInitialPickerThreshold ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->initial_picker_threshold } |]

setInitialPickerThreshold :: SessionSettings -> CInt -> IO ()
setInitialPickerThreshold ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->initial_picker_threshold = $(int val)} |]

getAllowedFastSetSize :: SessionSettings -> IO CInt
getAllowedFastSetSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->allowed_fast_set_size } |]

setAllowedFastSetSize :: SessionSettings -> CInt -> IO ()
setAllowedFastSetSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->allowed_fast_set_size = $(int val)} |]

getSuggestMode :: SessionSettings -> IO SuggestMode
getSuggestMode ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->suggest_mode } |]

setSuggestMode :: SessionSettings -> SuggestMode -> IO ()
setSuggestMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->suggest_mode = $(int val')} |]

getMaxQueuedDiskBytes :: SessionSettings -> IO CInt
getMaxQueuedDiskBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_queued_disk_bytes } |]

setMaxQueuedDiskBytes :: SessionSettings -> CInt -> IO ()
setMaxQueuedDiskBytes ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_queued_disk_bytes = $(int val)} |]

getMaxQueuedDiskBytesLowWatermark :: SessionSettings -> IO CInt
getMaxQueuedDiskBytesLowWatermark ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_queued_disk_bytes_low_watermark } |]

setMaxQueuedDiskBytesLowWatermark :: SessionSettings -> CInt -> IO ()
setMaxQueuedDiskBytesLowWatermark ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_queued_disk_bytes_low_watermark = $(int val)} |]

getHandshakeTimeout :: SessionSettings -> IO CInt
getHandshakeTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->handshake_timeout } |]

setHandshakeTimeout :: SessionSettings -> CInt -> IO ()
setHandshakeTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->handshake_timeout = $(int val)} |]

getUseDhtAsFallback :: SessionSettings -> IO Bool
getUseDhtAsFallback ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_dht_as_fallback } |]

setUseDhtAsFallback :: SessionSettings -> Bool -> IO ()
setUseDhtAsFallback ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_dht_as_fallback = $(bool val')} |]

getFreeTorrentHashes :: SessionSettings -> IO Bool
getFreeTorrentHashes ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->free_torrent_hashes } |]

setFreeTorrentHashes :: SessionSettings -> Bool -> IO ()
setFreeTorrentHashes ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->free_torrent_hashes = $(bool val')} |]

getUpnpIgnoreNonrouters :: SessionSettings -> IO Bool
getUpnpIgnoreNonrouters ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->upnp_ignore_nonrouters } |]

setUpnpIgnoreNonrouters :: SessionSettings -> Bool -> IO ()
setUpnpIgnoreNonrouters ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->upnp_ignore_nonrouters = $(bool val')} |]

getSendBufferLowWatermark :: SessionSettings -> IO CInt
getSendBufferLowWatermark ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_low_watermark } |]

setSendBufferLowWatermark :: SessionSettings -> CInt -> IO ()
setSendBufferLowWatermark ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_low_watermark = $(int val)} |]

getSendBufferWatermark :: SessionSettings -> IO CInt
getSendBufferWatermark ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_watermark } |]

setSendBufferWatermark :: SessionSettings -> CInt -> IO ()
setSendBufferWatermark ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_watermark = $(int val)} |]

getSendBufferWatermarkFactor :: SessionSettings -> IO CInt
getSendBufferWatermarkFactor ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_watermark_factor } |]

setSendBufferWatermarkFactor :: SessionSettings -> CInt -> IO ()
setSendBufferWatermarkFactor ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_watermark_factor = $(int val)} |]

getChokingAlgorithm :: SessionSettings -> IO ChokingAlgorithm
getChokingAlgorithm ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->choking_algorithm } |]

setChokingAlgorithm :: SessionSettings -> ChokingAlgorithm -> IO ()
setChokingAlgorithm ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->choking_algorithm = $(int val')} |]

getSeedChokingAlgorithm :: SessionSettings -> IO SeedChokingAlgorithm
getSeedChokingAlgorithm ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->seed_choking_algorithm } |]

setSeedChokingAlgorithm :: SessionSettings -> CInt -> IO ()
setSeedChokingAlgorithm ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->seed_choking_algorithm = $(int val')} |]

getUseParoleMode :: SessionSettings -> IO Bool
getUseParoleMode ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_parole_mode } |]

setUseParoleMode :: SessionSettings -> Bool -> IO ()
setUseParoleMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_parole_mode = $(bool val')} |]

getCacheSize :: SessionSettings -> IO CInt
getCacheSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_size } |]

setCacheSize :: SessionSettings -> CInt -> IO ()
setCacheSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_size = $(int val)} |]

getCacheBufferChunkSize :: SessionSettings -> IO CInt
getCacheBufferChunkSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_buffer_chunk_size } |]

setCacheBufferChunkSize :: SessionSettings -> CInt -> IO ()
setCacheBufferChunkSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_buffer_chunk_size = $(int val)} |]

getCacheExpiry :: SessionSettings -> IO CInt
getCacheExpiry ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_expiry } |]

setCacheExpiry :: SessionSettings -> CInt -> IO ()
setCacheExpiry ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_expiry = $(int val)} |]

getUseReadCache :: SessionSettings -> IO Bool
getUseReadCache ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_read_cache } |]

setUseReadCache :: SessionSettings -> Bool -> IO ()
setUseReadCache ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_read_cache = $(bool val')} |]

getExplicitReadCache :: SessionSettings -> IO Bool
getExplicitReadCache ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->explicit_read_cache } |]

setExplicitReadCache :: SessionSettings -> Bool -> IO ()
setExplicitReadCache ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->explicit_read_cache = $(bool val')} |]

getExplicitCacheInterval :: SessionSettings -> IO CInt
getExplicitCacheInterval ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->explicit_cache_interval } |]

setExplicitCacheInterval :: SessionSettings -> CInt -> IO ()
setExplicitCacheInterval ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->explicit_cache_interval = $(int val)} |]

getDiskIoWriteMode :: SessionSettings -> IO IoBufferMode
getDiskIoWriteMode ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_io_write_mode } |]

setDiskIoWriteMode :: SessionSettings -> IoBufferMode -> IO ()
setDiskIoWriteMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_io_write_mode = $(int val')} |]

getDiskIoReadMode :: SessionSettings -> IO IoBufferMode
getDiskIoReadMode ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_io_read_mode } |]

setDiskIoReadMode :: SessionSettings -> CInt -> IO ()
setDiskIoReadMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_io_read_mode = $(int val')} |]

getCoalesceReads :: SessionSettings -> IO Bool
getCoalesceReads ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->coalesce_reads } |]

setCoalesceReads :: SessionSettings -> Bool -> IO ()
setCoalesceReads ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->coalesce_reads = $(bool val')} |]

getCoalesceWrites :: SessionSettings -> IO Bool
getCoalesceWrites ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->coalesce_writes } |]

setCoalesceWrites :: SessionSettings -> Bool -> IO ()
setCoalesceWrites ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->coalesce_writes = $(bool val')} |]

getOutgoingPorts :: SessionSettings -> IO (CInt, CInt)
getOutgoingPorts ho =
  withPtr ho $ \hoPtr ->
  C.withPtrs_ $ \(fromPtr', toPtr') ->
  [CU.block| void {
      *$(int * fromPtr') = $(session_settings * hoPtr)->outgoing_ports.first;
      *$(int * toPtr') = $(session_settings * hoPtr)->outgoing_ports.second;
     }
  |]

setOutgoingPorts :: SessionSettings -> (CInt, CInt) -> IO ()
setOutgoingPorts ho (fromPtr', toPtr') =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->outgoing_ports = std::make_pair($(int fromPtr'), $(int toPtr')) } |]

getPeerTos :: SessionSettings -> IO C.CChar
getPeerTos ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| char { $(session_settings * hoPtr)->peer_tos } |]

setPeerTos :: SessionSettings -> C.CChar -> IO ()
setPeerTos ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_tos = $(char val)} |]

getActiveDownloads :: SessionSettings -> IO CInt
getActiveDownloads ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_downloads } |]

setActiveDownloads :: SessionSettings -> CInt -> IO ()
setActiveDownloads ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_downloads = $(int val)} |]

getActiveSeeds :: SessionSettings -> IO CInt
getActiveSeeds ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_seeds } |]

setActiveSeeds :: SessionSettings -> CInt -> IO ()
setActiveSeeds ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_seeds = $(int val)} |]

getActiveDhtLimit :: SessionSettings -> IO CInt
getActiveDhtLimit ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_dht_limit } |]

setActiveDhtLimit :: SessionSettings -> CInt -> IO ()
setActiveDhtLimit ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_dht_limit = $(int val)} |]

getActiveTrackerLimit :: SessionSettings -> IO CInt
getActiveTrackerLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_tracker_limit } |]

setActiveTrackerLimit :: SessionSettings -> CInt -> IO ()
setActiveTrackerLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_tracker_limit = $(int val)} |]

getActiveLsdLimit :: SessionSettings -> IO CInt
getActiveLsdLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_lsd_limit } |]

setActiveLsdLimit :: SessionSettings -> CInt -> IO ()
setActiveLsdLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_lsd_limit = $(int val)} |]

getActiveLimit :: SessionSettings -> IO CInt
getActiveLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_limit } |]

setActiveLimit :: SessionSettings -> CInt -> IO ()
setActiveLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_limit = $(int val)} |]

getAutoManagePreferSeeds :: SessionSettings -> IO Bool
getAutoManagePreferSeeds ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->auto_manage_prefer_seeds } |]

setAutoManagePreferSeeds :: SessionSettings -> Bool -> IO ()
setAutoManagePreferSeeds ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->auto_manage_prefer_seeds = $(bool val')} |]

getDontCountSlowTorrents :: SessionSettings -> IO Bool
getDontCountSlowTorrents ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->dont_count_slow_torrents } |]

setDontCountSlowTorrents :: SessionSettings -> Bool -> IO ()
setDontCountSlowTorrents ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->dont_count_slow_torrents = $(bool val')} |]

getAutoManageInterval :: SessionSettings -> IO CInt
getAutoManageInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_manage_interval } |]

setAutoManageInterval :: SessionSettings -> CInt -> IO ()
setAutoManageInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_manage_interval = $(int val)} |]

getShareRatioLimit :: SessionSettings -> IO C.CFloat
getShareRatioLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->share_ratio_limit } |]

setShareRatioLimit :: SessionSettings -> C.CFloat -> IO ()
setShareRatioLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->share_ratio_limit = $(float val)} |]

getSeedTimeRatioLimit :: SessionSettings -> IO C.CFloat
getSeedTimeRatioLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->seed_time_ratio_limit } |]

setSeedTimeRatioLimit :: SessionSettings -> C.CFloat -> IO ()
setSeedTimeRatioLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seed_time_ratio_limit = $(float val)} |]

getSeedTimeLimit :: SessionSettings -> IO CInt
getSeedTimeLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->seed_time_limit } |]

setSeedTimeLimit :: SessionSettings -> CInt -> IO ()
setSeedTimeLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seed_time_limit = $(int val)} |]

getPeerTurnoverInterval :: SessionSettings -> IO CInt
getPeerTurnoverInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->peer_turnover_interval } |]

setPeerTurnoverInterval :: SessionSettings -> CInt -> IO ()
setPeerTurnoverInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover_interval = $(int val)} |]

getPeerTurnover :: SessionSettings -> IO C.CFloat
getPeerTurnover ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->peer_turnover } |]

setPeerTurnover :: SessionSettings -> C.CFloat -> IO ()
setPeerTurnover ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover = $(float val)} |]

getPeerTurnoverCutoff :: SessionSettings -> IO C.CFloat
getPeerTurnoverCutoff ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->peer_turnover_cutoff } |]

setPeerTurnoverCutoff :: SessionSettings -> C.CFloat -> IO ()
setPeerTurnoverCutoff ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover_cutoff = $(float val)} |]

getCloseRedundantConnections :: SessionSettings -> IO Bool
getCloseRedundantConnections ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->close_redundant_connections } |]

setCloseRedundantConnections :: SessionSettings -> Bool -> IO ()
setCloseRedundantConnections ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->close_redundant_connections = $(bool val')} |]

getAutoScrapeInterval :: SessionSettings -> IO CInt
getAutoScrapeInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_scrape_interval } |]

setAutoScrapeInterval :: SessionSettings -> CInt -> IO ()
setAutoScrapeInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_scrape_interval = $(int val)} |]

getAutoScrapeMinInterval :: SessionSettings -> IO CInt
getAutoScrapeMinInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_scrape_min_interval } |]

setAutoScrapeMinInterval :: SessionSettings -> CInt -> IO ()
setAutoScrapeMinInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_scrape_min_interval = $(int val)} |]

getMaxPeerlistSize :: SessionSettings -> IO CInt
getMaxPeerlistSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_peerlist_size } |]

setMaxPeerlistSize :: SessionSettings -> CInt -> IO ()
setMaxPeerlistSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_peerlist_size = $(int val)} |]

getMaxPausedPeerlistSize :: SessionSettings -> IO CInt
getMaxPausedPeerlistSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_paused_peerlist_size } |]

setMaxPausedPeerlistSize :: SessionSettings -> CInt -> IO ()
setMaxPausedPeerlistSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_paused_peerlist_size = $(int val)} |]

getMinAnnounceInterval :: SessionSettings -> IO CInt
getMinAnnounceInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->min_announce_interval } |]

setMinAnnounceInterval :: SessionSettings -> CInt -> IO ()
setMinAnnounceInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->min_announce_interval = $(int val)} |]

getPrioritizePartialPieces :: SessionSettings -> IO Bool
getPrioritizePartialPieces ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->prioritize_partial_pieces } |]

setPrioritizePartialPieces :: SessionSettings -> Bool -> IO ()
setPrioritizePartialPieces ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->prioritize_partial_pieces = $(bool val')} |]

getAutoManageStartup :: SessionSettings -> IO CInt
getAutoManageStartup ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_manage_startup } |]

setAutoManageStartup :: SessionSettings -> CInt -> IO ()
setAutoManageStartup ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_manage_startup = $(int val)} |]

getRateLimitIpOverhead :: SessionSettings -> IO Bool
getRateLimitIpOverhead ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->rate_limit_ip_overhead } |]

setRateLimitIpOverhead :: SessionSettings -> Bool -> IO ()
setRateLimitIpOverhead ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->rate_limit_ip_overhead = $(bool val')} |]

getAnnounceToAllTrackers :: SessionSettings -> IO Bool
getAnnounceToAllTrackers ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_to_all_trackers } |]

setAnnounceToAllTrackers :: SessionSettings -> Bool -> IO ()
setAnnounceToAllTrackers ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_to_all_trackers = $(bool val')} |]

getAnnounceToAllTiers :: SessionSettings -> IO Bool
getAnnounceToAllTiers ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_to_all_tiers } |]

setAnnounceToAllTiers :: SessionSettings -> Bool -> IO ()
setAnnounceToAllTiers ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_to_all_tiers = $(bool val')} |]

getPreferUdpTrackers :: SessionSettings -> IO Bool
getPreferUdpTrackers ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->prefer_udp_trackers } |]

setPreferUdpTrackers :: SessionSettings -> Bool -> IO ()
setPreferUdpTrackers ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->prefer_udp_trackers = $(bool val')} |]

getStrictSuperSeeding :: SessionSettings -> IO Bool
getStrictSuperSeeding ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->strict_super_seeding } |]

setStrictSuperSeeding :: SessionSettings -> Bool -> IO ()
setStrictSuperSeeding ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->strict_super_seeding = $(bool val')} |]

getSeedingPieceQuota :: SessionSettings -> IO CInt
getSeedingPieceQuota ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->seeding_piece_quota } |]

setSeedingPieceQuota :: SessionSettings -> CInt -> IO ()
setSeedingPieceQuota ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seeding_piece_quota = $(int val)} |]

getMaxSparseRegions :: SessionSettings -> IO CInt
getMaxSparseRegions ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_sparse_regions } |]

setMaxSparseRegions :: SessionSettings -> CInt -> IO ()
setMaxSparseRegions ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_sparse_regions = $(int val)} |]

getLockDiskCache :: SessionSettings -> IO Bool
getLockDiskCache ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lock_disk_cache } |]

setLockDiskCache :: SessionSettings -> Bool -> IO ()
setLockDiskCache ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lock_disk_cache = $(bool val')} |]

getMaxRejects :: SessionSettings -> IO CInt
getMaxRejects ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_rejects } |]

setMaxRejects :: SessionSettings -> CInt -> IO ()
setMaxRejects ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_rejects = $(int val)} |]

getRecvSocketBufferSize :: SessionSettings -> IO CInt
getRecvSocketBufferSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->recv_socket_buffer_size } |]

setRecvSocketBufferSize :: SessionSettings -> CInt -> IO ()
setRecvSocketBufferSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->recv_socket_buffer_size = $(int val)} |]

getSendSocketBufferSize :: SessionSettings -> IO CInt
getSendSocketBufferSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->send_socket_buffer_size } |]

setSendSocketBufferSize :: SessionSettings -> CInt -> IO ()
setSendSocketBufferSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->send_socket_buffer_size = $(int val)} |]

getOptimizeHashingForSpeed :: SessionSettings -> IO Bool
getOptimizeHashingForSpeed ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->optimize_hashing_for_speed } |]

setOptimizeHashingForSpeed :: SessionSettings -> Bool -> IO ()
setOptimizeHashingForSpeed ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->optimize_hashing_for_speed = $(bool val')} |]

getFileChecksDelayPerBlock :: SessionSettings -> IO CInt
getFileChecksDelayPerBlock ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->file_checks_delay_per_block } |]

setFileChecksDelayPerBlock :: SessionSettings -> CInt -> IO ()
setFileChecksDelayPerBlock ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->file_checks_delay_per_block = $(int val)} |]

getDiskCacheAlgorithm :: SessionSettings -> IO DiskCacheAlgo
getDiskCacheAlgorithm ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_cache_algorithm } |]

setDiskCacheAlgorithm :: SessionSettings -> DiskCacheAlgo -> IO ()
setDiskCacheAlgorithm ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_cache_algorithm = (session_settings::disk_cache_algo_t) $(int val')} |]

getReadCacheLineSize :: SessionSettings -> IO CInt
getReadCacheLineSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->read_cache_line_size } |]

setReadCacheLineSize :: SessionSettings -> CInt -> IO ()
setReadCacheLineSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->read_cache_line_size = $(int val)} |]

getWriteCacheLineSize :: SessionSettings -> IO CInt
getWriteCacheLineSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->write_cache_line_size } |]

setWriteCacheLineSize :: SessionSettings -> CInt -> IO ()
setWriteCacheLineSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->write_cache_line_size = $(int val)} |]

getOptimisticDiskRetry :: SessionSettings -> IO CInt
getOptimisticDiskRetry ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->optimistic_disk_retry } |]

setOptimisticDiskRetry :: SessionSettings -> CInt -> IO ()
setOptimisticDiskRetry ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->optimistic_disk_retry = $(int val)} |]

getDisableHashChecks :: SessionSettings -> IO Bool
getDisableHashChecks ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->disable_hash_checks } |]

setDisableHashChecks :: SessionSettings -> Bool -> IO ()
setDisableHashChecks ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->disable_hash_checks = $(bool val')} |]

getAllowReorderedDiskOperations :: SessionSettings -> IO Bool
getAllowReorderedDiskOperations ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_reordered_disk_operations } |]

setAllowReorderedDiskOperations :: SessionSettings -> Bool -> IO ()
setAllowReorderedDiskOperations ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_reordered_disk_operations = $(bool val')} |]

getAllowI2pMixed :: SessionSettings -> IO Bool
getAllowI2pMixed ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_i2p_mixed } |]

setAllowI2pMixed :: SessionSettings -> Bool -> IO ()
setAllowI2pMixed ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_i2p_mixed = $(bool val')} |]

getMaxSuggestPieces :: SessionSettings -> IO CInt
getMaxSuggestPieces ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_suggest_pieces } |]

setMaxSuggestPieces :: SessionSettings -> CInt -> IO ()
setMaxSuggestPieces ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_suggest_pieces = $(int val)} |]

getDropSkippedRequests :: SessionSettings -> IO Bool
getDropSkippedRequests ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->drop_skipped_requests } |]

setDropSkippedRequests :: SessionSettings -> Bool -> IO ()
setDropSkippedRequests ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->drop_skipped_requests = $(bool val')} |]

getLowPrioDisk :: SessionSettings -> IO Bool
getLowPrioDisk ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->low_prio_disk } |]

setLowPrioDisk :: SessionSettings -> Bool -> IO ()
setLowPrioDisk ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->low_prio_disk = $(bool val')} |]

getLocalServiceAnnounceInterval :: SessionSettings -> IO CInt
getLocalServiceAnnounceInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_service_announce_interval } |]

setLocalServiceAnnounceInterval :: SessionSettings -> CInt -> IO ()
setLocalServiceAnnounceInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_service_announce_interval = $(int val)} |]

getDhtAnnounceInterval :: SessionSettings -> IO CInt
getDhtAnnounceInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->dht_announce_interval } |]

setDhtAnnounceInterval :: SessionSettings -> CInt -> IO ()
setDhtAnnounceInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->dht_announce_interval = $(int val)} |]

getUdpTrackerTokenExpiry :: SessionSettings -> IO CInt
getUdpTrackerTokenExpiry ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->udp_tracker_token_expiry } |]

setUdpTrackerTokenExpiry :: SessionSettings -> CInt -> IO ()
setUdpTrackerTokenExpiry ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->udp_tracker_token_expiry = $(int val)} |]

getVolatileReadCache :: SessionSettings -> IO Bool
getVolatileReadCache ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->volatile_read_cache } |]

setVolatileReadCache :: SessionSettings -> Bool -> IO ()
setVolatileReadCache ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->volatile_read_cache = $(bool val')} |]

getGuidedReadCache :: SessionSettings -> IO Bool
getGuidedReadCache ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->guided_read_cache } |]

setGuidedReadCache :: SessionSettings -> Bool -> IO ()
setGuidedReadCache ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->guided_read_cache = $(bool val')} |]

getDefaultCacheMinAge :: SessionSettings -> IO CInt
getDefaultCacheMinAge ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->default_cache_min_age } |]

setDefaultCacheMinAge :: SessionSettings -> CInt -> IO ()
setDefaultCacheMinAge ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->default_cache_min_age = $(int val)} |]

getNumOptimisticUnchokeSlots :: SessionSettings -> IO CInt
getNumOptimisticUnchokeSlots ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->num_optimistic_unchoke_slots } |]

setNumOptimisticUnchokeSlots :: SessionSettings -> CInt -> IO ()
setNumOptimisticUnchokeSlots ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->num_optimistic_unchoke_slots = $(int val)} |]

getNoAtimeStorage :: SessionSettings -> IO Bool
getNoAtimeStorage ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_atime_storage } |]

setNoAtimeStorage :: SessionSettings -> Bool -> IO ()
setNoAtimeStorage ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_atime_storage = $(bool val')} |]

getDefaultEstReciprocationRate :: SessionSettings -> IO CInt
getDefaultEstReciprocationRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->default_est_reciprocation_rate } |]

setDefaultEstReciprocationRate :: SessionSettings -> CInt -> IO ()
setDefaultEstReciprocationRate ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->default_est_reciprocation_rate = $(int val)} |]

getIncreaseEstReciprocationRate :: SessionSettings -> IO CInt
getIncreaseEstReciprocationRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->increase_est_reciprocation_rate } |]

setIncreaseEstReciprocationRate :: SessionSettings -> CInt -> IO ()
setIncreaseEstReciprocationRate ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->increase_est_reciprocation_rate = $(int val)} |]

getDecreaseEstReciprocationRate :: SessionSettings -> IO CInt
getDecreaseEstReciprocationRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->decrease_est_reciprocation_rate } |]

setDecreaseEstReciprocationRate :: SessionSettings -> CInt -> IO ()
setDecreaseEstReciprocationRate ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->decrease_est_reciprocation_rate = $(int val)} |]

getIncomingStartsQueuedTorrents :: SessionSettings -> IO Bool
getIncomingStartsQueuedTorrents ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->incoming_starts_queued_torrents } |]

setIncomingStartsQueuedTorrents :: SessionSettings -> Bool -> IO ()
setIncomingStartsQueuedTorrents ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->incoming_starts_queued_torrents = $(bool val')} |]

getReportTrueDownloaded :: SessionSettings -> IO Bool
getReportTrueDownloaded ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_true_downloaded } |]

setReportTrueDownloaded :: SessionSettings -> Bool -> IO ()
setReportTrueDownloaded ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_true_downloaded = $(bool val')} |]

getStrictEndGameMode :: SessionSettings -> IO Bool
getStrictEndGameMode ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->strict_end_game_mode } |]

setStrictEndGameMode :: SessionSettings -> Bool -> IO ()
setStrictEndGameMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->strict_end_game_mode = $(bool val')} |]

getBroadcastLsd :: SessionSettings -> IO Bool
getBroadcastLsd ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->broadcast_lsd } |]

setBroadcastLsd :: SessionSettings -> Bool -> IO ()
setBroadcastLsd ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->broadcast_lsd = $(bool val')} |]

getEnableOutgoingUtp :: SessionSettings -> IO Bool
getEnableOutgoingUtp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_outgoing_utp } |]

setEnableOutgoingUtp :: SessionSettings -> Bool -> IO ()
setEnableOutgoingUtp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_outgoing_utp = $(bool val')} |]

getEnableIncomingUtp :: SessionSettings -> IO Bool
getEnableIncomingUtp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_incoming_utp } |]

setEnableIncomingUtp :: SessionSettings -> Bool -> IO ()
setEnableIncomingUtp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_incoming_utp = $(bool val')} |]

getEnableOutgoingTcp :: SessionSettings -> IO Bool
getEnableOutgoingTcp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_outgoing_tcp } |]

setEnableOutgoingTcp :: SessionSettings -> Bool -> IO ()
setEnableOutgoingTcp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_outgoing_tcp = $(bool val')} |]

getEnableIncomingTcp :: SessionSettings -> IO Bool
getEnableIncomingTcp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_incoming_tcp } |]

setEnableIncomingTcp :: SessionSettings -> Bool -> IO ()
setEnableIncomingTcp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_incoming_tcp = $(bool val')} |]

getMaxPexPeers :: SessionSettings -> IO CInt
getMaxPexPeers ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_pex_peers } |]

setMaxPexPeers :: SessionSettings -> CInt -> IO ()
setMaxPexPeers ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_pex_peers = $(int val)} |]


getIgnoreResumeTimestamps :: SessionSettings -> IO Bool
getIgnoreResumeTimestamps ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ignore_resume_timestamps } |]

setIgnoreResumeTimestamps :: SessionSettings -> Bool -> IO ()
setIgnoreResumeTimestamps ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ignore_resume_timestamps = $(bool val')} |]

getNoRecheckIncompleteResume :: SessionSettings -> IO Bool
getNoRecheckIncompleteResume ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_recheck_incomplete_resume } |]

setNoRecheckIncompleteResume :: SessionSettings -> Bool -> IO ()
setNoRecheckIncompleteResume ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_recheck_incomplete_resume = $(bool val')} |]

getAnonymousMode :: SessionSettings -> IO Bool
getAnonymousMode ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->anonymous_mode } |]

setAnonymousMode :: SessionSettings -> Bool -> IO ()
setAnonymousMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->anonymous_mode = $(bool val')} |]

getForceProxy :: SessionSettings -> IO Bool
getForceProxy ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->force_proxy } |]

setForceProxy :: SessionSettings -> Bool -> IO ()
setForceProxy ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->force_proxy = $(bool val')} |]

getTickInterval :: SessionSettings -> IO CInt
getTickInterval ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->tick_interval } |]

setTickInterval :: SessionSettings -> CInt -> IO ()
setTickInterval ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->tick_interval = $(int val)} |]

getReportWebSeedDownloads :: SessionSettings -> IO Bool
getReportWebSeedDownloads ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_web_seed_downloads } |]

setReportWebSeedDownloads :: SessionSettings -> Bool -> IO ()
setReportWebSeedDownloads ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_web_seed_downloads = $(bool val')} |]

getShareModeTarget :: SessionSettings -> IO CInt
getShareModeTarget ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->share_mode_target } |]

setShareModeTarget :: SessionSettings -> CInt -> IO ()
setShareModeTarget ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->share_mode_target = $(int val)} |]

getUploadRateLimit :: SessionSettings -> IO CInt
getUploadRateLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->upload_rate_limit } |]

setUploadRateLimit :: SessionSettings -> CInt -> IO ()
setUploadRateLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->upload_rate_limit = $(int val)} |]

getDownloadRateLimit :: SessionSettings -> IO CInt
getDownloadRateLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->download_rate_limit } |]

setDownloadRateLimit :: SessionSettings -> CInt -> IO ()
setDownloadRateLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->download_rate_limit = $(int val)} |]

getLocalUploadRateLimit :: SessionSettings -> IO CInt
getLocalUploadRateLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_upload_rate_limit } |]

setLocalUploadRateLimit :: SessionSettings -> CInt -> IO ()
setLocalUploadRateLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_upload_rate_limit = $(int val)} |]

getLocalDownloadRateLimit :: SessionSettings -> IO CInt
getLocalDownloadRateLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_download_rate_limit } |]

setLocalDownloadRateLimit :: SessionSettings -> CInt -> IO ()
setLocalDownloadRateLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_download_rate_limit = $(int val)} |]

getDhtUploadRateLimit :: SessionSettings -> IO CInt
getDhtUploadRateLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->dht_upload_rate_limit } |]

setDhtUploadRateLimit :: SessionSettings -> CInt -> IO ()
setDhtUploadRateLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->dht_upload_rate_limit = $(int val)} |]

getUnchokeSlotsLimit :: SessionSettings -> IO CInt
getUnchokeSlotsLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->unchoke_slots_limit } |]

setUnchokeSlotsLimit :: SessionSettings -> CInt -> IO ()
setUnchokeSlotsLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->unchoke_slots_limit = $(int val)} |]

getHalfOpenLimit :: SessionSettings -> IO CInt
getHalfOpenLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->half_open_limit } |]

setHalfOpenLimit :: SessionSettings -> CInt -> IO ()
setHalfOpenLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->half_open_limit = $(int val)} |]

getSessionConnectionsLimit :: SessionSettings -> IO CInt
getSessionConnectionsLimit ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->connections_limit } |]

setSessionConnectionsLimit :: SessionSettings -> CInt -> IO ()
setSessionConnectionsLimit ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->connections_limit = $(int val)} |]

getConnectionsSlack :: SessionSettings -> IO CInt
getConnectionsSlack ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->connections_slack } |]

setConnectionsSlack :: SessionSettings -> CInt -> IO ()
setConnectionsSlack ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->connections_slack = $(int val)} |]

getUtpTargetDelay :: SessionSettings -> IO CInt
getUtpTargetDelay ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_target_delay } |]

setUtpTargetDelay :: SessionSettings -> CInt -> IO ()
setUtpTargetDelay ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_target_delay = $(int val)} |]

getUtpGainFactor :: SessionSettings -> IO CInt
getUtpGainFactor ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_gain_factor } |]

setUtpGainFactor :: SessionSettings -> CInt -> IO ()
setUtpGainFactor ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_gain_factor = $(int val)} |]

getUtpMinTimeout :: SessionSettings -> IO CInt
getUtpMinTimeout ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_min_timeout } |]

setUtpMinTimeout :: SessionSettings -> CInt -> IO ()
setUtpMinTimeout ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_min_timeout = $(int val)} |]

getUtpSynResends :: SessionSettings -> IO CInt
getUtpSynResends ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_syn_resends } |]

setUtpSynResends :: SessionSettings -> CInt -> IO ()
setUtpSynResends ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_syn_resends = $(int val)} |]

getUtpFinResends :: SessionSettings -> IO CInt
getUtpFinResends ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_fin_resends } |]

setUtpFinResends :: SessionSettings -> CInt -> IO ()
setUtpFinResends ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_fin_resends = $(int val)} |]

getUtpNumResends :: SessionSettings -> IO CInt
getUtpNumResends ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_num_resends } |]

setUtpNumResends :: SessionSettings -> CInt -> IO ()
setUtpNumResends ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_num_resends = $(int val)} |]

getUtpConnectTimeout :: SessionSettings -> IO CInt
getUtpConnectTimeout ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_connect_timeout } |]

setUtpConnectTimeout :: SessionSettings -> CInt -> IO ()
setUtpConnectTimeout ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_connect_timeout = $(int val)} |]

getUtpDynamicSockBuf :: SessionSettings -> IO Bool
getUtpDynamicSockBuf ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->utp_dynamic_sock_buf } |]

setUtpDynamicSockBuf :: SessionSettings -> Bool -> IO ()
setUtpDynamicSockBuf ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->utp_dynamic_sock_buf = $(bool val')} |]

getUtpLossMultiplier :: SessionSettings -> IO CInt
getUtpLossMultiplier ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_loss_multiplier } |]

setUtpLossMultiplier :: SessionSettings -> CInt -> IO ()
setUtpLossMultiplier ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_loss_multiplier = $(int val)} |]

getMixedModeAlgorithm :: SessionSettings -> IO BandwidthMixedAlgo
getMixedModeAlgorithm ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->mixed_mode_algorithm } |]

setMixedModeAlgorithm :: SessionSettings -> BandwidthMixedAlgo -> IO ()
setMixedModeAlgorithm ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->mixed_mode_algorithm = $(int val')} |]

getRateLimitUtp :: SessionSettings -> IO Bool
getRateLimitUtp ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->rate_limit_utp } |]

setRateLimitUtp :: SessionSettings -> Bool -> IO ()
setRateLimitUtp ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->rate_limit_utp = $(bool val')} |]

getListenQueueSize :: SessionSettings -> IO CInt
getListenQueueSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->listen_queue_size } |]

setListenQueueSize :: SessionSettings -> CInt -> IO ()
setListenQueueSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->listen_queue_size = $(int val)} |]

getAnnounceDoubleNat :: SessionSettings -> IO Bool
getAnnounceDoubleNat ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_double_nat } |]

setAnnounceDoubleNat :: SessionSettings -> Bool -> IO ()
setAnnounceDoubleNat ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_double_nat = $(bool val')} |]

getTorrentConnectBoost :: SessionSettings -> IO CInt
getTorrentConnectBoost ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->torrent_connect_boost } |]

setTorrentConnectBoost :: SessionSettings -> CInt -> IO ()
setTorrentConnectBoost ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->torrent_connect_boost = $(int val)} |]

getSeedingOutgoingConnections :: SessionSettings -> IO Bool
getSeedingOutgoingConnections ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->seeding_outgoing_connections } |]

setSeedingOutgoingConnections :: SessionSettings -> Bool -> IO ()
setSeedingOutgoingConnections ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->seeding_outgoing_connections = $(bool val')} |]

getNoConnectPrivilegedPorts :: SessionSettings -> IO Bool
getNoConnectPrivilegedPorts ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_connect_privileged_ports } |]

setNoConnectPrivilegedPorts :: SessionSettings -> Bool -> IO ()
setNoConnectPrivilegedPorts ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_connect_privileged_ports = $(bool val')} |]

getAlertQueueSize :: SessionSettings -> IO CInt
getAlertQueueSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->alert_queue_size } |]

setAlertQueueSize :: SessionSettings -> CInt -> IO ()
setAlertQueueSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->alert_queue_size = $(int val)} |]

getMaxMetadataSize :: SessionSettings -> IO CInt
getMaxMetadataSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_metadata_size } |]

setMaxMetadataSize :: SessionSettings -> CInt -> IO ()
setMaxMetadataSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_metadata_size = $(int val)} |]

getSmoothConnects :: SessionSettings -> IO Bool
getSmoothConnects ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->smooth_connects } |]

setSmoothConnects :: SessionSettings -> Bool -> IO ()
setSmoothConnects ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->smooth_connects = $(bool val')} |]

getAlwaysSendUserAgent :: SessionSettings -> IO Bool
getAlwaysSendUserAgent ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->always_send_user_agent } |]

setAlwaysSendUserAgent :: SessionSettings -> Bool -> IO ()
setAlwaysSendUserAgent ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->always_send_user_agent = $(bool val')} |]

getApplyIpFilterToTrackers :: SessionSettings -> IO Bool
getApplyIpFilterToTrackers ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->apply_ip_filter_to_trackers } |]

setApplyIpFilterToTrackers :: SessionSettings -> Bool -> IO ()
setApplyIpFilterToTrackers ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->apply_ip_filter_to_trackers = $(bool val')} |]

getReadJobEvery :: SessionSettings -> IO CInt
getReadJobEvery ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->read_job_every } |]

setReadJobEvery :: SessionSettings -> CInt -> IO ()
setReadJobEvery ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->read_job_every = $(int val)} |]

getUseDiskReadAhead :: SessionSettings -> IO Bool
getUseDiskReadAhead ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_disk_read_ahead } |]

setUseDiskReadAhead :: SessionSettings -> Bool -> IO ()
setUseDiskReadAhead ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_disk_read_ahead = $(bool val')} |]

getLockFiles :: SessionSettings -> IO Bool
getLockFiles ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lock_files } |]

setLockFiles :: SessionSettings -> Bool -> IO ()
setLockFiles ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lock_files = $(bool val')} |]

getSslListen :: SessionSettings -> IO CInt
getSslListen ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->ssl_listen } |]

setSslListen :: SessionSettings -> CInt -> IO ()
setSslListen ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->ssl_listen = $(int val)} |]

getTrackerBackoff :: SessionSettings -> IO CInt
getTrackerBackoff ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->tracker_backoff } |]

setTrackerBackoff :: SessionSettings -> CInt -> IO ()
setTrackerBackoff ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->tracker_backoff = $(int val)} |]

getBanWebSeeds :: SessionSettings -> IO Bool
getBanWebSeeds ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ban_web_seeds } |]

setBanWebSeeds :: SessionSettings -> Bool -> IO ()
setBanWebSeeds ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ban_web_seeds = $(bool val')} |]

getMaxHttpRecvBufferSize :: SessionSettings -> IO CInt
getMaxHttpRecvBufferSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_http_recv_buffer_size } |]

setMaxHttpRecvBufferSize :: SessionSettings -> CInt -> IO ()
setMaxHttpRecvBufferSize ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_http_recv_buffer_size = $(int val)} |]

getSupportShareMode :: SessionSettings -> IO Bool
getSupportShareMode ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->support_share_mode } |]

setSupportShareMode :: SessionSettings -> Bool -> IO ()
setSupportShareMode ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->support_share_mode = $(bool val')} |]

getSupportMerkleTorrents :: SessionSettings -> IO Bool
getSupportMerkleTorrents ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->support_merkle_torrents } |]

setSupportMerkleTorrents :: SessionSettings -> Bool -> IO ()
setSupportMerkleTorrents ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->support_merkle_torrents = $(bool val')} |]

getReportRedundantBytes :: SessionSettings -> IO Bool
getReportRedundantBytes ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_redundant_bytes } |]

setReportRedundantBytes :: SessionSettings -> Bool -> IO ()
setReportRedundantBytes ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_redundant_bytes = $(bool val')} |]

getHandshakeClientVersion :: SessionSettings -> IO Text
getHandshakeClientVersion ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->handshake_client_version) } |]
  stdStringToText res

setHandshakeClientVersion :: SessionSettings -> Text -> IO ()
setHandshakeClientVersion ho val =
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->handshake_client_version = std::string($(const char * cstr), $(size_t clen))} |]

getUseDiskCachePool :: SessionSettings -> IO Bool
getUseDiskCachePool ho =
  withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_disk_cache_pool } |]

setUseDiskCachePool :: SessionSettings -> Bool -> IO ()
setUseDiskCachePool ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_disk_cache_pool = $(bool val')} |]

getInactiveDownRate :: SessionSettings -> IO CInt
getInactiveDownRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->inactive_down_rate } |]

setInactiveDownRate :: SessionSettings -> CInt -> IO ()
setInactiveDownRate ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->inactive_down_rate = $(int val)} |]

getInactiveUpRate :: SessionSettings -> IO CInt
getInactiveUpRate ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->inactive_up_rate } |]

setInactiveUpRate :: SessionSettings -> CInt -> IO ()
setInactiveUpRate ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->inactive_up_rate = $(int val)} |]
