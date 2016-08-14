{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | <http://www.libtorrent.org/reference-Core.html#session_settings session_settings> for "Libtorrent"

module Network.Libtorrent.Session.SessionSettings (SessionSettings
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
                                        , minMemoryUsage
                                        , highPerformanceSeed
                                        ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Text                   (Text)
import qualified Data.Text.Foreign           as TF
import           Foreign.C.Types             (CInt)
import           Foreign.ForeignPtr          (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils       (fromBool, toBool)
import           GHC.Generics                (Generic)
import qualified Language.C.Inline           as C
import qualified Language.C.Inline.Cpp       as C
import qualified Language.C.Inline.Unsafe    as CU


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.String
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"
C.include "<libtorrent/session.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data SuggestMode =
  NoPieceSuggestions
  | SuggestReadCache
  deriving (Show, Enum, Bounded, Eq, Generic)

data ChokingAlgorithm =
  FixedSlotsChoker
  | AutoExpandChoker
  | RateBasedChoker
  | BittyrantChoker
  deriving (Show, Enum, Bounded, Eq, Generic)

data SeedChokingAlgorithm =
  RoundRobin
  | FastestUpload
  | AntiLeech
  deriving (Show, Enum, Bounded, Eq, Generic)

data IoBufferMode =
  EnableOsCache
  | DisableOsCacheForAlignedFiles
  | DisableOsCache
  deriving (Show, Enum, Bounded, Eq, Generic)

data DiskCacheAlgo =
  Lru
  | LargestContiguous
  | AvoidReadback
  deriving (Show, Enum, Bounded, Eq, Generic)

data BandwidthMixedAlgo =
  PreferTcp
  | PeerProportional
  deriving (Show, Enum, Bounded, Eq, Generic)


instance ToJSON SuggestMode
instance FromJSON SuggestMode

instance ToJSON   SeedChokingAlgorithm
instance FromJSON SeedChokingAlgorithm

instance ToJSON   ChokingAlgorithm
instance FromJSON ChokingAlgorithm

instance ToJSON   BandwidthMixedAlgo
instance FromJSON BandwidthMixedAlgo

instance ToJSON   IoBufferMode
instance FromJSON IoBufferMode

instance ToJSON   DiskCacheAlgo
instance FromJSON DiskCacheAlgo

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

newSessionSettings :: MonadIO m =>  m SessionSettings
newSessionSettings =
  liftIO$ fromPtr [CU.exp| session_settings * { new session_settings() }|]


getVersion :: MonadIO m =>  SessionSettings -> m CInt
getVersion ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->version } |]

setVersion :: MonadIO m =>  SessionSettings -> CInt -> m ()
setVersion ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->version = $(int val)} |]

getUserAgent :: MonadIO m =>  SessionSettings -> m Text
getUserAgent ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->user_agent) } |]
  stdStringToText res

setUserAgent :: MonadIO m =>  SessionSettings -> Text -> m ()
setUserAgent ho val =
  liftIO .TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->user_agent = std::string($(const char * cstr), $(size_t clen))} |]

getTrackerCompletionTimeout :: MonadIO m =>  SessionSettings -> m CInt
getTrackerCompletionTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_completion_timeout } |]

setTrackerCompletionTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTrackerCompletionTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_completion_timeout = $(int val)} |]

getTrackerReceiveTimeout :: MonadIO m =>  SessionSettings -> m CInt
getTrackerReceiveTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_receive_timeout } |]

setTrackerReceiveTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTrackerReceiveTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_receive_timeout = $(int val)} |]

getStopTrackerTimeout :: MonadIO m =>  SessionSettings -> m CInt
getStopTrackerTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->stop_tracker_timeout } |]

setStopTrackerTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setStopTrackerTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->stop_tracker_timeout = $(int val)} |]

getTrackerMaximumResponseLength :: MonadIO m =>  SessionSettings -> m CInt
getTrackerMaximumResponseLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->tracker_maximum_response_length } |]

setTrackerMaximumResponseLength :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTrackerMaximumResponseLength ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->tracker_maximum_response_length = $(int val)} |]

getPieceTimeout :: MonadIO m =>  SessionSettings -> m CInt
getPieceTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->piece_timeout } |]

setPieceTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setPieceTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->piece_timeout = $(int val)} |]

getRequestTimeout :: MonadIO m =>  SessionSettings -> m CInt
getRequestTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->request_timeout } |]

setRequestTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setRequestTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->request_timeout = $(int val)} |]

getRequestQueueTime :: MonadIO m =>  SessionSettings -> m CInt
getRequestQueueTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->request_queue_time } |]

setRequestQueueTime :: MonadIO m =>  SessionSettings -> CInt -> m ()
setRequestQueueTime ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->request_queue_time = $(int val)} |]

getMaxAllowedInRequestQueue :: MonadIO m =>  SessionSettings -> m CInt
getMaxAllowedInRequestQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_allowed_in_request_queue } |]

setMaxAllowedInRequestQueue :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxAllowedInRequestQueue ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_allowed_in_request_queue = $(int val)} |]

getMaxOutRequestQueue :: MonadIO m =>  SessionSettings -> m CInt
getMaxOutRequestQueue ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_out_request_queue } |]

setMaxOutRequestQueue :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxOutRequestQueue ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_out_request_queue = $(int val)} |]

getWholePiecesThreshold :: MonadIO m =>  SessionSettings -> m CInt
getWholePiecesThreshold ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->whole_pieces_threshold } |]

setWholePiecesThreshold :: MonadIO m =>  SessionSettings -> CInt -> m ()
setWholePiecesThreshold ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->whole_pieces_threshold = $(int val)} |]

getPeerTimeout :: MonadIO m =>  SessionSettings -> m CInt
getPeerTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->peer_timeout } |]

setPeerTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setPeerTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->peer_timeout = $(int val)} |]

getUrlseedTimeout :: MonadIO m =>  SessionSettings -> m CInt
getUrlseedTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_timeout } |]

setUrlseedTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUrlseedTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_timeout = $(int val)} |]

getUrlseedPipelineSize :: MonadIO m =>  SessionSettings -> m CInt
getUrlseedPipelineSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_pipeline_size } |]

setUrlseedPipelineSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUrlseedPipelineSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_pipeline_size = $(int val)} |]

getUrlseedWaitRetry :: MonadIO m =>  SessionSettings -> m CInt
getUrlseedWaitRetry ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->urlseed_wait_retry } |]

setUrlseedWaitRetry :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUrlseedWaitRetry ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->urlseed_wait_retry = $(int val)} |]

getFilePoolSize :: MonadIO m =>  SessionSettings -> m CInt
getFilePoolSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->file_pool_size } |]

setFilePoolSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setFilePoolSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->file_pool_size = $(int val)} |]

getAllowMultipleConnectionsPerIp :: MonadIO m =>  SessionSettings -> m Bool
getAllowMultipleConnectionsPerIp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_multiple_connections_per_ip } |]

setAllowMultipleConnectionsPerIp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAllowMultipleConnectionsPerIp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_multiple_connections_per_ip = $(bool val')} |]

getMaxFailcount :: MonadIO m =>  SessionSettings -> m CInt
getMaxFailcount ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_failcount } |]

setMaxFailcount :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxFailcount ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_failcount = $(int val)} |]

getMinReconnectTime :: MonadIO m =>  SessionSettings -> m CInt
getMinReconnectTime ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->min_reconnect_time } |]

setMinReconnectTime :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMinReconnectTime ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->min_reconnect_time = $(int val)} |]

getPeerConnectTimeout :: MonadIO m =>  SessionSettings -> m CInt
getPeerConnectTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->peer_connect_timeout } |]

setPeerConnectTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setPeerConnectTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->peer_connect_timeout = $(int val)} |]

getIgnoreLimitsOnLocalNetwork :: MonadIO m =>  SessionSettings -> m Bool
getIgnoreLimitsOnLocalNetwork ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ignore_limits_on_local_network } |]

setIgnoreLimitsOnLocalNetwork :: MonadIO m =>  SessionSettings -> Bool -> m ()
setIgnoreLimitsOnLocalNetwork ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ignore_limits_on_local_network = $(bool val')} |]

getConnectionSpeed :: MonadIO m =>  SessionSettings -> m CInt
getConnectionSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->connection_speed } |]

setConnectionSpeed :: MonadIO m =>  SessionSettings -> CInt -> m ()
setConnectionSpeed ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->connection_speed = $(int val)} |]

getSendRedundantHave :: MonadIO m =>  SessionSettings -> m Bool
getSendRedundantHave ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->send_redundant_have } |]

setSendRedundantHave :: MonadIO m =>  SessionSettings -> Bool -> m ()
setSendRedundantHave ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->send_redundant_have = $(bool val')} |]

getLazyBitfields :: MonadIO m =>  SessionSettings -> m Bool
getLazyBitfields ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lazy_bitfields } |]

setLazyBitfields :: MonadIO m =>  SessionSettings -> Bool -> m ()
setLazyBitfields ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lazy_bitfields = $(bool val')} |]

getInactivityTimeout :: MonadIO m =>  SessionSettings -> m CInt
getInactivityTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->inactivity_timeout } |]

setInactivityTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setInactivityTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->inactivity_timeout = $(int val)} |]

getUnchokeInterval :: MonadIO m =>  SessionSettings -> m CInt
getUnchokeInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->unchoke_interval } |]

setUnchokeInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUnchokeInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->unchoke_interval = $(int val)} |]

getOptimisticUnchokeInterval :: MonadIO m =>  SessionSettings -> m CInt
getOptimisticUnchokeInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->optimistic_unchoke_interval } |]

setOptimisticUnchokeInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setOptimisticUnchokeInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->optimistic_unchoke_interval = $(int val)} |]

getAnnounceIp :: MonadIO m =>  SessionSettings -> m Text
getAnnounceIp ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->announce_ip) } |]
  stdStringToText res

setAnnounceIp :: MonadIO m =>  SessionSettings -> Text -> m ()
setAnnounceIp ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    liftIO . withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->announce_ip = std::string($(const char * cstr), $(size_t clen))} |]

getNumWant :: MonadIO m =>  SessionSettings -> m CInt
getNumWant ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->num_want } |]

setNumWant :: MonadIO m =>  SessionSettings -> CInt -> m ()
setNumWant ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->num_want = $(int val)} |]

getInitialPickerThreshold :: MonadIO m =>  SessionSettings -> m CInt
getInitialPickerThreshold ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->initial_picker_threshold } |]

setInitialPickerThreshold :: MonadIO m =>  SessionSettings -> CInt -> m ()
setInitialPickerThreshold ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->initial_picker_threshold = $(int val)} |]

getAllowedFastSetSize :: MonadIO m =>  SessionSettings -> m CInt
getAllowedFastSetSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->allowed_fast_set_size } |]

setAllowedFastSetSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAllowedFastSetSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->allowed_fast_set_size = $(int val)} |]

getSuggestMode :: MonadIO m =>  SessionSettings -> m SuggestMode
getSuggestMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->suggest_mode } |]

setSuggestMode :: MonadIO m =>  SessionSettings -> SuggestMode -> m ()
setSuggestMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->suggest_mode = $(int val')} |]

getMaxQueuedDiskBytes :: MonadIO m =>  SessionSettings -> m CInt
getMaxQueuedDiskBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_queued_disk_bytes } |]

setMaxQueuedDiskBytes :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxQueuedDiskBytes ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_queued_disk_bytes = $(int val)} |]

getMaxQueuedDiskBytesLowWatermark :: MonadIO m =>  SessionSettings -> m CInt
getMaxQueuedDiskBytesLowWatermark ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->max_queued_disk_bytes_low_watermark } |]

setMaxQueuedDiskBytesLowWatermark :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxQueuedDiskBytesLowWatermark ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->max_queued_disk_bytes_low_watermark = $(int val)} |]

getHandshakeTimeout :: MonadIO m =>  SessionSettings -> m CInt
getHandshakeTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->handshake_timeout } |]

setHandshakeTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setHandshakeTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->handshake_timeout = $(int val)} |]

getUseDhtAsFallback :: MonadIO m =>  SessionSettings -> m Bool
getUseDhtAsFallback ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_dht_as_fallback } |]

setUseDhtAsFallback :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUseDhtAsFallback ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_dht_as_fallback = $(bool val')} |]

getFreeTorrentHashes :: MonadIO m =>  SessionSettings -> m Bool
getFreeTorrentHashes ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->free_torrent_hashes } |]

setFreeTorrentHashes :: MonadIO m =>  SessionSettings -> Bool -> m ()
setFreeTorrentHashes ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->free_torrent_hashes = $(bool val')} |]

getUpnpIgnoreNonrouters :: MonadIO m =>  SessionSettings -> m Bool
getUpnpIgnoreNonrouters ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->upnp_ignore_nonrouters } |]

setUpnpIgnoreNonrouters :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUpnpIgnoreNonrouters ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->upnp_ignore_nonrouters = $(bool val')} |]

getSendBufferLowWatermark :: MonadIO m =>  SessionSettings -> m CInt
getSendBufferLowWatermark ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_low_watermark } |]

setSendBufferLowWatermark :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSendBufferLowWatermark ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_low_watermark = $(int val)} |]

getSendBufferWatermark :: MonadIO m =>  SessionSettings -> m CInt
getSendBufferWatermark ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_watermark } |]

setSendBufferWatermark :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSendBufferWatermark ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_watermark = $(int val)} |]

getSendBufferWatermarkFactor :: MonadIO m =>  SessionSettings -> m CInt
getSendBufferWatermarkFactor ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->send_buffer_watermark_factor } |]

setSendBufferWatermarkFactor :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSendBufferWatermarkFactor ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->send_buffer_watermark_factor = $(int val)} |]

getChokingAlgorithm :: MonadIO m =>  SessionSettings -> m ChokingAlgorithm
getChokingAlgorithm ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->choking_algorithm } |]

setChokingAlgorithm :: MonadIO m =>  SessionSettings -> ChokingAlgorithm -> m ()
setChokingAlgorithm ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->choking_algorithm = $(int val')} |]

getSeedChokingAlgorithm :: MonadIO m =>  SessionSettings -> m SeedChokingAlgorithm
getSeedChokingAlgorithm ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->seed_choking_algorithm } |]

setSeedChokingAlgorithm :: MonadIO m =>  SessionSettings -> SeedChokingAlgorithm -> m ()
setSeedChokingAlgorithm ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->seed_choking_algorithm = $(int val')} |]

getUseParoleMode :: MonadIO m =>  SessionSettings -> m Bool
getUseParoleMode ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_parole_mode } |]

setUseParoleMode :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUseParoleMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_parole_mode = $(bool val')} |]

getCacheSize :: MonadIO m =>  SessionSettings -> m CInt
getCacheSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_size } |]

setCacheSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setCacheSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_size = $(int val)} |]

getCacheBufferChunkSize :: MonadIO m =>  SessionSettings -> m CInt
getCacheBufferChunkSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_buffer_chunk_size } |]

setCacheBufferChunkSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setCacheBufferChunkSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_buffer_chunk_size = $(int val)} |]

getCacheExpiry :: MonadIO m =>  SessionSettings -> m CInt
getCacheExpiry ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->cache_expiry } |]

setCacheExpiry :: MonadIO m =>  SessionSettings -> CInt -> m ()
setCacheExpiry ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->cache_expiry = $(int val)} |]

getUseReadCache :: MonadIO m =>  SessionSettings -> m Bool
getUseReadCache ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_read_cache } |]

setUseReadCache :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUseReadCache ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_read_cache = $(bool val')} |]

getExplicitReadCache :: MonadIO m =>  SessionSettings -> m Bool
getExplicitReadCache ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->explicit_read_cache } |]

setExplicitReadCache :: MonadIO m =>  SessionSettings -> Bool -> m ()
setExplicitReadCache ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->explicit_read_cache = $(bool val')} |]

getExplicitCacheInterval :: MonadIO m =>  SessionSettings -> m CInt
getExplicitCacheInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->explicit_cache_interval } |]

setExplicitCacheInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setExplicitCacheInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->explicit_cache_interval = $(int val)} |]

getDiskIoWriteMode :: MonadIO m =>  SessionSettings -> m IoBufferMode
getDiskIoWriteMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_io_write_mode } |]

setDiskIoWriteMode :: MonadIO m =>  SessionSettings -> IoBufferMode -> m ()
setDiskIoWriteMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_io_write_mode = $(int val')} |]

getDiskIoReadMode :: MonadIO m =>  SessionSettings -> m IoBufferMode
getDiskIoReadMode ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_io_read_mode } |]

setDiskIoReadMode :: MonadIO m =>  SessionSettings -> IoBufferMode -> m ()
setDiskIoReadMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_io_read_mode = $(int val')} |]

getCoalesceReads :: MonadIO m =>  SessionSettings -> m Bool
getCoalesceReads ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->coalesce_reads } |]

setCoalesceReads :: MonadIO m =>  SessionSettings -> Bool -> m ()
setCoalesceReads ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->coalesce_reads = $(bool val')} |]

getCoalesceWrites :: MonadIO m =>  SessionSettings -> m Bool
getCoalesceWrites ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->coalesce_writes } |]

setCoalesceWrites :: MonadIO m =>  SessionSettings -> Bool -> m ()
setCoalesceWrites ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->coalesce_writes = $(bool val')} |]

getOutgoingPorts :: MonadIO m =>  SessionSettings -> m (CInt, CInt)
getOutgoingPorts ho =
  liftIO . withPtr ho $ \hoPtr ->
  C.withPtrs_ $ \(fromPtr', toPtr') ->
  [CU.block| void {
      *$(int * fromPtr') = $(session_settings * hoPtr)->outgoing_ports.first;
      *$(int * toPtr') = $(session_settings * hoPtr)->outgoing_ports.second;
     }
  |]

setOutgoingPorts :: MonadIO m =>  SessionSettings -> (CInt, CInt) -> m ()
setOutgoingPorts ho (fromPtr', toPtr') =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->outgoing_ports = std::make_pair($(int fromPtr'), $(int toPtr')) } |]

getPeerTos :: MonadIO m =>  SessionSettings -> m C.CChar
getPeerTos ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| char { $(session_settings * hoPtr)->peer_tos } |]

setPeerTos :: MonadIO m =>  SessionSettings -> C.CChar -> m ()
setPeerTos ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_tos = $(char val)} |]

getActiveDownloads :: MonadIO m =>  SessionSettings -> m CInt
getActiveDownloads ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_downloads } |]

setActiveDownloads :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveDownloads ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_downloads = $(int val)} |]

getActiveSeeds :: MonadIO m =>  SessionSettings -> m CInt
getActiveSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_seeds } |]

setActiveSeeds :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveSeeds ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_seeds = $(int val)} |]

getActiveDhtLimit :: MonadIO m =>  SessionSettings -> m CInt
getActiveDhtLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->active_dht_limit } |]

setActiveDhtLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveDhtLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session_settings * hoPtr)->active_dht_limit = $(int val)} |]

getActiveTrackerLimit :: MonadIO m =>  SessionSettings -> m CInt
getActiveTrackerLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_tracker_limit } |]

setActiveTrackerLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveTrackerLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_tracker_limit = $(int val)} |]

getActiveLsdLimit :: MonadIO m =>  SessionSettings -> m CInt
getActiveLsdLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_lsd_limit } |]

setActiveLsdLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveLsdLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_lsd_limit = $(int val)} |]

getActiveLimit :: MonadIO m =>  SessionSettings -> m CInt
getActiveLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->active_limit } |]

setActiveLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setActiveLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->active_limit = $(int val)} |]

getAutoManagePreferSeeds :: MonadIO m =>  SessionSettings -> m Bool
getAutoManagePreferSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->auto_manage_prefer_seeds } |]

setAutoManagePreferSeeds :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAutoManagePreferSeeds ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->auto_manage_prefer_seeds = $(bool val')} |]

getDontCountSlowTorrents :: MonadIO m =>  SessionSettings -> m Bool
getDontCountSlowTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->dont_count_slow_torrents } |]

setDontCountSlowTorrents :: MonadIO m =>  SessionSettings -> Bool -> m ()
setDontCountSlowTorrents ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->dont_count_slow_torrents = $(bool val')} |]

getAutoManageInterval :: MonadIO m =>  SessionSettings -> m CInt
getAutoManageInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_manage_interval } |]

setAutoManageInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAutoManageInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_manage_interval = $(int val)} |]

getShareRatioLimit :: MonadIO m =>  SessionSettings -> m C.CFloat
getShareRatioLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->share_ratio_limit } |]

setShareRatioLimit :: MonadIO m =>  SessionSettings -> C.CFloat -> m ()
setShareRatioLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->share_ratio_limit = $(float val)} |]

getSeedTimeRatioLimit :: MonadIO m =>  SessionSettings -> m C.CFloat
getSeedTimeRatioLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->seed_time_ratio_limit } |]

setSeedTimeRatioLimit :: MonadIO m =>  SessionSettings -> C.CFloat -> m ()
setSeedTimeRatioLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seed_time_ratio_limit = $(float val)} |]

getSeedTimeLimit :: MonadIO m =>  SessionSettings -> m CInt
getSeedTimeLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->seed_time_limit } |]

setSeedTimeLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSeedTimeLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seed_time_limit = $(int val)} |]

getPeerTurnoverInterval :: MonadIO m =>  SessionSettings -> m CInt
getPeerTurnoverInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->peer_turnover_interval } |]

setPeerTurnoverInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setPeerTurnoverInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover_interval = $(int val)} |]

getPeerTurnover :: MonadIO m =>  SessionSettings -> m C.CFloat
getPeerTurnover ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->peer_turnover } |]

setPeerTurnover :: MonadIO m =>  SessionSettings -> C.CFloat -> m ()
setPeerTurnover ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover = $(float val)} |]

getPeerTurnoverCutoff :: MonadIO m =>  SessionSettings -> m C.CFloat
getPeerTurnoverCutoff ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| float { $(session_settings * hoPtr)->peer_turnover_cutoff } |]

setPeerTurnoverCutoff :: MonadIO m =>  SessionSettings -> C.CFloat -> m ()
setPeerTurnoverCutoff ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->peer_turnover_cutoff = $(float val)} |]

getCloseRedundantConnections :: MonadIO m =>  SessionSettings -> m Bool
getCloseRedundantConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->close_redundant_connections } |]

setCloseRedundantConnections :: MonadIO m =>  SessionSettings -> Bool -> m ()
setCloseRedundantConnections ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->close_redundant_connections = $(bool val')} |]

getAutoScrapeInterval :: MonadIO m =>  SessionSettings -> m CInt
getAutoScrapeInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_scrape_interval } |]

setAutoScrapeInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAutoScrapeInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_scrape_interval = $(int val)} |]

getAutoScrapeMinInterval :: MonadIO m =>  SessionSettings -> m CInt
getAutoScrapeMinInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_scrape_min_interval } |]

setAutoScrapeMinInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAutoScrapeMinInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_scrape_min_interval = $(int val)} |]

getMaxPeerlistSize :: MonadIO m =>  SessionSettings -> m CInt
getMaxPeerlistSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_peerlist_size } |]

setMaxPeerlistSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxPeerlistSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_peerlist_size = $(int val)} |]

getMaxPausedPeerlistSize :: MonadIO m =>  SessionSettings -> m CInt
getMaxPausedPeerlistSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_paused_peerlist_size } |]

setMaxPausedPeerlistSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxPausedPeerlistSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_paused_peerlist_size = $(int val)} |]

getMinAnnounceInterval :: MonadIO m =>  SessionSettings -> m CInt
getMinAnnounceInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->min_announce_interval } |]

setMinAnnounceInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMinAnnounceInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->min_announce_interval = $(int val)} |]

getPrioritizePartialPieces :: MonadIO m =>  SessionSettings -> m Bool
getPrioritizePartialPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->prioritize_partial_pieces } |]

setPrioritizePartialPieces :: MonadIO m =>  SessionSettings -> Bool -> m ()
setPrioritizePartialPieces ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->prioritize_partial_pieces = $(bool val')} |]

getAutoManageStartup :: MonadIO m =>  SessionSettings -> m CInt
getAutoManageStartup ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->auto_manage_startup } |]

setAutoManageStartup :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAutoManageStartup ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->auto_manage_startup = $(int val)} |]

getRateLimitIpOverhead :: MonadIO m =>  SessionSettings -> m Bool
getRateLimitIpOverhead ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->rate_limit_ip_overhead } |]

setRateLimitIpOverhead :: MonadIO m =>  SessionSettings -> Bool -> m ()
setRateLimitIpOverhead ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->rate_limit_ip_overhead = $(bool val')} |]

getAnnounceToAllTrackers :: MonadIO m =>  SessionSettings -> m Bool
getAnnounceToAllTrackers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_to_all_trackers } |]

setAnnounceToAllTrackers :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAnnounceToAllTrackers ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_to_all_trackers = $(bool val')} |]

getAnnounceToAllTiers :: MonadIO m =>  SessionSettings -> m Bool
getAnnounceToAllTiers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_to_all_tiers } |]

setAnnounceToAllTiers :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAnnounceToAllTiers ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_to_all_tiers = $(bool val')} |]

getPreferUdpTrackers :: MonadIO m =>  SessionSettings -> m Bool
getPreferUdpTrackers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->prefer_udp_trackers } |]

setPreferUdpTrackers :: MonadIO m =>  SessionSettings -> Bool -> m ()
setPreferUdpTrackers ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->prefer_udp_trackers = $(bool val')} |]

getStrictSuperSeeding :: MonadIO m =>  SessionSettings -> m Bool
getStrictSuperSeeding ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->strict_super_seeding } |]

setStrictSuperSeeding :: MonadIO m =>  SessionSettings -> Bool -> m ()
setStrictSuperSeeding ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->strict_super_seeding = $(bool val')} |]

getSeedingPieceQuota :: MonadIO m =>  SessionSettings -> m CInt
getSeedingPieceQuota ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->seeding_piece_quota } |]

setSeedingPieceQuota :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSeedingPieceQuota ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->seeding_piece_quota = $(int val)} |]

getMaxSparseRegions :: MonadIO m =>  SessionSettings -> m CInt
getMaxSparseRegions ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_sparse_regions } |]

setMaxSparseRegions :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxSparseRegions ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_sparse_regions = $(int val)} |]

getLockDiskCache :: MonadIO m =>  SessionSettings -> m Bool
getLockDiskCache ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lock_disk_cache } |]

setLockDiskCache :: MonadIO m =>  SessionSettings -> Bool -> m ()
setLockDiskCache ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lock_disk_cache = $(bool val')} |]

getMaxRejects :: MonadIO m =>  SessionSettings -> m CInt
getMaxRejects ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_rejects } |]

setMaxRejects :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxRejects ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_rejects = $(int val)} |]

getRecvSocketBufferSize :: MonadIO m =>  SessionSettings -> m CInt
getRecvSocketBufferSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->recv_socket_buffer_size } |]

setRecvSocketBufferSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setRecvSocketBufferSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->recv_socket_buffer_size = $(int val)} |]

getSendSocketBufferSize :: MonadIO m =>  SessionSettings -> m CInt
getSendSocketBufferSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->send_socket_buffer_size } |]

setSendSocketBufferSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSendSocketBufferSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->send_socket_buffer_size = $(int val)} |]

getOptimizeHashingForSpeed :: MonadIO m =>  SessionSettings -> m Bool
getOptimizeHashingForSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->optimize_hashing_for_speed } |]

setOptimizeHashingForSpeed :: MonadIO m =>  SessionSettings -> Bool -> m ()
setOptimizeHashingForSpeed ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->optimize_hashing_for_speed = $(bool val')} |]

getFileChecksDelayPerBlock :: MonadIO m =>  SessionSettings -> m CInt
getFileChecksDelayPerBlock ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->file_checks_delay_per_block } |]

setFileChecksDelayPerBlock :: MonadIO m =>  SessionSettings -> CInt -> m ()
setFileChecksDelayPerBlock ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->file_checks_delay_per_block = $(int val)} |]

getDiskCacheAlgorithm :: MonadIO m =>  SessionSettings -> m DiskCacheAlgo
getDiskCacheAlgorithm ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->disk_cache_algorithm } |]

setDiskCacheAlgorithm :: MonadIO m =>  SessionSettings -> DiskCacheAlgo -> m ()
setDiskCacheAlgorithm ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->disk_cache_algorithm = (session_settings::disk_cache_algo_t) $(int val')} |]

getReadCacheLineSize :: MonadIO m =>  SessionSettings -> m CInt
getReadCacheLineSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(session_settings * hoPtr)->read_cache_line_size } |]

setReadCacheLineSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setReadCacheLineSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->read_cache_line_size = $(int val)} |]

getWriteCacheLineSize :: MonadIO m =>  SessionSettings -> m CInt
getWriteCacheLineSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->write_cache_line_size } |]

setWriteCacheLineSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setWriteCacheLineSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->write_cache_line_size = $(int val)} |]

getOptimisticDiskRetry :: MonadIO m =>  SessionSettings -> m CInt
getOptimisticDiskRetry ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->optimistic_disk_retry } |]

setOptimisticDiskRetry :: MonadIO m =>  SessionSettings -> CInt -> m ()
setOptimisticDiskRetry ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->optimistic_disk_retry = $(int val)} |]

getDisableHashChecks :: MonadIO m =>  SessionSettings -> m Bool
getDisableHashChecks ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->disable_hash_checks } |]

setDisableHashChecks :: MonadIO m =>  SessionSettings -> Bool -> m ()
setDisableHashChecks ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->disable_hash_checks = $(bool val')} |]

getAllowReorderedDiskOperations :: MonadIO m =>  SessionSettings -> m Bool
getAllowReorderedDiskOperations ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_reordered_disk_operations } |]

setAllowReorderedDiskOperations :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAllowReorderedDiskOperations ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_reordered_disk_operations = $(bool val')} |]

getAllowI2pMixed :: MonadIO m =>  SessionSettings -> m Bool
getAllowI2pMixed ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->allow_i2p_mixed } |]

setAllowI2pMixed :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAllowI2pMixed ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->allow_i2p_mixed = $(bool val')} |]

getMaxSuggestPieces :: MonadIO m =>  SessionSettings -> m CInt
getMaxSuggestPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_suggest_pieces } |]

setMaxSuggestPieces :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxSuggestPieces ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_suggest_pieces = $(int val)} |]

getDropSkippedRequests :: MonadIO m =>  SessionSettings -> m Bool
getDropSkippedRequests ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->drop_skipped_requests } |]

setDropSkippedRequests :: MonadIO m =>  SessionSettings -> Bool -> m ()
setDropSkippedRequests ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->drop_skipped_requests = $(bool val')} |]

getLowPrioDisk :: MonadIO m =>  SessionSettings -> m Bool
getLowPrioDisk ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->low_prio_disk } |]

setLowPrioDisk :: MonadIO m =>  SessionSettings -> Bool -> m ()
setLowPrioDisk ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->low_prio_disk = $(bool val')} |]

getLocalServiceAnnounceInterval :: MonadIO m =>  SessionSettings -> m CInt
getLocalServiceAnnounceInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_service_announce_interval } |]

setLocalServiceAnnounceInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setLocalServiceAnnounceInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_service_announce_interval = $(int val)} |]

getDhtAnnounceInterval :: MonadIO m =>  SessionSettings -> m CInt
getDhtAnnounceInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->dht_announce_interval } |]

setDhtAnnounceInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDhtAnnounceInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->dht_announce_interval = $(int val)} |]

getUdpTrackerTokenExpiry :: MonadIO m =>  SessionSettings -> m CInt
getUdpTrackerTokenExpiry ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->udp_tracker_token_expiry } |]

setUdpTrackerTokenExpiry :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUdpTrackerTokenExpiry ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->udp_tracker_token_expiry = $(int val)} |]

getVolatileReadCache :: MonadIO m =>  SessionSettings -> m Bool
getVolatileReadCache ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->volatile_read_cache } |]

setVolatileReadCache :: MonadIO m =>  SessionSettings -> Bool -> m ()
setVolatileReadCache ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->volatile_read_cache = $(bool val')} |]

getGuidedReadCache :: MonadIO m =>  SessionSettings -> m Bool
getGuidedReadCache ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->guided_read_cache } |]

setGuidedReadCache :: MonadIO m =>  SessionSettings -> Bool -> m ()
setGuidedReadCache ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->guided_read_cache = $(bool val')} |]

getDefaultCacheMinAge :: MonadIO m =>  SessionSettings -> m CInt
getDefaultCacheMinAge ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->default_cache_min_age } |]

setDefaultCacheMinAge :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDefaultCacheMinAge ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->default_cache_min_age = $(int val)} |]

getNumOptimisticUnchokeSlots :: MonadIO m =>  SessionSettings -> m CInt
getNumOptimisticUnchokeSlots ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->num_optimistic_unchoke_slots } |]

setNumOptimisticUnchokeSlots :: MonadIO m =>  SessionSettings -> CInt -> m ()
setNumOptimisticUnchokeSlots ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->num_optimistic_unchoke_slots = $(int val)} |]

getNoAtimeStorage :: MonadIO m =>  SessionSettings -> m Bool
getNoAtimeStorage ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_atime_storage } |]

setNoAtimeStorage :: MonadIO m =>  SessionSettings -> Bool -> m ()
setNoAtimeStorage ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_atime_storage = $(bool val')} |]

getDefaultEstReciprocationRate :: MonadIO m =>  SessionSettings -> m CInt
getDefaultEstReciprocationRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->default_est_reciprocation_rate } |]

setDefaultEstReciprocationRate :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDefaultEstReciprocationRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->default_est_reciprocation_rate = $(int val)} |]

getIncreaseEstReciprocationRate :: MonadIO m =>  SessionSettings -> m CInt
getIncreaseEstReciprocationRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->increase_est_reciprocation_rate } |]

setIncreaseEstReciprocationRate :: MonadIO m =>  SessionSettings -> CInt -> m ()
setIncreaseEstReciprocationRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->increase_est_reciprocation_rate = $(int val)} |]

getDecreaseEstReciprocationRate :: MonadIO m =>  SessionSettings -> m CInt
getDecreaseEstReciprocationRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->decrease_est_reciprocation_rate } |]

setDecreaseEstReciprocationRate :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDecreaseEstReciprocationRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->decrease_est_reciprocation_rate = $(int val)} |]

getIncomingStartsQueuedTorrents :: MonadIO m =>  SessionSettings -> m Bool
getIncomingStartsQueuedTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->incoming_starts_queued_torrents } |]

setIncomingStartsQueuedTorrents :: MonadIO m =>  SessionSettings -> Bool -> m ()
setIncomingStartsQueuedTorrents ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->incoming_starts_queued_torrents = $(bool val')} |]

getReportTrueDownloaded :: MonadIO m =>  SessionSettings -> m Bool
getReportTrueDownloaded ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_true_downloaded } |]

setReportTrueDownloaded :: MonadIO m =>  SessionSettings -> Bool -> m ()
setReportTrueDownloaded ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_true_downloaded = $(bool val')} |]

getStrictEndGameMode :: MonadIO m =>  SessionSettings -> m Bool
getStrictEndGameMode ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->strict_end_game_mode } |]

setStrictEndGameMode :: MonadIO m =>  SessionSettings -> Bool -> m ()
setStrictEndGameMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->strict_end_game_mode = $(bool val')} |]

getBroadcastLsd :: MonadIO m =>  SessionSettings -> m Bool
getBroadcastLsd ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->broadcast_lsd } |]

setBroadcastLsd :: MonadIO m =>  SessionSettings -> Bool -> m ()
setBroadcastLsd ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->broadcast_lsd = $(bool val')} |]

getEnableOutgoingUtp :: MonadIO m =>  SessionSettings -> m Bool
getEnableOutgoingUtp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_outgoing_utp } |]

setEnableOutgoingUtp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setEnableOutgoingUtp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_outgoing_utp = $(bool val')} |]

getEnableIncomingUtp :: MonadIO m =>  SessionSettings -> m Bool
getEnableIncomingUtp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_incoming_utp } |]

setEnableIncomingUtp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setEnableIncomingUtp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_incoming_utp = $(bool val')} |]

getEnableOutgoingTcp :: MonadIO m =>  SessionSettings -> m Bool
getEnableOutgoingTcp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_outgoing_tcp } |]

setEnableOutgoingTcp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setEnableOutgoingTcp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_outgoing_tcp = $(bool val')} |]

getEnableIncomingTcp :: MonadIO m =>  SessionSettings -> m Bool
getEnableIncomingTcp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->enable_incoming_tcp } |]

setEnableIncomingTcp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setEnableIncomingTcp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->enable_incoming_tcp = $(bool val')} |]

getMaxPexPeers :: MonadIO m =>  SessionSettings -> m CInt
getMaxPexPeers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_pex_peers } |]

setMaxPexPeers :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxPexPeers ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_pex_peers = $(int val)} |]


getIgnoreResumeTimestamps :: MonadIO m =>  SessionSettings -> m Bool
getIgnoreResumeTimestamps ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ignore_resume_timestamps } |]

setIgnoreResumeTimestamps :: MonadIO m =>  SessionSettings -> Bool -> m ()
setIgnoreResumeTimestamps ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ignore_resume_timestamps = $(bool val')} |]

getNoRecheckIncompleteResume :: MonadIO m =>  SessionSettings -> m Bool
getNoRecheckIncompleteResume ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_recheck_incomplete_resume } |]

setNoRecheckIncompleteResume :: MonadIO m =>  SessionSettings -> Bool -> m ()
setNoRecheckIncompleteResume ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_recheck_incomplete_resume = $(bool val')} |]

getAnonymousMode :: MonadIO m =>  SessionSettings -> m Bool
getAnonymousMode ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->anonymous_mode } |]

setAnonymousMode :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAnonymousMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->anonymous_mode = $(bool val')} |]

getForceProxy :: MonadIO m =>  SessionSettings -> m Bool
getForceProxy ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->force_proxy } |]

setForceProxy :: MonadIO m =>  SessionSettings -> Bool -> m ()
setForceProxy ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->force_proxy = $(bool val')} |]

getTickInterval :: MonadIO m =>  SessionSettings -> m CInt
getTickInterval ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->tick_interval } |]

setTickInterval :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTickInterval ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->tick_interval = $(int val)} |]

getReportWebSeedDownloads :: MonadIO m =>  SessionSettings -> m Bool
getReportWebSeedDownloads ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_web_seed_downloads } |]

setReportWebSeedDownloads :: MonadIO m =>  SessionSettings -> Bool -> m ()
setReportWebSeedDownloads ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_web_seed_downloads = $(bool val')} |]

getShareModeTarget :: MonadIO m =>  SessionSettings -> m CInt
getShareModeTarget ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->share_mode_target } |]

setShareModeTarget :: MonadIO m =>  SessionSettings -> CInt -> m ()
setShareModeTarget ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->share_mode_target = $(int val)} |]

getUploadRateLimit :: MonadIO m =>  SessionSettings -> m CInt
getUploadRateLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->upload_rate_limit } |]

setUploadRateLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUploadRateLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->upload_rate_limit = $(int val)} |]

getDownloadRateLimit :: MonadIO m =>  SessionSettings -> m CInt
getDownloadRateLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->download_rate_limit } |]

setDownloadRateLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDownloadRateLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->download_rate_limit = $(int val)} |]

getLocalUploadRateLimit :: MonadIO m =>  SessionSettings -> m CInt
getLocalUploadRateLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_upload_rate_limit } |]

setLocalUploadRateLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setLocalUploadRateLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_upload_rate_limit = $(int val)} |]

getLocalDownloadRateLimit :: MonadIO m =>  SessionSettings -> m CInt
getLocalDownloadRateLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->local_download_rate_limit } |]

setLocalDownloadRateLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setLocalDownloadRateLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->local_download_rate_limit = $(int val)} |]

getDhtUploadRateLimit :: MonadIO m =>  SessionSettings -> m CInt
getDhtUploadRateLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->dht_upload_rate_limit } |]

setDhtUploadRateLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setDhtUploadRateLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->dht_upload_rate_limit = $(int val)} |]

getUnchokeSlotsLimit :: MonadIO m =>  SessionSettings -> m CInt
getUnchokeSlotsLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->unchoke_slots_limit } |]

setUnchokeSlotsLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUnchokeSlotsLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->unchoke_slots_limit = $(int val)} |]

getHalfOpenLimit :: MonadIO m =>  SessionSettings -> m CInt
getHalfOpenLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->half_open_limit } |]

setHalfOpenLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setHalfOpenLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->half_open_limit = $(int val)} |]

getSessionConnectionsLimit :: MonadIO m =>  SessionSettings -> m CInt
getSessionConnectionsLimit ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->connections_limit } |]

setSessionConnectionsLimit :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSessionConnectionsLimit ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->connections_limit = $(int val)} |]

getConnectionsSlack :: MonadIO m =>  SessionSettings -> m CInt
getConnectionsSlack ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->connections_slack } |]

setConnectionsSlack :: MonadIO m =>  SessionSettings -> CInt -> m ()
setConnectionsSlack ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->connections_slack = $(int val)} |]

getUtpTargetDelay :: MonadIO m =>  SessionSettings -> m CInt
getUtpTargetDelay ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_target_delay } |]

setUtpTargetDelay :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpTargetDelay ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_target_delay = $(int val)} |]

getUtpGainFactor :: MonadIO m =>  SessionSettings -> m CInt
getUtpGainFactor ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_gain_factor } |]

setUtpGainFactor :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpGainFactor ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_gain_factor = $(int val)} |]

getUtpMinTimeout :: MonadIO m =>  SessionSettings -> m CInt
getUtpMinTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_min_timeout } |]

setUtpMinTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpMinTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_min_timeout = $(int val)} |]

getUtpSynResends :: MonadIO m =>  SessionSettings -> m CInt
getUtpSynResends ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_syn_resends } |]

setUtpSynResends :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpSynResends ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_syn_resends = $(int val)} |]

getUtpFinResends :: MonadIO m =>  SessionSettings -> m CInt
getUtpFinResends ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_fin_resends } |]

setUtpFinResends :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpFinResends ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_fin_resends = $(int val)} |]

getUtpNumResends :: MonadIO m =>  SessionSettings -> m CInt
getUtpNumResends ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_num_resends } |]

setUtpNumResends :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpNumResends ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_num_resends = $(int val)} |]

getUtpConnectTimeout :: MonadIO m =>  SessionSettings -> m CInt
getUtpConnectTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_connect_timeout } |]

setUtpConnectTimeout :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpConnectTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_connect_timeout = $(int val)} |]

getUtpDynamicSockBuf :: MonadIO m =>  SessionSettings -> m Bool
getUtpDynamicSockBuf ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->utp_dynamic_sock_buf } |]

setUtpDynamicSockBuf :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUtpDynamicSockBuf ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->utp_dynamic_sock_buf = $(bool val')} |]

getUtpLossMultiplier :: MonadIO m =>  SessionSettings -> m CInt
getUtpLossMultiplier ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->utp_loss_multiplier } |]

setUtpLossMultiplier :: MonadIO m =>  SessionSettings -> CInt -> m ()
setUtpLossMultiplier ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->utp_loss_multiplier = $(int val)} |]

getMixedModeAlgorithm :: MonadIO m =>  SessionSettings -> m BandwidthMixedAlgo
getMixedModeAlgorithm ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| int { $(session_settings * hoPtr)->mixed_mode_algorithm } |]

setMixedModeAlgorithm :: MonadIO m =>  SessionSettings -> BandwidthMixedAlgo -> m ()
setMixedModeAlgorithm ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(session_settings * hoPtr)->mixed_mode_algorithm = $(int val')} |]

getRateLimitUtp :: MonadIO m =>  SessionSettings -> m Bool
getRateLimitUtp ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->rate_limit_utp } |]

setRateLimitUtp :: MonadIO m =>  SessionSettings -> Bool -> m ()
setRateLimitUtp ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->rate_limit_utp = $(bool val')} |]

getListenQueueSize :: MonadIO m =>  SessionSettings -> m CInt
getListenQueueSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->listen_queue_size } |]

setListenQueueSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setListenQueueSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->listen_queue_size = $(int val)} |]

getAnnounceDoubleNat :: MonadIO m =>  SessionSettings -> m Bool
getAnnounceDoubleNat ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->announce_double_nat } |]

setAnnounceDoubleNat :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAnnounceDoubleNat ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->announce_double_nat = $(bool val')} |]

getTorrentConnectBoost :: MonadIO m =>  SessionSettings -> m CInt
getTorrentConnectBoost ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->torrent_connect_boost } |]

setTorrentConnectBoost :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTorrentConnectBoost ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->torrent_connect_boost = $(int val)} |]

getSeedingOutgoingConnections :: MonadIO m =>  SessionSettings -> m Bool
getSeedingOutgoingConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->seeding_outgoing_connections } |]

setSeedingOutgoingConnections :: MonadIO m =>  SessionSettings -> Bool -> m ()
setSeedingOutgoingConnections ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->seeding_outgoing_connections = $(bool val')} |]

getNoConnectPrivilegedPorts :: MonadIO m =>  SessionSettings -> m Bool
getNoConnectPrivilegedPorts ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->no_connect_privileged_ports } |]

setNoConnectPrivilegedPorts :: MonadIO m =>  SessionSettings -> Bool -> m ()
setNoConnectPrivilegedPorts ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->no_connect_privileged_ports = $(bool val')} |]

getAlertQueueSize :: MonadIO m =>  SessionSettings -> m CInt
getAlertQueueSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->alert_queue_size } |]

setAlertQueueSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setAlertQueueSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->alert_queue_size = $(int val)} |]

getMaxMetadataSize :: MonadIO m =>  SessionSettings -> m CInt
getMaxMetadataSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_metadata_size } |]

setMaxMetadataSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxMetadataSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_metadata_size = $(int val)} |]

getSmoothConnects :: MonadIO m =>  SessionSettings -> m Bool
getSmoothConnects ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->smooth_connects } |]

setSmoothConnects :: MonadIO m =>  SessionSettings -> Bool -> m ()
setSmoothConnects ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->smooth_connects = $(bool val')} |]

getAlwaysSendUserAgent :: MonadIO m =>  SessionSettings -> m Bool
getAlwaysSendUserAgent ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->always_send_user_agent } |]

setAlwaysSendUserAgent :: MonadIO m =>  SessionSettings -> Bool -> m ()
setAlwaysSendUserAgent ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->always_send_user_agent = $(bool val')} |]

getApplyIpFilterToTrackers :: MonadIO m =>  SessionSettings -> m Bool
getApplyIpFilterToTrackers ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->apply_ip_filter_to_trackers } |]

setApplyIpFilterToTrackers :: MonadIO m =>  SessionSettings -> Bool -> m ()
setApplyIpFilterToTrackers ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->apply_ip_filter_to_trackers = $(bool val')} |]

getReadJobEvery :: MonadIO m =>  SessionSettings -> m CInt
getReadJobEvery ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->read_job_every } |]

setReadJobEvery :: MonadIO m =>  SessionSettings -> CInt -> m ()
setReadJobEvery ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->read_job_every = $(int val)} |]

getUseDiskReadAhead :: MonadIO m =>  SessionSettings -> m Bool
getUseDiskReadAhead ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_disk_read_ahead } |]

setUseDiskReadAhead :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUseDiskReadAhead ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_disk_read_ahead = $(bool val')} |]

getLockFiles :: MonadIO m =>  SessionSettings -> m Bool
getLockFiles ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->lock_files } |]

setLockFiles :: MonadIO m =>  SessionSettings -> Bool -> m ()
setLockFiles ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->lock_files = $(bool val')} |]

getSslListen :: MonadIO m =>  SessionSettings -> m CInt
getSslListen ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->ssl_listen } |]

setSslListen :: MonadIO m =>  SessionSettings -> CInt -> m ()
setSslListen ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->ssl_listen = $(int val)} |]

getTrackerBackoff :: MonadIO m =>  SessionSettings -> m CInt
getTrackerBackoff ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->tracker_backoff } |]

setTrackerBackoff :: MonadIO m =>  SessionSettings -> CInt -> m ()
setTrackerBackoff ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->tracker_backoff = $(int val)} |]

getBanWebSeeds :: MonadIO m =>  SessionSettings -> m Bool
getBanWebSeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->ban_web_seeds } |]

setBanWebSeeds :: MonadIO m =>  SessionSettings -> Bool -> m ()
setBanWebSeeds ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->ban_web_seeds = $(bool val')} |]

getMaxHttpRecvBufferSize :: MonadIO m =>  SessionSettings -> m CInt
getMaxHttpRecvBufferSize ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->max_http_recv_buffer_size } |]

setMaxHttpRecvBufferSize :: MonadIO m =>  SessionSettings -> CInt -> m ()
setMaxHttpRecvBufferSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->max_http_recv_buffer_size = $(int val)} |]

getSupportShareMode :: MonadIO m =>  SessionSettings -> m Bool
getSupportShareMode ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->support_share_mode } |]

setSupportShareMode :: MonadIO m =>  SessionSettings -> Bool -> m ()
setSupportShareMode ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->support_share_mode = $(bool val')} |]

getSupportMerkleTorrents :: MonadIO m =>  SessionSettings -> m Bool
getSupportMerkleTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->support_merkle_torrents } |]

setSupportMerkleTorrents :: MonadIO m =>  SessionSettings -> Bool -> m ()
setSupportMerkleTorrents ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->support_merkle_torrents = $(bool val')} |]

getReportRedundantBytes :: MonadIO m =>  SessionSettings -> m Bool
getReportRedundantBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->report_redundant_bytes } |]

setReportRedundantBytes :: MonadIO m =>  SessionSettings -> Bool -> m ()
setReportRedundantBytes ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->report_redundant_bytes = $(bool val')} |]

getHandshakeClientVersion :: MonadIO m =>  SessionSettings -> m Text
getHandshakeClientVersion ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(session_settings * hoPtr)->handshake_client_version) } |]
  stdStringToText res

setHandshakeClientVersion :: MonadIO m =>  SessionSettings -> Text -> m ()
setHandshakeClientVersion ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    liftIO . withPtr ho $ \hoPtr ->
      [CU.exp| void { $(session_settings * hoPtr)->handshake_client_version = std::string($(const char * cstr), $(size_t clen))} |]

getUseDiskCachePool :: MonadIO m =>  SessionSettings -> m Bool
getUseDiskCachePool ho =
  liftIO . withPtr ho $ \hoPtr ->
                 toBool <$> [CU.exp| bool { $(session_settings * hoPtr)->use_disk_cache_pool } |]

setUseDiskCachePool :: MonadIO m =>  SessionSettings -> Bool -> m ()
setUseDiskCachePool ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(session_settings * hoPtr)->use_disk_cache_pool = $(bool val')} |]

getInactiveDownRate :: MonadIO m =>  SessionSettings -> m CInt
getInactiveDownRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->inactive_down_rate } |]

setInactiveDownRate :: MonadIO m =>  SessionSettings -> CInt -> m ()
setInactiveDownRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->inactive_down_rate = $(int val)} |]

getInactiveUpRate :: MonadIO m =>  SessionSettings -> m CInt
getInactiveUpRate ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(session_settings * hoPtr)->inactive_up_rate } |]

setInactiveUpRate :: MonadIO m =>  SessionSettings -> CInt -> m ()
setInactiveUpRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(session_settings * hoPtr)->inactive_up_rate = $(int val)} |]

minMemoryUsage :: MonadIO m =>  m SessionSettings
minMemoryUsage =
  liftIO $ fromPtr [CU.exp| session_settings * { new session_settings(min_memory_usage()) } |]

highPerformanceSeed :: MonadIO m =>  m SessionSettings
highPerformanceSeed =
  liftIO $ fromPtr [CU.exp| session_settings * { new session_settings(high_performance_seed()) } |]
