{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Network.Libtorrent.H.SessionOpts (SessionOpts
                                        , sessionSettingsToOpts
                                        , sessionOptsToSettings
                                        , sesVersion
                                        , userAgent
                                        , trackerCompletionTimeout
                                        , trackerReceiveTimeout
                                        , stopTrackerTimeout
                                        , trackerMaximumResponseLength
                                        , pieceTimeout
                                        , requestTimeout
                                        , requestQueueTime
                                        , maxAllowedInRequestQueue
                                        , maxOutRequestQueue
                                        , wholePiecesThreshold
                                        , peerTimeout
                                        , urlseedTimeout
                                        , urlseedPipelineSize
                                        , urlseedWaitRetry
                                        , filePoolSize
                                        , allowMultipleConnectionsPerIp
                                        , maxFailcount
                                        , minReconnectTime
                                        , peerConnectTimeout
                                        , ignoreLimitsOnLocalNetwork
                                        , connectionSpeed
                                        , sendRedundantHave
                                        , lazyBitfields
                                        , inactivityTimeout
                                        , unchokeInterval
                                        , optimisticUnchokeInterval
                                        , announceIp
                                        , numWant
                                        , initialPickerThreshold
                                        , allowedFastSetSize
                                        , suggestMode
                                        , maxQueuedDiskBytes
                                        , maxQueuedDiskBytesLowWatermark
                                        , handshakeTimeout
                                        , useDhtAsFallback
                                        , freeTorrentHashes
                                        , upnpIgnoreNonrouters
                                        , sendBufferLowWatermark
                                        , sendBufferWatermark
                                        , sendBufferWatermarkFactor
                                        , chokingAlgorithm
                                        , seedChokingAlgorithm
                                        , useParoleMode
                                        , cacheSize
                                        , cacheBufferChunkSize
                                        , cacheExpiry
                                        , useReadCache
                                        , explicitReadCache
                                        , explicitCacheInterval
                                        , diskIoWriteMode
                                        , diskIoReadMode
                                        , coalesceReads
                                        , coalesceWrites
                                        , outgoingPorts
                                        , peerTos
                                        , activeDownloads
                                        , activeSeeds
                                        , activeDhtLimit
                                        , activeTrackerLimit
                                        , activeLsdLimit
                                        , activeLimit
                                        , autoManagePreferSeeds
                                        , dontCountSlowTorrents
                                        , autoManageInterval
                                        , shareRatioLimit
                                        , seedTimeRatioLimit
                                        , seedTimeLimit
                                        , peerTurnoverInterval
                                        , peerTurnover
                                        , peerTurnoverCutoff
                                        , closeRedundantConnections
                                        , autoScrapeInterval
                                        , autoScrapeMinInterval
                                        , maxPeerlistSize
                                        , maxPausedPeerlistSize
                                        , minAnnounceInterval
                                        , prioritizePartialPieces
                                        , autoManageStartup
                                        , rateLimitIpOverhead
                                        , announceToAllTrackers
                                        , announceToAllTiers
                                        , preferUdpTrackers
                                        , strictSuperSeeding
                                        , seedingPieceQuota
                                        , maxSparseRegions
                                        , lockDiskCache
                                        , maxRejects
                                        , recvSocketBufferSize
                                        , sendSocketBufferSize
                                        , optimizeHashingForSpeed
                                        , fileChecksDelayPerBlock
                                        , diskCacheAlgorithm
                                        , readCacheLineSize
                                        , writeCacheLineSize
                                        , optimisticDiskRetry
                                        , disableHashChecks
                                        , allowReorderedDiskOperations
                                        , allowI2pMixed
                                        , maxSuggestPieces
                                        , dropSkippedRequests
                                        , lowPrioDisk
                                        , localServiceAnnounceInterval
                                        , dhtAnnounceInterval
                                        , udpTrackerTokenExpiry
                                        , volatileReadCache
                                        , guidedReadCache
                                        , defaultCacheMinAge
                                        , numOptimisticUnchokeSlots
                                        , noAtimeStorage
                                        , defaultEstReciprocationRate
                                        , increaseEstReciprocationRate
                                        , decreaseEstReciprocationRate
                                        , incomingStartsQueuedTorrents
                                        , reportTrueDownloaded
                                        , strictEndGameMode
                                        , broadcastLsd
                                        , enableOutgoingUtp
                                        , enableIncomingUtp
                                        , enableOutgoingTcp
                                        , enableIncomingTcp
                                        , maxPexPeers
                                        , ignoreResumeTimestamps
                                        , noRecheckIncompleteResume
                                        , anonymousMode
                                        , forceProxy
                                        , tickInterval
                                        , reportWebSeedDownloads
                                        , shareModeTarget
                                        , uploadRateLimit
                                        , downloadRateLimit
                                        , localUploadRateLimit
                                        , localDownloadRateLimit
                                        , dhtUploadRateLimit
                                        , unchokeSlotsLimit
                                        , halfOpenLimit
                                        , sessionConnectionsLimit
                                        , connectionsSlack
                                        , utpTargetDelay
                                        , utpGainFactor
                                        , utpMinTimeout
                                        , utpSynResends
                                        , utpFinResends
                                        , utpNumResends
                                        , utpConnectTimeout
                                        , utpDynamicSockBuf
                                        , utpLossMultiplier
                                        , mixedModeAlgorithm
                                        , rateLimitUtp
                                        , listenQueueSize
                                        , announceDoubleNat
                                        , torrentConnectBoost
                                        , seedingOutgoingConnections
                                        , noConnectPrivilegedPorts
                                        , alertQueueSize
                                        , maxMetadataSize
                                        , smoothConnects
                                        , alwaysSendUserAgent
                                        , applyIpFilterToTrackers
                                        , readJobEvery
                                        , useDiskReadAhead
                                        , lockFiles
                                        , sslListen
                                        , trackerBackoff
                                        , banWebSeeds
                                        , maxHttpRecvBufferSize
                                        , supportShareMode
                                        , supportMerkleTorrents
                                        , reportRedundantBytes
                                        , handshakeClientVersion
                                        , useDiskCachePool
                                        , inactiveDownRate
                                        , inactiveUpRate
                                        ) where

import           Control.Applicative        ((<|>))
import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Default               as D
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

import           Network.Libtorrent
import           Network.Libtorrent.H.Types


instance D.Default SessionOpts where
  def = SessionOpts
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing

data SessionOpts = SessionOpts {
    _sesVersion                     :: !(Maybe Int)
  , _userAgent                      :: !(Maybe Text)
  , _trackerCompletionTimeout       :: !(Maybe Int)
  , _trackerReceiveTimeout          :: !(Maybe Int)
  , _stopTrackerTimeout             :: !(Maybe Int)
  , _trackerMaximumResponseLength   :: !(Maybe Int)
  , _pieceTimeout                   :: !(Maybe Int)
  , _requestTimeout                 :: !(Maybe Int)
  , _requestQueueTime               :: !(Maybe Int)
  , _maxAllowedInRequestQueue       :: !(Maybe Int)
  , _maxOutRequestQueue             :: !(Maybe Int)
  , _wholePiecesThreshold           :: !(Maybe Int)
  , _peerTimeout                    :: !(Maybe Int)
  , _urlseedTimeout                 :: !(Maybe Int)
  , _urlseedPipelineSize            :: !(Maybe Int)
  , _urlseedWaitRetry               :: !(Maybe Int)
  , _filePoolSize                   :: !(Maybe Int)
  , _allowMultipleConnectionsPerIp  :: !(Maybe Bool)
  , _maxFailcount                   :: !(Maybe Int)
  , _minReconnectTime               :: !(Maybe Int)
  , _peerConnectTimeout             :: !(Maybe Int)
  , _ignoreLimitsOnLocalNetwork     :: !(Maybe Bool)
  , _connectionSpeed                :: !(Maybe Int)
  , _sendRedundantHave              :: !(Maybe Bool)
  , _lazyBitfields                  :: !(Maybe Bool)
  , _inactivityTimeout              :: !(Maybe Int)
  , _unchokeInterval                :: !(Maybe Int)
  , _optimisticUnchokeInterval      :: !(Maybe Int)
  , _announceIp                     :: !(Maybe Text)
  , _numWant                        :: !(Maybe Int)
  , _initialPickerThreshold         :: !(Maybe Int)
  , _allowedFastSetSize             :: !(Maybe Int)
  , _suggestMode                    :: !(Maybe SuggestMode)
  , _maxQueuedDiskBytes             :: !(Maybe Int)
  , _maxQueuedDiskBytesLowWatermark :: !(Maybe Int)
  , _handshakeTimeout               :: !(Maybe Int)
  , _useDhtAsFallback               :: !(Maybe Bool)
  , _freeTorrentHashes              :: !(Maybe Bool)
  , _upnpIgnoreNonrouters           :: !(Maybe Bool)
  , _sendBufferLowWatermark         :: !(Maybe Int)
  , _sendBufferWatermark            :: !(Maybe Int)
  , _sendBufferWatermarkFactor      :: !(Maybe Int)
  , _chokingAlgorithm               :: !(Maybe ChokingAlgorithm)
  , _seedChokingAlgorithm           :: !(Maybe SeedChokingAlgorithm)
  , _useParoleMode                  :: !(Maybe Bool)
  , _cacheSize                      :: !(Maybe Int)
  , _cacheBufferChunkSize           :: !(Maybe Int)
  , _cacheExpiry                    :: !(Maybe Int)
  , _useReadCache                   :: !(Maybe Bool)
  , _explicitReadCache              :: !(Maybe Bool)
  , _explicitCacheInterval          :: !(Maybe Int)
  , _diskIoWriteMode                :: !(Maybe IoBufferMode)
  , _diskIoReadMode                 :: !(Maybe IoBufferMode)
  , _coalesceReads                  :: !(Maybe Bool)
  , _coalesceWrites                 :: !(Maybe Bool)
  , _outgoingPorts                  :: !(Maybe (Int, Int))
  , _peerTos                        :: !(Maybe Char)
  , _activeDownloads                :: !(Maybe Int)
  , _activeSeeds                    :: !(Maybe Int)
  , _activeDhtLimit                 :: !(Maybe Int)
  , _activeTrackerLimit             :: !(Maybe Int)
  , _activeLsdLimit                 :: !(Maybe Int)
  , _activeLimit                    :: !(Maybe Int)
  , _autoManagePreferSeeds          :: !(Maybe Bool)
  , _dontCountSlowTorrents          :: !(Maybe Bool)
  , _autoManageInterval             :: !(Maybe Int)
  , _shareRatioLimit                :: !(Maybe Float)
  , _seedTimeRatioLimit             :: !(Maybe Float)
  , _seedTimeLimit                  :: !(Maybe Int)
  , _peerTurnoverInterval           :: !(Maybe Int)
  , _peerTurnover                   :: !(Maybe Float)
  , _peerTurnoverCutoff             :: !(Maybe Float)
  , _closeRedundantConnections      :: !(Maybe Bool)
  , _autoScrapeInterval             :: !(Maybe Int)
  , _autoScrapeMinInterval          :: !(Maybe Int)
  , _maxPeerlistSize                :: !(Maybe Int)
  , _maxPausedPeerlistSize          :: !(Maybe Int)
  , _minAnnounceInterval            :: !(Maybe Int)
  , _prioritizePartialPieces        :: !(Maybe Bool)
  , _autoManageStartup              :: !(Maybe Int)
  , _rateLimitIpOverhead            :: !(Maybe Bool)
  , _announceToAllTrackers          :: !(Maybe Bool)
  , _announceToAllTiers             :: !(Maybe Bool)
  , _preferUdpTrackers              :: !(Maybe Bool)
  , _strictSuperSeeding             :: !(Maybe Bool)
  , _seedingPieceQuota              :: !(Maybe Int)
  , _maxSparseRegions               :: !(Maybe Int)
  , _lockDiskCache                  :: !(Maybe Bool)
  , _maxRejects                     :: !(Maybe Int)
  , _recvSocketBufferSize           :: !(Maybe Int)
  , _sendSocketBufferSize           :: !(Maybe Int)
  , _optimizeHashingForSpeed        :: !(Maybe Bool)
  , _fileChecksDelayPerBlock        :: !(Maybe Int)
  , _diskCacheAlgorithm             :: !(Maybe DiskCacheAlgo)
  , _readCacheLineSize              :: !(Maybe Int)
  , _writeCacheLineSize             :: !(Maybe Int)
  , _optimisticDiskRetry            :: !(Maybe Int)
  , _disableHashChecks              :: !(Maybe Bool)
  , _allowReorderedDiskOperations   :: !(Maybe Bool)
  , _allowI2pMixed                  :: !(Maybe Bool)
  , _maxSuggestPieces               :: !(Maybe Int)
  , _dropSkippedRequests            :: !(Maybe Bool)
  , _lowPrioDisk                    :: !(Maybe Bool)
  , _localServiceAnnounceInterval   :: !(Maybe Int)
  , _dhtAnnounceInterval            :: !(Maybe Int)
  , _udpTrackerTokenExpiry          :: !(Maybe Int)
  , _volatileReadCache              :: !(Maybe Bool)
  , _guidedReadCache                :: !(Maybe Bool)
  , _defaultCacheMinAge             :: !(Maybe Int)
  , _numOptimisticUnchokeSlots      :: !(Maybe Int)
  , _noAtimeStorage                 :: !(Maybe Bool)
  , _defaultEstReciprocationRate    :: !(Maybe Int)
  , _increaseEstReciprocationRate   :: !(Maybe Int)
  , _decreaseEstReciprocationRate   :: !(Maybe Int)
  , _incomingStartsQueuedTorrents   :: !(Maybe Bool)
  , _reportTrueDownloaded           :: !(Maybe Bool)
  , _strictEndGameMode              :: !(Maybe Bool)
  , _broadcastLsd                   :: !(Maybe Bool)
  , _enableOutgoingUtp              :: !(Maybe Bool)
  , _enableIncomingUtp              :: !(Maybe Bool)
  , _enableOutgoingTcp              :: !(Maybe Bool)
  , _enableIncomingTcp              :: !(Maybe Bool)
  , _maxPexPeers                    :: !(Maybe Int)
  , _ignoreResumeTimestamps         :: !(Maybe Bool)
  , _noRecheckIncompleteResume      :: !(Maybe Bool)
  , _anonymousMode                  :: !(Maybe Bool)
  , _forceProxy                     :: !(Maybe Bool)
  , _tickInterval                   :: !(Maybe Int)
  , _reportWebSeedDownloads         :: !(Maybe Bool)
  , _shareModeTarget                :: !(Maybe Int)
  , _uploadRateLimit                :: !(Maybe Int)
  , _downloadRateLimit              :: !(Maybe Int)
  , _localUploadRateLimit           :: !(Maybe Int)
  , _localDownloadRateLimit         :: !(Maybe Int)
  , _dhtUploadRateLimit             :: !(Maybe Int)
  , _unchokeSlotsLimit              :: !(Maybe Int)
  , _halfOpenLimit                  :: !(Maybe Int)
  , _sessionConnectionsLimit        :: !(Maybe Int)
  , _connectionsSlack               :: !(Maybe Int)
  , _utpTargetDelay                 :: !(Maybe Int)
  , _utpGainFactor                  :: !(Maybe Int)
  , _utpMinTimeout                  :: !(Maybe Int)
  , _utpSynResends                  :: !(Maybe Int)
  , _utpFinResends                  :: !(Maybe Int)
  , _utpNumResends                  :: !(Maybe Int)
  , _utpConnectTimeout              :: !(Maybe Int)
  , _utpDynamicSockBuf              :: !(Maybe Bool)
  , _utpLossMultiplier              :: !(Maybe Int)
  , _mixedModeAlgorithm             :: !(Maybe BandwidthMixedAlgo)
  , _rateLimitUtp                   :: !(Maybe Bool)
  , _listenQueueSize                :: !(Maybe Int)
  , _announceDoubleNat              :: !(Maybe Bool)
  , _torrentConnectBoost            :: !(Maybe Int)
  , _seedingOutgoingConnections     :: !(Maybe Bool)
  , _noConnectPrivilegedPorts       :: !(Maybe Bool)
  , _alertQueueSize                 :: !(Maybe Int)
  , _maxMetadataSize                :: !(Maybe Int)
  , _smoothConnects                 :: !(Maybe Bool)
  , _alwaysSendUserAgent            :: !(Maybe Bool)
  , _applyIpFilterToTrackers        :: !(Maybe Bool)
  , _readJobEvery                   :: !(Maybe Int)
  , _useDiskReadAhead               :: !(Maybe Bool)
  , _lockFiles                      :: !(Maybe Bool)
  , _sslListen                      :: !(Maybe Int)
  , _trackerBackoff                 :: !(Maybe Int)
  , _banWebSeeds                    :: !(Maybe Bool)
  , _maxHttpRecvBufferSize          :: !(Maybe Int)
  , _supportShareMode               :: !(Maybe Bool)
  , _supportMerkleTorrents          :: !(Maybe Bool)
  , _reportRedundantBytes           :: !(Maybe Bool)
  , _handshakeClientVersion         :: !(Maybe Text)
  , _useDiskCachePool               :: !(Maybe Bool)
  , _inactiveDownRate               :: !(Maybe Int)
  , _inactiveUpRate                 :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance MergeableOpts SessionOpts where
  mergeOpts o1 o2 =
    SessionOpts
    (_sesVersion                     o1 <|> _sesVersion                     o2  )
    (_userAgent                      o1 <|> _userAgent                      o2  )
    (_trackerCompletionTimeout       o1 <|> _trackerCompletionTimeout       o2  )
    (_trackerReceiveTimeout          o1 <|> _trackerReceiveTimeout          o2  )
    (_stopTrackerTimeout             o1 <|> _stopTrackerTimeout             o2  )
    (_trackerMaximumResponseLength   o1 <|> _trackerMaximumResponseLength   o2  )
    (_pieceTimeout                   o1 <|> _pieceTimeout                   o2  )
    (_requestTimeout                 o1 <|> _requestTimeout                 o2  )
    (_requestQueueTime               o1 <|> _requestQueueTime               o2  )
    (_maxAllowedInRequestQueue       o1 <|> _maxAllowedInRequestQueue       o2  )
    (_maxOutRequestQueue             o1 <|> _maxOutRequestQueue             o2  )
    (_wholePiecesThreshold           o1 <|> _wholePiecesThreshold           o2  )
    (_peerTimeout                    o1 <|> _peerTimeout                    o2  )
    (_urlseedTimeout                 o1 <|> _urlseedTimeout                 o2  )
    (_urlseedPipelineSize            o1 <|> _urlseedPipelineSize            o2  )
    (_urlseedWaitRetry               o1 <|> _urlseedWaitRetry               o2  )
    (_filePoolSize                   o1 <|> _filePoolSize                   o2  )
    (_allowMultipleConnectionsPerIp  o1 <|> _allowMultipleConnectionsPerIp  o2  )
    (_maxFailcount                   o1 <|> _maxFailcount                   o2  )
    (_minReconnectTime               o1 <|> _minReconnectTime               o2  )
    (_peerConnectTimeout             o1 <|> _peerConnectTimeout             o2  )
    (_ignoreLimitsOnLocalNetwork     o1 <|> _ignoreLimitsOnLocalNetwork     o2  )
    (_connectionSpeed                o1 <|> _connectionSpeed                o2  )
    (_sendRedundantHave              o1 <|> _sendRedundantHave              o2  )
    (_lazyBitfields                  o1 <|> _lazyBitfields                  o2  )
    (_inactivityTimeout              o1 <|> _inactivityTimeout              o2  )
    (_unchokeInterval                o1 <|> _unchokeInterval                o2  )
    (_optimisticUnchokeInterval      o1 <|> _optimisticUnchokeInterval      o2  )
    (_announceIp                     o1 <|> _announceIp                     o2  )
    (_numWant                        o1 <|> _numWant                        o2  )
    (_initialPickerThreshold         o1 <|> _initialPickerThreshold         o2  )
    (_allowedFastSetSize             o1 <|> _allowedFastSetSize             o2  )
    (_suggestMode                    o1 <|> _suggestMode                    o2  )
    (_maxQueuedDiskBytes             o1 <|> _maxQueuedDiskBytes             o2  )
    (_maxQueuedDiskBytesLowWatermark o1 <|> _maxQueuedDiskBytesLowWatermark o2  )
    (_handshakeTimeout               o1 <|> _handshakeTimeout               o2  )
    (_useDhtAsFallback               o1 <|> _useDhtAsFallback               o2  )
    (_freeTorrentHashes              o1 <|> _freeTorrentHashes              o2  )
    (_upnpIgnoreNonrouters           o1 <|> _upnpIgnoreNonrouters           o2  )
    (_sendBufferLowWatermark         o1 <|> _sendBufferLowWatermark         o2  )
    (_sendBufferWatermark            o1 <|> _sendBufferWatermark            o2  )
    (_sendBufferWatermarkFactor      o1 <|> _sendBufferWatermarkFactor      o2  )
    (_chokingAlgorithm               o1 <|> _chokingAlgorithm               o2  )
    (_seedChokingAlgorithm           o1 <|> _seedChokingAlgorithm           o2  )
    (_useParoleMode                  o1 <|> _useParoleMode                  o2  )
    (_cacheSize                      o1 <|> _cacheSize                      o2  )
    (_cacheBufferChunkSize           o1 <|> _cacheBufferChunkSize           o2  )
    (_cacheExpiry                    o1 <|> _cacheExpiry                    o2  )
    (_useReadCache                   o1 <|> _useReadCache                   o2  )
    (_explicitReadCache              o1 <|> _explicitReadCache              o2  )
    (_explicitCacheInterval          o1 <|> _explicitCacheInterval          o2  )
    (_diskIoWriteMode                o1 <|> _diskIoWriteMode                o2  )
    (_diskIoReadMode                 o1 <|> _diskIoReadMode                 o2  )
    (_coalesceReads                  o1 <|> _coalesceReads                  o2  )
    (_coalesceWrites                 o1 <|> _coalesceWrites                 o2  )
    (_outgoingPorts                  o1 <|> _outgoingPorts                  o2  )
    (_peerTos                        o1 <|> _peerTos                        o2  )
    (_activeDownloads                o1 <|> _activeDownloads                o2  )
    (_activeSeeds                    o1 <|> _activeSeeds                    o2  )
    (_activeDhtLimit                 o1 <|> _activeDhtLimit                 o2  )
    (_activeTrackerLimit             o1 <|> _activeTrackerLimit             o2  )
    (_activeLsdLimit                 o1 <|> _activeLsdLimit                 o2  )
    (_activeLimit                    o1 <|> _activeLimit                    o2  )
    (_autoManagePreferSeeds          o1 <|> _autoManagePreferSeeds          o2  )
    (_dontCountSlowTorrents          o1 <|> _dontCountSlowTorrents          o2  )
    (_autoManageInterval             o1 <|> _autoManageInterval             o2  )
    (_shareRatioLimit                o1 <|> _shareRatioLimit                o2  )
    (_seedTimeRatioLimit             o1 <|> _seedTimeRatioLimit             o2  )
    (_seedTimeLimit                  o1 <|> _seedTimeLimit                  o2  )
    (_peerTurnoverInterval           o1 <|> _peerTurnoverInterval           o2  )
    (_peerTurnover                   o1 <|> _peerTurnover                   o2  )
    (_peerTurnoverCutoff             o1 <|> _peerTurnoverCutoff             o2  )
    (_closeRedundantConnections      o1 <|> _closeRedundantConnections      o2  )
    (_autoScrapeInterval             o1 <|> _autoScrapeInterval             o2  )
    (_autoScrapeMinInterval          o1 <|> _autoScrapeMinInterval          o2  )
    (_maxPeerlistSize                o1 <|> _maxPeerlistSize                o2  )
    (_maxPausedPeerlistSize          o1 <|> _maxPausedPeerlistSize          o2  )
    (_minAnnounceInterval            o1 <|> _minAnnounceInterval            o2  )
    (_prioritizePartialPieces        o1 <|> _prioritizePartialPieces        o2  )
    (_autoManageStartup              o1 <|> _autoManageStartup              o2  )
    (_rateLimitIpOverhead            o1 <|> _rateLimitIpOverhead            o2  )
    (_announceToAllTrackers          o1 <|> _announceToAllTrackers          o2  )
    (_announceToAllTiers             o1 <|> _announceToAllTiers             o2  )
    (_preferUdpTrackers              o1 <|> _preferUdpTrackers              o2  )
    (_strictSuperSeeding             o1 <|> _strictSuperSeeding             o2  )
    (_seedingPieceQuota              o1 <|> _seedingPieceQuota              o2  )
    (_maxSparseRegions               o1 <|> _maxSparseRegions               o2  )
    (_lockDiskCache                  o1 <|> _lockDiskCache                  o2  )
    (_maxRejects                     o1 <|> _maxRejects                     o2  )
    (_recvSocketBufferSize           o1 <|> _recvSocketBufferSize           o2  )
    (_sendSocketBufferSize           o1 <|> _sendSocketBufferSize           o2  )
    (_optimizeHashingForSpeed        o1 <|> _optimizeHashingForSpeed        o2  )
    (_fileChecksDelayPerBlock        o1 <|> _fileChecksDelayPerBlock        o2  )
    (_diskCacheAlgorithm             o1 <|> _diskCacheAlgorithm             o2  )
    (_readCacheLineSize              o1 <|> _readCacheLineSize              o2  )
    (_writeCacheLineSize             o1 <|> _writeCacheLineSize             o2  )
    (_optimisticDiskRetry            o1 <|> _optimisticDiskRetry            o2  )
    (_disableHashChecks              o1 <|> _disableHashChecks              o2  )
    (_allowReorderedDiskOperations   o1 <|> _allowReorderedDiskOperations   o2  )
    (_allowI2pMixed                  o1 <|> _allowI2pMixed                  o2  )
    (_maxSuggestPieces               o1 <|> _maxSuggestPieces               o2  )
    (_dropSkippedRequests            o1 <|> _dropSkippedRequests            o2  )
    (_lowPrioDisk                    o1 <|> _lowPrioDisk                    o2  )
    (_localServiceAnnounceInterval   o1 <|> _localServiceAnnounceInterval   o2  )
    (_dhtAnnounceInterval            o1 <|> _dhtAnnounceInterval            o2  )
    (_udpTrackerTokenExpiry          o1 <|> _udpTrackerTokenExpiry          o2  )
    (_volatileReadCache              o1 <|> _volatileReadCache              o2  )
    (_guidedReadCache                o1 <|> _guidedReadCache                o2  )
    (_defaultCacheMinAge             o1 <|> _defaultCacheMinAge             o2  )
    (_numOptimisticUnchokeSlots      o1 <|> _numOptimisticUnchokeSlots      o2  )
    (_noAtimeStorage                 o1 <|> _noAtimeStorage                 o2  )
    (_defaultEstReciprocationRate    o1 <|> _defaultEstReciprocationRate    o2  )
    (_increaseEstReciprocationRate   o1 <|> _increaseEstReciprocationRate   o2  )
    (_decreaseEstReciprocationRate   o1 <|> _decreaseEstReciprocationRate   o2  )
    (_incomingStartsQueuedTorrents   o1 <|> _incomingStartsQueuedTorrents   o2  )
    (_reportTrueDownloaded           o1 <|> _reportTrueDownloaded           o2  )
    (_strictEndGameMode              o1 <|> _strictEndGameMode              o2  )
    (_broadcastLsd                   o1 <|> _broadcastLsd                   o2  )
    (_enableOutgoingUtp              o1 <|> _enableOutgoingUtp              o2  )
    (_enableIncomingUtp              o1 <|> _enableIncomingUtp              o2  )
    (_enableOutgoingTcp              o1 <|> _enableOutgoingTcp              o2  )
    (_enableIncomingTcp              o1 <|> _enableIncomingTcp              o2  )
    (_maxPexPeers                    o1 <|> _maxPexPeers                    o2  )
    (_ignoreResumeTimestamps         o1 <|> _ignoreResumeTimestamps         o2  )
    (_noRecheckIncompleteResume      o1 <|> _noRecheckIncompleteResume      o2  )
    (_anonymousMode                  o1 <|> _anonymousMode                  o2  )
    (_forceProxy                     o1 <|> _forceProxy                     o2  )
    (_tickInterval                   o1 <|> _tickInterval                   o2  )
    (_reportWebSeedDownloads         o1 <|> _reportWebSeedDownloads         o2  )
    (_shareModeTarget                o1 <|> _shareModeTarget                o2  )
    (_uploadRateLimit                o1 <|> _uploadRateLimit                o2  )
    (_downloadRateLimit              o1 <|> _downloadRateLimit              o2  )
    (_localUploadRateLimit           o1 <|> _localUploadRateLimit           o2  )
    (_localDownloadRateLimit         o1 <|> _localDownloadRateLimit         o2  )
    (_dhtUploadRateLimit             o1 <|> _dhtUploadRateLimit             o2  )
    (_unchokeSlotsLimit              o1 <|> _unchokeSlotsLimit              o2  )
    (_halfOpenLimit                  o1 <|> _halfOpenLimit                  o2  )
    (_sessionConnectionsLimit        o1 <|> _sessionConnectionsLimit        o2  )
    (_connectionsSlack               o1 <|> _connectionsSlack               o2  )
    (_utpTargetDelay                 o1 <|> _utpTargetDelay                 o2  )
    (_utpGainFactor                  o1 <|> _utpGainFactor                  o2  )
    (_utpMinTimeout                  o1 <|> _utpMinTimeout                  o2  )
    (_utpSynResends                  o1 <|> _utpSynResends                  o2  )
    (_utpFinResends                  o1 <|> _utpFinResends                  o2  )
    (_utpNumResends                  o1 <|> _utpNumResends                  o2  )
    (_utpConnectTimeout              o1 <|> _utpConnectTimeout              o2  )
    (_utpDynamicSockBuf              o1 <|> _utpDynamicSockBuf              o2  )
    (_utpLossMultiplier              o1 <|> _utpLossMultiplier              o2  )
    (_mixedModeAlgorithm             o1 <|> _mixedModeAlgorithm             o2  )
    (_rateLimitUtp                   o1 <|> _rateLimitUtp                   o2  )
    (_listenQueueSize                o1 <|> _listenQueueSize                o2  )
    (_announceDoubleNat              o1 <|> _announceDoubleNat              o2  )
    (_torrentConnectBoost            o1 <|> _torrentConnectBoost            o2  )
    (_seedingOutgoingConnections     o1 <|> _seedingOutgoingConnections     o2  )
    (_noConnectPrivilegedPorts       o1 <|> _noConnectPrivilegedPorts       o2  )
    (_alertQueueSize                 o1 <|> _alertQueueSize                 o2  )
    (_maxMetadataSize                o1 <|> _maxMetadataSize                o2  )
    (_smoothConnects                 o1 <|> _smoothConnects                 o2  )
    (_alwaysSendUserAgent            o1 <|> _alwaysSendUserAgent            o2  )
    (_applyIpFilterToTrackers        o1 <|> _applyIpFilterToTrackers        o2  )
    (_readJobEvery                   o1 <|> _readJobEvery                   o2  )
    (_useDiskReadAhead               o1 <|> _useDiskReadAhead               o2  )
    (_lockFiles                      o1 <|> _lockFiles                      o2  )
    (_sslListen                      o1 <|> _sslListen                      o2  )
    (_trackerBackoff                 o1 <|> _trackerBackoff                 o2  )
    (_banWebSeeds                    o1 <|> _banWebSeeds                    o2  )
    (_maxHttpRecvBufferSize          o1 <|> _maxHttpRecvBufferSize          o2  )
    (_supportShareMode               o1 <|> _supportShareMode               o2  )
    (_supportMerkleTorrents          o1 <|> _supportMerkleTorrents          o2  )
    (_reportRedundantBytes           o1 <|> _reportRedundantBytes           o2  )
    (_handshakeClientVersion         o1 <|> _handshakeClientVersion         o2  )
    (_useDiskCachePool               o1 <|> _useDiskCachePool               o2  )
    (_inactiveDownRate               o1 <|> _inactiveDownRate               o2  )
    (_inactiveUpRate                 o1 <|> _inactiveUpRate                 o2  )


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

instance ToJSON SessionOpts  where
   toJSON = genericToJSON $ aesonDrop 1 snakeCase
instance FromJSON SessionOpts where
   parseJSON = genericParseJSON $ aesonDrop 1 snakeCase

sessionSettingsToOpts :: MonadIO m => SessionSettings -> m SessionOpts
sessionSettingsToOpts ss =
  SessionOpts
    <$> (Just . fromIntegral <$> getVersion ss)
    <*> (Just <$> getUserAgent ss)
    <*> (Just . fromIntegral <$> getTrackerCompletionTimeout ss)
    <*> (Just . fromIntegral <$> getTrackerReceiveTimeout ss)
    <*> (Just . fromIntegral <$> getStopTrackerTimeout ss)
    <*> (Just . fromIntegral <$> getTrackerMaximumResponseLength ss)
    <*> (Just . fromIntegral <$> getPieceTimeout ss)
    <*> (Just . fromIntegral <$> getRequestTimeout ss)
    <*> (Just . fromIntegral <$> getRequestQueueTime ss)
    <*> (Just . fromIntegral <$> getMaxAllowedInRequestQueue ss)
    <*> (Just . fromIntegral <$> getMaxOutRequestQueue ss)
    <*> (Just . fromIntegral <$> getWholePiecesThreshold ss)
    <*> (Just . fromIntegral <$> getPeerTimeout ss)
    <*> (Just . fromIntegral <$> getUrlseedTimeout ss)
    <*> (Just . fromIntegral <$> getUrlseedPipelineSize ss)
    <*> (Just . fromIntegral <$> getUrlseedWaitRetry ss)
    <*> (Just . fromIntegral <$> getFilePoolSize ss)
    <*> (Just <$> getAllowMultipleConnectionsPerIp ss)
    <*> (Just . fromIntegral <$> getMaxFailcount ss)
    <*> (Just . fromIntegral <$> getMinReconnectTime ss)
    <*> (Just . fromIntegral <$> getPeerConnectTimeout ss)
    <*> (Just <$> getIgnoreLimitsOnLocalNetwork ss)
    <*> (Just . fromIntegral <$> getConnectionSpeed ss)
    <*> (Just <$> getSendRedundantHave ss)
    <*> (Just <$> getLazyBitfields ss)
    <*> (Just . fromIntegral <$> getInactivityTimeout ss)
    <*> (Just . fromIntegral <$> getUnchokeInterval ss)
    <*> (Just . fromIntegral <$> getOptimisticUnchokeInterval ss)
    <*> (Just <$> getAnnounceIp ss)
    <*> (Just . fromIntegral <$> getNumWant ss)
    <*> (Just . fromIntegral <$> getInitialPickerThreshold ss)
    <*> (Just . fromIntegral <$> getAllowedFastSetSize ss)
    <*> (Just <$> getSuggestMode ss)
    <*> (Just . fromIntegral <$> getMaxQueuedDiskBytes ss)
    <*> (Just . fromIntegral <$> getMaxQueuedDiskBytesLowWatermark ss)
    <*> (Just . fromIntegral <$> getHandshakeTimeout ss)
    <*> (Just <$> getUseDhtAsFallback ss)
    <*> (Just <$> getFreeTorrentHashes ss)
    <*> (Just <$> getUpnpIgnoreNonrouters ss)
    <*> (Just . fromIntegral <$> getSendBufferLowWatermark ss)
    <*> (Just . fromIntegral <$> getSendBufferWatermark ss)
    <*> (Just . fromIntegral <$> getSendBufferWatermarkFactor ss)
    <*> (Just <$> getChokingAlgorithm ss)
    <*> (Just <$> getSeedChokingAlgorithm ss)
    <*> (Just <$> getUseParoleMode ss)
    <*> (Just . fromIntegral <$> getCacheSize ss)
    <*> (Just . fromIntegral <$> getCacheBufferChunkSize ss)
    <*> (Just . fromIntegral <$> getCacheExpiry ss)
    <*> (Just <$> getUseReadCache ss)
    <*> (Just <$> getExplicitReadCache ss)
    <*> (Just . fromIntegral <$> getExplicitCacheInterval ss)
    <*> (Just <$> getDiskIoWriteMode ss)
    <*> (Just <$> getDiskIoReadMode ss)
    <*> (Just <$> getCoalesceReads ss)
    <*> (Just <$> getCoalesceWrites ss)
    <*> (Just . bimap fromIntegral fromIntegral <$> getOutgoingPorts ss)
    <*> (Just . toEnum . fromEnum <$> getPeerTos ss)
    <*> (Just . fromIntegral <$> getActiveDownloads ss)
    <*> (Just . fromIntegral <$> getActiveSeeds ss)
    <*> (Just . fromIntegral <$> getActiveDhtLimit ss)
    <*> (Just . fromIntegral <$> getActiveTrackerLimit ss)
    <*> (Just . fromIntegral <$> getActiveLsdLimit ss)
    <*> (Just . fromIntegral <$> getActiveLimit ss)
    <*> (Just <$> getAutoManagePreferSeeds ss)
    <*> (Just <$> getDontCountSlowTorrents ss)
    <*> (Just . fromIntegral <$> getAutoManageInterval ss)
    <*> (Just . fromRational . toRational <$> getShareRatioLimit ss)
    <*> (Just . fromRational . toRational <$> getSeedTimeRatioLimit ss)
    <*> (Just . fromIntegral <$> getSeedTimeLimit ss)
    <*> (Just . fromIntegral <$> getPeerTurnoverInterval ss)
    <*> (Just . fromRational . toRational <$> getPeerTurnover ss)
    <*> (Just . fromRational . toRational <$> getPeerTurnoverCutoff ss)
    <*> (Just <$> getCloseRedundantConnections ss)
    <*> (Just . fromIntegral <$> getAutoScrapeInterval ss)
    <*> (Just . fromIntegral <$> getAutoScrapeMinInterval ss)
    <*> (Just . fromIntegral <$> getMaxPeerlistSize ss)
    <*> (Just . fromIntegral <$> getMaxPausedPeerlistSize ss)
    <*> (Just . fromIntegral <$> getMinAnnounceInterval ss)
    <*> (Just <$> getPrioritizePartialPieces ss)
    <*> (Just . fromIntegral <$> getAutoManageStartup ss)
    <*> (Just <$> getRateLimitIpOverhead ss)
    <*> (Just <$> getAnnounceToAllTrackers ss)
    <*> (Just <$> getAnnounceToAllTiers ss)
    <*> (Just <$> getPreferUdpTrackers ss)
    <*> (Just <$> getStrictSuperSeeding ss)
    <*> (Just . fromIntegral <$> getSeedingPieceQuota ss)
    <*> (Just . fromIntegral <$> getMaxSparseRegions ss)
    <*> (Just <$> getLockDiskCache ss)
    <*> (Just . fromIntegral <$> getMaxRejects ss)
    <*> (Just . fromIntegral <$> getRecvSocketBufferSize ss)
    <*> (Just . fromIntegral <$> getSendSocketBufferSize ss)
    <*> (Just <$> getOptimizeHashingForSpeed ss)
    <*> (Just . fromIntegral <$> getFileChecksDelayPerBlock ss)
    <*> (Just <$> getDiskCacheAlgorithm ss)
    <*> (Just . fromIntegral <$> getReadCacheLineSize ss)
    <*> (Just . fromIntegral <$> getWriteCacheLineSize ss)
    <*> (Just . fromIntegral <$> getOptimisticDiskRetry ss)
    <*> (Just <$> getDisableHashChecks ss)
    <*> (Just <$> getAllowReorderedDiskOperations ss)
    <*> (Just <$> getAllowI2pMixed ss)
    <*> (Just . fromIntegral <$> getMaxSuggestPieces ss)
    <*> (Just <$> getDropSkippedRequests ss)
    <*> (Just <$> getLowPrioDisk ss)
    <*> (Just . fromIntegral <$> getLocalServiceAnnounceInterval ss)
    <*> (Just . fromIntegral <$> getDhtAnnounceInterval ss)
    <*> (Just . fromIntegral <$> getUdpTrackerTokenExpiry ss)
    <*> (Just <$> getVolatileReadCache ss)
    <*> (Just <$> getGuidedReadCache ss)
    <*> (Just . fromIntegral <$> getDefaultCacheMinAge ss)
    <*> (Just . fromIntegral <$> getNumOptimisticUnchokeSlots ss)
    <*> (Just <$> getNoAtimeStorage ss)
    <*> (Just . fromIntegral <$> getDefaultEstReciprocationRate ss)
    <*> (Just . fromIntegral <$> getIncreaseEstReciprocationRate ss)
    <*> (Just . fromIntegral <$> getDecreaseEstReciprocationRate ss)
    <*> (Just <$> getIncomingStartsQueuedTorrents ss)
    <*> (Just <$> getReportTrueDownloaded ss)
    <*> (Just <$> getStrictEndGameMode ss)
    <*> (Just <$> getBroadcastLsd ss)
    <*> (Just <$> getEnableOutgoingUtp ss)
    <*> (Just <$> getEnableIncomingUtp ss)
    <*> (Just <$> getEnableOutgoingTcp ss)
    <*> (Just <$> getEnableIncomingTcp ss)
    <*> (Just . fromIntegral <$> getMaxPexPeers ss)
    <*> (Just <$> getIgnoreResumeTimestamps ss)
    <*> (Just <$> getNoRecheckIncompleteResume ss)
    <*> (Just <$> getAnonymousMode ss)
    <*> (Just <$> getForceProxy ss)
    <*> (Just . fromIntegral <$> getTickInterval ss)
    <*> (Just <$> getReportWebSeedDownloads ss)
    <*> (Just . fromIntegral <$> getShareModeTarget ss)
    <*> (Just . fromIntegral <$> getUploadRateLimit ss)
    <*> (Just . fromIntegral <$> getDownloadRateLimit ss)
    <*> (Just . fromIntegral <$> getLocalUploadRateLimit ss)
    <*> (Just . fromIntegral <$> getLocalDownloadRateLimit ss)
    <*> (Just . fromIntegral <$> getDhtUploadRateLimit ss)
    <*> (Just . fromIntegral <$> getUnchokeSlotsLimit ss)
    <*> (Just . fromIntegral <$> getHalfOpenLimit ss)
    <*> (Just . fromIntegral <$> getSessionConnectionsLimit ss)
    <*> (Just . fromIntegral <$> getConnectionsSlack ss)
    <*> (Just . fromIntegral <$> getUtpTargetDelay ss)
    <*> (Just . fromIntegral <$> getUtpGainFactor ss)
    <*> (Just . fromIntegral <$> getUtpMinTimeout ss)
    <*> (Just . fromIntegral <$> getUtpSynResends ss)
    <*> (Just . fromIntegral <$> getUtpFinResends ss)
    <*> (Just . fromIntegral <$> getUtpNumResends ss)
    <*> (Just . fromIntegral <$> getUtpConnectTimeout ss)
    <*> (Just <$> getUtpDynamicSockBuf ss)
    <*> (Just . fromIntegral <$> getUtpLossMultiplier ss)
    <*> (Just <$> getMixedModeAlgorithm ss)
    <*> (Just <$> getRateLimitUtp ss)
    <*> (Just . fromIntegral <$> getListenQueueSize ss)
    <*> (Just <$> getAnnounceDoubleNat ss)
    <*> (Just . fromIntegral <$> getTorrentConnectBoost ss)
    <*> (Just <$> getSeedingOutgoingConnections ss)
    <*> (Just <$> getNoConnectPrivilegedPorts ss)
    <*> (Just . fromIntegral <$> getAlertQueueSize ss)
    <*> (Just . fromIntegral <$> getMaxMetadataSize ss)
    <*> (Just <$> getSmoothConnects ss)
    <*> (Just <$> getAlwaysSendUserAgent ss)
    <*> (Just <$> getApplyIpFilterToTrackers ss)
    <*> (Just . fromIntegral <$> getReadJobEvery ss)
    <*> (Just <$> getUseDiskReadAhead ss)
    <*> (Just <$> getLockFiles ss)
    <*> (Just . fromIntegral <$> getSslListen ss)
    <*> (Just . fromIntegral <$> getTrackerBackoff ss)
    <*> (Just <$> getBanWebSeeds ss)
    <*> (Just . fromIntegral <$> getMaxHttpRecvBufferSize ss)
    <*> (Just <$> getSupportShareMode ss)
    <*> (Just <$> getSupportMerkleTorrents ss)
    <*> (Just <$> getReportRedundantBytes ss)
    <*> (Just <$> getHandshakeClientVersion ss)
    <*> (Just <$> getUseDiskCachePool ss)
    <*> (Just . fromIntegral <$> getInactiveDownRate ss)
    <*> (Just . fromIntegral <$> getInactiveUpRate ss)

sessionOptsToSettings :: MonadIO m => SessionOpts -> SessionSettings -> m ()
sessionOptsToSettings SessionOpts{..} ss = do
  when (isJust _sesVersion) $ setVersion ss (fromIntegral $ fromJust _sesVersion)
  when (isJust _userAgent) $ setUserAgent ss (fromJust _userAgent)
  when (isJust _trackerCompletionTimeout) $ setTrackerCompletionTimeout ss (fromIntegral $ fromJust _trackerCompletionTimeout)
  when (isJust _trackerReceiveTimeout) $ setTrackerReceiveTimeout ss (fromIntegral $ fromJust _trackerReceiveTimeout)
  when (isJust _stopTrackerTimeout) $ setStopTrackerTimeout ss (fromIntegral $ fromJust _stopTrackerTimeout)
  when (isJust _trackerMaximumResponseLength) $ setTrackerMaximumResponseLength ss (fromIntegral $ fromJust _trackerMaximumResponseLength)
  when (isJust _pieceTimeout) $ setPieceTimeout ss (fromIntegral $ fromJust _pieceTimeout)
  when (isJust _requestTimeout) $ setRequestTimeout ss (fromIntegral $ fromJust _requestTimeout)
  when (isJust _requestQueueTime) $ setRequestQueueTime ss (fromIntegral $ fromJust _requestQueueTime)
  when (isJust _maxAllowedInRequestQueue) $ setMaxAllowedInRequestQueue ss (fromIntegral $ fromJust _maxAllowedInRequestQueue)
  when (isJust _maxOutRequestQueue) $ setMaxOutRequestQueue ss (fromIntegral $ fromJust _maxOutRequestQueue)
  when (isJust _wholePiecesThreshold) $ setWholePiecesThreshold ss (fromIntegral $ fromJust _wholePiecesThreshold)
  when (isJust _peerTimeout) $ setPeerTimeout ss (fromIntegral $ fromJust _peerTimeout)
  when (isJust _urlseedTimeout) $ setUrlseedTimeout ss (fromIntegral $ fromJust _urlseedTimeout)
  when (isJust _urlseedPipelineSize) $ setUrlseedPipelineSize ss (fromIntegral $ fromJust _urlseedPipelineSize)
  when (isJust _urlseedWaitRetry) $ setUrlseedWaitRetry ss (fromIntegral $ fromJust _urlseedWaitRetry)
  when (isJust _filePoolSize) $ setFilePoolSize ss (fromIntegral $ fromJust _filePoolSize)
  when (isJust _allowMultipleConnectionsPerIp) $ setAllowMultipleConnectionsPerIp ss (fromJust _allowMultipleConnectionsPerIp)
  when (isJust _maxFailcount) $ setMaxFailcount ss (fromIntegral $ fromJust _maxFailcount)
  when (isJust _minReconnectTime) $ setMinReconnectTime ss (fromIntegral $ fromJust _minReconnectTime)
  when (isJust _peerConnectTimeout) $ setPeerConnectTimeout ss (fromIntegral $ fromJust _peerConnectTimeout)
  when (isJust _ignoreLimitsOnLocalNetwork) $ setIgnoreLimitsOnLocalNetwork ss (fromJust _ignoreLimitsOnLocalNetwork)
  when (isJust _connectionSpeed) $ setConnectionSpeed ss (fromIntegral $ fromJust _connectionSpeed)
  when (isJust _sendRedundantHave) $ setSendRedundantHave ss (fromJust _sendRedundantHave)
  when (isJust _lazyBitfields) $ setLazyBitfields ss (fromJust _lazyBitfields)
  when (isJust _inactivityTimeout) $ setInactivityTimeout ss (fromIntegral $ fromJust _inactivityTimeout)
  when (isJust _unchokeInterval) $ setUnchokeInterval ss (fromIntegral $ fromJust _unchokeInterval)
  when (isJust _optimisticUnchokeInterval) $ setOptimisticUnchokeInterval ss (fromIntegral $ fromJust _optimisticUnchokeInterval)
  when (isJust _announceIp) $ setAnnounceIp ss (fromJust _announceIp)
  when (isJust _numWant) $ setNumWant ss (fromIntegral $ fromJust _numWant)
  when (isJust _initialPickerThreshold) $ setInitialPickerThreshold ss (fromIntegral $ fromJust _initialPickerThreshold)
  when (isJust _allowedFastSetSize) $ setAllowedFastSetSize ss (fromIntegral $ fromJust _allowedFastSetSize)
  when (isJust _suggestMode) $ setSuggestMode ss (fromJust _suggestMode)
  when (isJust _maxQueuedDiskBytes) $ setMaxQueuedDiskBytes ss (fromIntegral $ fromJust _maxQueuedDiskBytes)
  when (isJust _maxQueuedDiskBytesLowWatermark) $ setMaxQueuedDiskBytesLowWatermark ss (fromIntegral $ fromJust _maxQueuedDiskBytesLowWatermark)
  when (isJust _handshakeTimeout) $ setHandshakeTimeout ss (fromIntegral $ fromJust _handshakeTimeout)
  when (isJust _useDhtAsFallback) $ setUseDhtAsFallback ss (fromJust _useDhtAsFallback)
  when (isJust _freeTorrentHashes) $ setFreeTorrentHashes ss (fromJust _freeTorrentHashes)
  when (isJust _upnpIgnoreNonrouters) $ setUpnpIgnoreNonrouters ss (fromJust _upnpIgnoreNonrouters)
  when (isJust _sendBufferLowWatermark) $ setSendBufferLowWatermark ss (fromIntegral $ fromJust _sendBufferLowWatermark)
  when (isJust _sendBufferWatermark) $ setSendBufferWatermark ss (fromIntegral $ fromJust _sendBufferWatermark)
  when (isJust _sendBufferWatermarkFactor) $ setSendBufferWatermarkFactor ss (fromIntegral $ fromJust _sendBufferWatermarkFactor)
  when (isJust _chokingAlgorithm) $ setChokingAlgorithm ss (fromJust _chokingAlgorithm)
  when (isJust _seedChokingAlgorithm) $ setSeedChokingAlgorithm ss (fromJust _seedChokingAlgorithm)
  when (isJust _useParoleMode) $ setUseParoleMode ss (fromJust _useParoleMode)
  when (isJust _cacheSize) $ setCacheSize ss (fromIntegral $ fromJust _cacheSize)
  when (isJust _cacheBufferChunkSize) $ setCacheBufferChunkSize ss (fromIntegral $ fromJust _cacheBufferChunkSize)
  when (isJust _cacheExpiry) $ setCacheExpiry ss (fromIntegral $ fromJust _cacheExpiry)
  when (isJust _useReadCache) $ setUseReadCache ss (fromJust _useReadCache)
  when (isJust _explicitReadCache) $ setExplicitReadCache ss (fromJust _explicitReadCache)
  when (isJust _explicitCacheInterval) $ setExplicitCacheInterval ss (fromIntegral $ fromJust _explicitCacheInterval)
  when (isJust _diskIoWriteMode) $ setDiskIoWriteMode ss (fromJust _diskIoWriteMode)
  when (isJust _diskIoReadMode) $ setDiskIoReadMode ss (fromJust _diskIoReadMode)
  when (isJust _coalesceReads) $ setCoalesceReads ss (fromJust _coalesceReads)
  when (isJust _coalesceWrites) $ setCoalesceWrites ss (fromJust _coalesceWrites)
  when (isJust _outgoingPorts) $ setOutgoingPorts ss (bimap fromIntegral fromIntegral $ fromJust _outgoingPorts)
  when (isJust _peerTos) $ setPeerTos ss (toEnum . fromEnum $ fromJust _peerTos)
  when (isJust _activeDownloads) $ setActiveDownloads ss (fromIntegral $ fromJust _activeDownloads)
  when (isJust _activeSeeds) $ setActiveSeeds ss (fromIntegral $ fromJust _activeSeeds)
  when (isJust _activeDhtLimit) $ setActiveDhtLimit ss (fromIntegral $ fromJust _activeDhtLimit)
  when (isJust _activeTrackerLimit) $ setActiveTrackerLimit ss (fromIntegral $ fromJust _activeTrackerLimit)
  when (isJust _activeLsdLimit) $ setActiveLsdLimit ss (fromIntegral $ fromJust _activeLsdLimit)
  when (isJust _activeLimit) $ setActiveLimit ss (fromIntegral $ fromJust _activeLimit)
  when (isJust _autoManagePreferSeeds) $ setAutoManagePreferSeeds ss (fromJust _autoManagePreferSeeds)
  when (isJust _dontCountSlowTorrents) $ setDontCountSlowTorrents ss (fromJust _dontCountSlowTorrents)
  when (isJust _autoManageInterval) $ setAutoManageInterval ss (fromIntegral $ fromJust _autoManageInterval)
  when (isJust _shareRatioLimit) $ setShareRatioLimit ss (fromRational . toRational $ fromJust _shareRatioLimit)
  when (isJust _seedTimeRatioLimit) $ setSeedTimeRatioLimit ss (fromRational . toRational $ fromJust _seedTimeRatioLimit)
  when (isJust _seedTimeLimit) $ setSeedTimeLimit ss (fromIntegral $ fromJust _seedTimeLimit)
  when (isJust _peerTurnoverInterval) $ setPeerTurnoverInterval ss (fromIntegral $ fromJust _peerTurnoverInterval)
  when (isJust _peerTurnover) $ setPeerTurnover ss (fromRational . toRational $ fromJust _peerTurnover)
  when (isJust _peerTurnoverCutoff) $ setPeerTurnoverCutoff ss (fromRational . toRational $ fromJust _peerTurnoverCutoff)
  when (isJust _closeRedundantConnections) $ setCloseRedundantConnections ss (fromJust _closeRedundantConnections)
  when (isJust _autoScrapeInterval) $ setAutoScrapeInterval ss (fromIntegral $ fromJust _autoScrapeInterval)
  when (isJust _autoScrapeMinInterval) $ setAutoScrapeMinInterval ss (fromIntegral $ fromJust _autoScrapeMinInterval)
  when (isJust _maxPeerlistSize) $ setMaxPeerlistSize ss (fromIntegral $ fromJust _maxPeerlistSize)
  when (isJust _maxPausedPeerlistSize) $ setMaxPausedPeerlistSize ss (fromIntegral $ fromJust _maxPausedPeerlistSize)
  when (isJust _minAnnounceInterval) $ setMinAnnounceInterval ss (fromIntegral $ fromJust _minAnnounceInterval)
  when (isJust _prioritizePartialPieces) $ setPrioritizePartialPieces ss (fromJust _prioritizePartialPieces)
  when (isJust _autoManageStartup) $ setAutoManageStartup ss (fromIntegral $ fromJust _autoManageStartup)
  when (isJust _rateLimitIpOverhead) $ setRateLimitIpOverhead ss (fromJust _rateLimitIpOverhead)
  when (isJust _announceToAllTrackers) $ setAnnounceToAllTrackers ss (fromJust _announceToAllTrackers)
  when (isJust _announceToAllTiers) $ setAnnounceToAllTiers ss (fromJust _announceToAllTiers)
  when (isJust _preferUdpTrackers) $ setPreferUdpTrackers ss (fromJust _preferUdpTrackers)
  when (isJust _strictSuperSeeding) $ setStrictSuperSeeding ss (fromJust _strictSuperSeeding)
  when (isJust _seedingPieceQuota) $ setSeedingPieceQuota ss (fromIntegral $ fromJust _seedingPieceQuota)
  when (isJust _maxSparseRegions) $ setMaxSparseRegions ss (fromIntegral $ fromJust _maxSparseRegions)
  when (isJust _lockDiskCache) $ setLockDiskCache ss (fromJust _lockDiskCache)
  when (isJust _maxRejects) $ setMaxRejects ss (fromIntegral $ fromJust _maxRejects)
  when (isJust _recvSocketBufferSize) $ setRecvSocketBufferSize ss (fromIntegral $ fromJust _recvSocketBufferSize)
  when (isJust _sendSocketBufferSize) $ setSendSocketBufferSize ss (fromIntegral $ fromJust _sendSocketBufferSize)
  when (isJust _optimizeHashingForSpeed) $ setOptimizeHashingForSpeed ss (fromJust _optimizeHashingForSpeed)
  when (isJust _fileChecksDelayPerBlock) $ setFileChecksDelayPerBlock ss (fromIntegral $ fromJust _fileChecksDelayPerBlock)
  when (isJust _diskCacheAlgorithm) $ setDiskCacheAlgorithm ss (fromJust _diskCacheAlgorithm)
  when (isJust _readCacheLineSize) $ setReadCacheLineSize ss (fromIntegral $ fromJust _readCacheLineSize)
  when (isJust _writeCacheLineSize) $ setWriteCacheLineSize ss (fromIntegral $ fromJust _writeCacheLineSize)
  when (isJust _optimisticDiskRetry) $ setOptimisticDiskRetry ss (fromIntegral $ fromJust _optimisticDiskRetry)
  when (isJust _disableHashChecks) $ setDisableHashChecks ss (fromJust _disableHashChecks)
  when (isJust _allowReorderedDiskOperations) $ setAllowReorderedDiskOperations ss (fromJust _allowReorderedDiskOperations)
  when (isJust _allowI2pMixed) $ setAllowI2pMixed ss (fromJust _allowI2pMixed)
  when (isJust _maxSuggestPieces) $ setMaxSuggestPieces ss (fromIntegral $ fromJust _maxSuggestPieces)
  when (isJust _dropSkippedRequests) $ setDropSkippedRequests ss (fromJust _dropSkippedRequests)
  when (isJust _lowPrioDisk) $ setLowPrioDisk ss (fromJust _lowPrioDisk)
  when (isJust _localServiceAnnounceInterval) $ setLocalServiceAnnounceInterval ss (fromIntegral $ fromJust _localServiceAnnounceInterval)
  when (isJust _dhtAnnounceInterval) $ setDhtAnnounceInterval ss (fromIntegral $ fromJust _dhtAnnounceInterval)
  when (isJust _udpTrackerTokenExpiry) $ setUdpTrackerTokenExpiry ss (fromIntegral $ fromJust _udpTrackerTokenExpiry)
  when (isJust _volatileReadCache) $ setVolatileReadCache ss (fromJust _volatileReadCache)
  when (isJust _guidedReadCache) $ setGuidedReadCache ss (fromJust _guidedReadCache)
  when (isJust _defaultCacheMinAge) $ setDefaultCacheMinAge ss (fromIntegral $ fromJust _defaultCacheMinAge)
  when (isJust _numOptimisticUnchokeSlots) $ setNumOptimisticUnchokeSlots ss (fromIntegral $ fromJust _numOptimisticUnchokeSlots)
  when (isJust _noAtimeStorage) $ setNoAtimeStorage ss (fromJust _noAtimeStorage)
  when (isJust _defaultEstReciprocationRate) $ setDefaultEstReciprocationRate ss (fromIntegral $ fromJust _defaultEstReciprocationRate)
  when (isJust _increaseEstReciprocationRate) $ setIncreaseEstReciprocationRate ss (fromIntegral $ fromJust _increaseEstReciprocationRate)
  when (isJust _decreaseEstReciprocationRate) $ setDecreaseEstReciprocationRate ss (fromIntegral $ fromJust _decreaseEstReciprocationRate)
  when (isJust _incomingStartsQueuedTorrents) $ setIncomingStartsQueuedTorrents ss (fromJust _incomingStartsQueuedTorrents)
  when (isJust _reportTrueDownloaded) $ setReportTrueDownloaded ss (fromJust _reportTrueDownloaded)
  when (isJust _strictEndGameMode) $ setStrictEndGameMode ss (fromJust _strictEndGameMode)
  when (isJust _broadcastLsd) $ setBroadcastLsd ss (fromJust _broadcastLsd)
  when (isJust _enableOutgoingUtp) $ setEnableOutgoingUtp ss (fromJust _enableOutgoingUtp)
  when (isJust _enableIncomingUtp) $ setEnableIncomingUtp ss (fromJust _enableIncomingUtp)
  when (isJust _enableOutgoingTcp) $ setEnableOutgoingTcp ss (fromJust _enableOutgoingTcp)
  when (isJust _enableIncomingTcp) $ setEnableIncomingTcp ss (fromJust _enableIncomingTcp)
  when (isJust _maxPexPeers) $ setMaxPexPeers ss (fromIntegral $ fromJust _maxPexPeers)
  when (isJust _ignoreResumeTimestamps) $ setIgnoreResumeTimestamps ss (fromJust _ignoreResumeTimestamps)
  when (isJust _noRecheckIncompleteResume) $ setNoRecheckIncompleteResume ss (fromJust _noRecheckIncompleteResume)
  when (isJust _anonymousMode) $ setAnonymousMode ss (fromJust _anonymousMode)
  when (isJust _forceProxy) $ setForceProxy ss (fromJust _forceProxy)
  when (isJust _tickInterval) $ setTickInterval ss (fromIntegral $ fromJust _tickInterval)
  when (isJust _reportWebSeedDownloads) $ setReportWebSeedDownloads ss (fromJust _reportWebSeedDownloads)
  when (isJust _shareModeTarget) $ setShareModeTarget ss (fromIntegral $ fromJust _shareModeTarget)
  when (isJust _uploadRateLimit) $ setUploadRateLimit ss (fromIntegral $ fromJust _uploadRateLimit)
  when (isJust _downloadRateLimit) $ setDownloadRateLimit ss (fromIntegral $ fromJust _downloadRateLimit)
  when (isJust _localUploadRateLimit) $ setLocalUploadRateLimit ss (fromIntegral $ fromJust _localUploadRateLimit)
  when (isJust _localDownloadRateLimit) $ setLocalDownloadRateLimit ss (fromIntegral $ fromJust _localDownloadRateLimit)
  when (isJust _dhtUploadRateLimit) $ setDhtUploadRateLimit ss (fromIntegral $ fromJust _dhtUploadRateLimit)
  when (isJust _unchokeSlotsLimit) $ setUnchokeSlotsLimit ss (fromIntegral $ fromJust _unchokeSlotsLimit)
  when (isJust _halfOpenLimit) $ setHalfOpenLimit ss (fromIntegral $ fromJust _halfOpenLimit)
  when (isJust _sessionConnectionsLimit) $ setSessionConnectionsLimit ss (fromIntegral $ fromJust _sessionConnectionsLimit)
  when (isJust _connectionsSlack) $ setConnectionsSlack ss (fromIntegral $ fromJust _connectionsSlack)
  when (isJust _utpTargetDelay) $ setUtpTargetDelay ss (fromIntegral $ fromJust _utpTargetDelay)
  when (isJust _utpGainFactor) $ setUtpGainFactor ss (fromIntegral $ fromJust _utpGainFactor)
  when (isJust _utpMinTimeout) $ setUtpMinTimeout ss (fromIntegral $ fromJust _utpMinTimeout)
  when (isJust _utpSynResends) $ setUtpSynResends ss (fromIntegral $ fromJust _utpSynResends)
  when (isJust _utpFinResends) $ setUtpFinResends ss (fromIntegral $ fromJust _utpFinResends)
  when (isJust _utpNumResends) $ setUtpNumResends ss (fromIntegral $ fromJust _utpNumResends)
  when (isJust _utpConnectTimeout) $ setUtpConnectTimeout ss (fromIntegral $ fromJust _utpConnectTimeout)
  when (isJust _utpDynamicSockBuf) $ setUtpDynamicSockBuf ss (fromJust _utpDynamicSockBuf)
  when (isJust _utpLossMultiplier) $ setUtpLossMultiplier ss (fromIntegral $ fromJust _utpLossMultiplier)
  when (isJust _mixedModeAlgorithm) $ setMixedModeAlgorithm ss (fromJust _mixedModeAlgorithm)
  when (isJust _rateLimitUtp) $ setRateLimitUtp ss (fromJust _rateLimitUtp)
  when (isJust _listenQueueSize) $ setListenQueueSize ss (fromIntegral $ fromJust _listenQueueSize)
  when (isJust _announceDoubleNat) $ setAnnounceDoubleNat ss (fromJust _announceDoubleNat)
  when (isJust _torrentConnectBoost) $ setTorrentConnectBoost ss (fromIntegral $ fromJust _torrentConnectBoost)
  when (isJust _seedingOutgoingConnections) $ setSeedingOutgoingConnections ss (fromJust _seedingOutgoingConnections)
  when (isJust _noConnectPrivilegedPorts) $ setNoConnectPrivilegedPorts ss (fromJust _noConnectPrivilegedPorts)
  when (isJust _alertQueueSize) $ setAlertQueueSize ss (fromIntegral $ fromJust _alertQueueSize)
  when (isJust _maxMetadataSize) $ setMaxMetadataSize ss (fromIntegral $ fromJust _maxMetadataSize)
  when (isJust _smoothConnects) $ setSmoothConnects ss (fromJust _smoothConnects)
  when (isJust _alwaysSendUserAgent) $ setAlwaysSendUserAgent ss (fromJust _alwaysSendUserAgent)
  when (isJust _applyIpFilterToTrackers) $ setApplyIpFilterToTrackers ss (fromJust _applyIpFilterToTrackers)
  when (isJust _readJobEvery) $ setReadJobEvery ss (fromIntegral $ fromJust _readJobEvery)
  when (isJust _useDiskReadAhead) $ setUseDiskReadAhead ss (fromJust _useDiskReadAhead)
  when (isJust _lockFiles) $ setLockFiles ss (fromJust _lockFiles)
  when (isJust _sslListen) $ setSslListen ss (fromIntegral $ fromJust _sslListen)
  when (isJust _trackerBackoff) $ setTrackerBackoff ss (fromIntegral $ fromJust _trackerBackoff)
  when (isJust _banWebSeeds) $ setBanWebSeeds ss (fromJust _banWebSeeds)
  when (isJust _maxHttpRecvBufferSize) $ setMaxHttpRecvBufferSize ss (fromIntegral $ fromJust _maxHttpRecvBufferSize)
  when (isJust _supportShareMode) $ setSupportShareMode ss (fromJust _supportShareMode)
  when (isJust _supportMerkleTorrents) $ setSupportMerkleTorrents ss (fromJust _supportMerkleTorrents)
  when (isJust _reportRedundantBytes) $ setReportRedundantBytes ss (fromJust _reportRedundantBytes)
  when (isJust _handshakeClientVersion) $ setHandshakeClientVersion ss (fromJust _handshakeClientVersion)
  when (isJust _useDiskCachePool) $ setUseDiskCachePool ss (fromJust _useDiskCachePool)
  when (isJust _inactiveDownRate) $ setInactiveDownRate ss (fromIntegral $ fromJust _inactiveDownRate)
  when (isJust _inactiveUpRate) $ setInactiveUpRate ss (fromIntegral $ fromJust _inactiveUpRate)

makeLenses ''SessionOpts
