{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Libtorrent.Inline where

import qualified Data.Map as M
import           Data.Monoid ( (<>) )
import           Foreign.Ptr ( Ptr, FunPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C

libtorrentCtx :: C.Context
libtorrentCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> C.bsCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = libtorrentTypesTable }

libtorrentTypesTable :: C.TypesTable
libtorrentTypesTable = M.fromList
  [ (C.TypeName "bool"        , [t| C.CInt |])
  , (C.TypeName "session", [t| C'Session|])
  , (C.TypeName "session_status", [t| C'SessionStatus|])
  , (C.TypeName "utp_status", [t| C'UtpStatus|])
  , (C.TypeName "session_proxy", [t| C'SessionProxy|])
  , (C.TypeName "torrent_info", [t| C'TorrentInfo|])
  , (C.TypeName "create_torrent", [t| C'CreateTorrent|])
  , (C.TypeName "add_torrent_params", [t| C'AddTorrentParams|])
  , (C.TypeName "torrent_handle", [t| C'TorrentHandle|])
  , (C.TypeName "VectorTorrentHandle", [t| C'VectorTorrentHandle|])
  , (C.TypeName "dht_settings", [t| C'DhtSettings|])
  , (C.TypeName "pe_settings", [t| C'PeSettings|])
  , (C.TypeName "proxy_settings", [t| C'ProxySettings|])
  , (C.TypeName "session_settings", [t| C'SessionSettings|])
  , (C.TypeName "feed_handle", [t| C'FeedHandle|])
  , (C.TypeName "feed_settings", [t| C'FeedSettings|])
  , (C.TypeName "feed_status", [t| C'FeedStatus|])
  , (C.TypeName "feed_item", [t| C'FeedItem|])
  , (C.TypeName "VectorFeedItem", [t| C'VectorFeedItem|])
  , (C.TypeName "VectorFeedHandle", [t| C'VectorFeedHandle|])
  , (C.TypeName "peer_info", [t| C'PeerInfo|])
  , (C.TypeName "peer_request", [t| C'PeerRequest|])
  , (C.TypeName "bitfield", [t|C'Bitfield|])
  , (C.TypeName "string", [t| C'String|])
  , (C.TypeName "VectorString", [t| C'VectorString|])
  , (C.TypeName "sha1_hash", [t| C'Sha1Hash|])
  , (C.TypeName "VectorSha1Hash", [t| C'VectorSha1Hash|])
  , (C.TypeName "error_code", [t| C'ErrorCode|])
  , (C.TypeName "VectorPeerInfo", [t| C'VectorPeerInfo|])
  , (C.TypeName "torrent_status", [t| C'TorrentStatus|])
  , (C.TypeName "VectorTorrentStatus", [t| C'VectorTorrentStatus|])
  , (C.TypeName "partial_piece_info", [t| C'PartialPieceInfo|])
  , (C.TypeName "VectorPartialPieceInfo", [t| C'VectorPartialPieceInfo|])
  , (C.TypeName "block_info", [t| C'BlockInfo|])
  , (C.TypeName "VectorBlockInfo", [t| C'VectorBlockInfo|])
  , (C.TypeName "announce_entry", [t| C'AnnounceEntry|])
  , (C.TypeName "VectorAnnounceEntry", [t| C'VectorAnnounceEntry|])
  , (C.TypeName "alert", [t| C'Alert|])
  , (C.TypeName "DequeAlertPtr", [t| C'DequeAlertPtr|])
  , (C.TypeName "torrent_alert", [t| C'TorrentAlert|])
  , (C.TypeName "peer_alert", [t| C'PeerAlert|])
  , (C.TypeName "tracker_alert", [t| C'TrackerAlert|])
  , (C.TypeName "torrent_added_alert"           , [t|C'TorrentAddedAlert|])
  , (C.TypeName "torrent_removed_alert"         , [t|C'TorrentRemovedAlert|])
  , (C.TypeName "read_piece_alert"              , [t|C'ReadPieceAlert|])
  , (C.TypeName "file_completed_alert"          , [t|C'FileCompletedAlert|])
  , (C.TypeName "file_renamed_alert"            , [t|C'FileRenamedAlert|])
  , (C.TypeName "file_rename_failed_alert"      , [t|C'FileRenameFailedAlert|])
  , (C.TypeName "performance_alert"             , [t|C'PerformanceAlert|])
  , (C.TypeName "state_changed_alert"           , [t|C'StateChangedAlert|])
  , (C.TypeName "tracker_error_alert"           , [t|C'TrackerErrorAlert|])
  , (C.TypeName "tracker_warning_alert"         , [t|C'TrackerWarningAlert|])
  , (C.TypeName "scrape_reply_alert"            , [t|C'ScrapeReplyAlert|])
  , (C.TypeName "scrape_failed_alert"           , [t|C'ScrapeFailedAlert|])
  , (C.TypeName "tracker_reply_alert"           , [t|C'TrackerReplyAlert|])
  , (C.TypeName "dht_reply_alert"               , [t|C'DhtReplyAlert|])
  , (C.TypeName "tracker_announce_alert"        , [t|C'TrackerAnnounceAlert|])
  , (C.TypeName "hash_failed_alert"             , [t|C'HashFailedAlert|])
  , (C.TypeName "peer_ban_alert"                , [t|C'PeerBanAlert|])
  , (C.TypeName "peer_unsnubbed_alert"          , [t|C'PeerUnsnubbedAlert|])
  , (C.TypeName "peer_snubbed_alert"            , [t|C'PeerSnubbedAlert|])
  , (C.TypeName "peer_error_alert"              , [t|C'PeerErrorAlert|])
  , (C.TypeName "peer_connect_alert"            , [t|C'PeerConnectAlert|])
  , (C.TypeName "peer_disconnected_alert"       , [t|C'PeerDisconnectedAlert|])
  , (C.TypeName "invalid_request_alert"         , [t|C'InvalidRequestAlert|])
  , (C.TypeName "torrent_finished_alert"        , [t|C'TorrentFinishedAlert|])
  , (C.TypeName "piece_finished_alert"          , [t|C'PieceFinishedAlert|])
  , (C.TypeName "request_dropped_alert"         , [t|C'RequestDroppedAlert|])
  , (C.TypeName "block_timeout_alert"           , [t|C'BlockTimeoutAlert|])
  , (C.TypeName "block_finished_alert"          , [t|C'BlockFinishedAlert|])
  , (C.TypeName "block_downloading_alert"       , [t|C'BlockDownloadingAlert|])
  , (C.TypeName "unwanted_block_alert"          , [t|C'UnwantedBlockAlert|])
  , (C.TypeName "storage_moved_alert"           , [t|C'StorageMovedAlert|])
  , (C.TypeName "storage_moved_failed_alert"    , [t|C'StorageMovedFailedAlert|])
  , (C.TypeName "torrent_deleted_alert"         , [t|C'TorrentDeletedAlert|])
  , (C.TypeName "torrent_delete_failed_alert"   , [t|C'TorrentDeleteFailedAlert|])
  , (C.TypeName "save_resume_data_alert"        , [t|C'SaveResumeDataAlert|])
  , (C.TypeName "save_resume_data_failed_alert" , [t|C'SaveResumeDataFailedAlert|])
  , (C.TypeName "torrent_paused_alert"          , [t|C'TorrentPausedAlert|])
  , (C.TypeName "torrent_resumed_alert"         , [t|C'TorrentResumedAlert|])
  , (C.TypeName "torrent_checked_alert"         , [t|C'TorrentCheckedAlert|])
  , (C.TypeName "url_seed_alert"                , [t|C'UrlSeedAlert|])
  , (C.TypeName "file_error_alert"              , [t|C'FileErrorAlert|])
  , (C.TypeName "metadata_failed_alert"         , [t|C'MetadataFailedAlert|])
  , (C.TypeName "metadata_received_alert"       , [t|C'MetadataReceivedAlert|])
  , (C.TypeName "udp_error_alert"               , [t|C'UdpErrorAlert|])
  , (C.TypeName "external_ip_alert"             , [t|C'ExternalIpAlert|])
  , (C.TypeName "listen_failed_alert"           , [t|C'ListenFailedAlert|])
  , (C.TypeName "listen_succeeded_alert"        , [t|C'ListenSucceededAlert|])
  , (C.TypeName "portmap_error_alert"           , [t|C'PortmapErrorAlert|])
  , (C.TypeName "portmap_alert"                 , [t|C'PortmapAlert|])
  , (C.TypeName "portmap_log_alert"             , [t|C'PortmapLogAlert|])
  , (C.TypeName "fastresume_rejected_alert"     , [t|C'FastresumeRejectedAlert|])
  , (C.TypeName "peer_blocked_alert"            , [t|C'PeerBlockedAlert|])
  , (C.TypeName "dht_announce_alert"            , [t|C'DhtAnnounceAlert|])
  , (C.TypeName "dht_get_peers_alert"           , [t|C'DhtGetPeersAlert|])
  , (C.TypeName "stats_alert"                   , [t|C'StatsAlert|])
  , (C.TypeName "cache_flushed_alert"           , [t|C'CacheFlushedAlert|])
  , (C.TypeName "anonymous_mode_alert"          , [t|C'AnonymousModeAlert|])
  , (C.TypeName "lsd_peer_alert"                , [t|C'LsdPeerAlert|])
  , (C.TypeName "trackerid_alert"               , [t|C'TrackeridAlert|])
  , (C.TypeName "dht_bootstrap_alert"           , [t|C'DhtBootstrapAlert|])
  , (C.TypeName "torrent_error_alert"           , [t|C'TorrentErrorAlert|])
  , (C.TypeName "torrent_need_cert_alert"       , [t|C'TorrentNeedCertAlert|])
  , (C.TypeName "incoming_connection_alert"     , [t|C'IncomingConnectionAlert|])
  , (C.TypeName "add_torrent_alert"             , [t|C'AddTorrentAlert|])
  , (C.TypeName "state_update_alert"            , [t|C'StateUpdateAlert|])
  , (C.TypeName "mmap_cache_alert"              , [t|C'MmapCacheAlert|])
  , (C.TypeName "session_stats_alert"           , [t|C'SessionStatsAlert|])
  , (C.TypeName "dht_error_alert"               , [t|C'DhtErrorAlert|])
  , (C.TypeName "dht_immutable_item_alert"      , [t|C'DhtImmutableItemAlert|])
  , (C.TypeName "dht_mutable_item_alert"        , [t|C'DhtMutableItemAlert|])
  , (C.TypeName "dht_put_alert"                 , [t|C'DhtPutAlert|])
  , (C.TypeName "rss_alert"                     , [t|C'RssAlert|])
  , (C.TypeName "rss_item_alert"                , [t|C'RssItemAlert|])
  , (C.TypeName "i2p_alert"                     , [t|C'I2pAlert|])
  , (C.TypeName "dht_outgoing_get_peers_alert"  , [t|C'DhtOutgoingGetPeersAlert|])
  , (C.TypeName "log_alert"                     , [t|C'LogAlert|])
  , (C.TypeName "torrent_log_alert"             , [t|C'TorrentLogAlert|])
  , (C.TypeName "peer_log_alert"                , [t|C'PeerLogAlert|])
  , (C.TypeName "lsd_error_alert"               , [t|C'LsdErrorAlert|])
  , (C.TypeName "dht_lookup"                    , [t|C'DhtLookup|])
  , (C.TypeName "dht_routing_bucket"            , [t|C'DhtRoutingBucket|])
  , (C.TypeName "dht_stats_alert"               , [t|C'DhtStatsAlert|])
  , (C.TypeName "incoming_request_alert"        , [t|C'IncomingRequestAlert|])
  , (C.TypeName "dht_log_alert"                 , [t|C'DhtLogAlert|])
  , (C.TypeName "dht_pkt_alert"                 , [t|C'DhtPktAlert|])
  , (C.TypeName "dht_get_peers_reply_alert"     , [t|C'DhtGetPeersReplyAlert|])
  , (C.TypeName "dht_direct_response_alert"     , [t|C'DhtDirectResponseAlert|])
  , (C.TypeName "picker_log_alert"              , [t|C'PickerLogAlert|])
  , (C.TypeName "VectorChar"                    , [t| C'VectorChar|])
  , (C.TypeName "VectorUint8"                   , [t| C'VectorUint8|])
  , (C.TypeName "Plugin"                        , [t|C'Plugin|])
  , (C.TypeName "TorrentPlugin"                 , [t|C'TorrentPlugin|])
  , (C.TypeName "entry"                         , [t|C'BencodeEntry|])
  , (C.TypeName "file_entry"                    , [t|C'FileEntry|])
  , (C.TypeName "file_slice"                    , [t|C'FileSlice|])
  , (C.TypeName "file_storage"                  , [t|C'FileStorage|])
  , (C.TypeName "VectorFileSlice"               , [t|C'VectorFileSlice|])
  -- callbacks
  , (C.TypeName "AlertDispatchCallback"         , [t|FunPtr C'AlertDispatchCallback|])
  , (C.TypeName "TorrentStatusFilter"           , [t|FunPtr C'TorrentStatusFilter|])
  , (C.TypeName "SetPieceHashesCb"              , [t|FunPtr (C.CInt -> IO ())|])
  ]

data C'String
data C'Sha1Hash
data C'ErrorCode
data C'Session
data C'SessionProxy
data C'TorrentInfo
data C'CreateTorrent
data C'AddTorrentParams
data C'TorrentHandle
data C'VectorTorrentHandle
data C'FeedHandle
data C'VectorFeedHandle
data C'FeedItem
data C'VectorFeedItem
data C'FeedSettings
data C'FeedStatus
data C'PeerInfo
data C'PeerRequest
data C'TorrentStatus
data C'DhtSettings
data C'PeSettings
data C'ProxySettings
data C'SessionSettings
data C'VectorTorrentStatus
data C'Bitfield
data C'VectorPeerInfo
data C'PartialPieceInfo
data C'VectorPartialPieceInfo
data C'BlockInfo
data C'VectorBlockInfo
data C'AnnounceEntry
data C'VectorAnnounceEntry
data C'Alert
data C'DequeAlertPtr
data C'TorrentAlert
data C'PeerAlert
data C'TrackerAlert
data C'TorrentAddedAlert
data C'TorrentRemovedAlert
data C'ReadPieceAlert
data C'FileCompletedAlert
data C'FileRenamedAlert
data C'FileRenameFailedAlert
data C'PerformanceAlert
data C'StateChangedAlert
data C'TrackerErrorAlert
data C'TrackerWarningAlert
data C'ScrapeReplyAlert
data C'ScrapeFailedAlert
data C'TrackerReplyAlert
data C'DhtReplyAlert
data C'TrackerAnnounceAlert
data C'HashFailedAlert
data C'PeerBanAlert
data C'PeerUnsnubbedAlert
data C'PeerSnubbedAlert
data C'PeerErrorAlert
data C'PeerConnectAlert
data C'PeerDisconnectedAlert
data C'InvalidRequestAlert
data C'TorrentFinishedAlert
data C'PieceFinishedAlert
data C'RequestDroppedAlert
data C'BlockTimeoutAlert
data C'BlockFinishedAlert
data C'BlockDownloadingAlert
data C'UnwantedBlockAlert
data C'StorageMovedAlert
data C'StorageMovedFailedAlert
data C'TorrentDeletedAlert
data C'TorrentDeleteFailedAlert
data C'SaveResumeDataAlert
data C'SaveResumeDataFailedAlert
data C'TorrentPausedAlert
data C'TorrentResumedAlert
data C'TorrentCheckedAlert
data C'UrlSeedAlert
data C'FileErrorAlert
data C'MetadataFailedAlert
data C'MetadataReceivedAlert
data C'UdpErrorAlert
data C'ExternalIpAlert
data C'ListenFailedAlert
data C'ListenSucceededAlert
data C'PortmapErrorAlert
data C'PortmapAlert
data C'PortmapLogAlert
data C'FastresumeRejectedAlert
data C'PeerBlockedAlert
data C'DhtAnnounceAlert
data C'DhtGetPeersAlert
data C'StatsAlert
data C'CacheFlushedAlert
data C'AnonymousModeAlert
data C'LsdPeerAlert
data C'TrackeridAlert
data C'DhtBootstrapAlert
data C'TorrentErrorAlert
data C'TorrentNeedCertAlert
data C'IncomingConnectionAlert
data C'AddTorrentAlert
data C'StateUpdateAlert
data C'MmapCacheAlert
data C'SessionStatsAlert
data C'DhtErrorAlert
data C'DhtImmutableItemAlert
data C'DhtMutableItemAlert
data C'DhtPutAlert
data C'RssAlert
data C'RssItemAlert
data C'I2pAlert
data C'DhtOutgoingGetPeersAlert
data C'LogAlert
data C'TorrentLogAlert
data C'PeerLogAlert
data C'LsdErrorAlert
data C'DhtLookup
data C'DhtRoutingBucket
data C'DhtStatsAlert
data C'IncomingRequestAlert
data C'DhtLogAlert
data C'DhtPktAlert
data C'DhtGetPeersReplyAlert
data C'DhtDirectResponseAlert
data C'PickerLogAlert
data C'VectorChar
data C'VectorUint8
data C'SessionStatus
data C'UtpStatus
data C'TorrentPlugin
data C'Plugin
data C'BencodeEntry
data C'FileEntry
data C'FileSlice
data C'FileStorage
data C'VectorFileSlice
data C'VectorSha1Hash
data C'VectorString

type C'AlertDispatchCallback = Ptr C'Alert -> IO ()
type C'TorrentStatusFilter = Ptr C'TorrentStatus -> IO Bool
