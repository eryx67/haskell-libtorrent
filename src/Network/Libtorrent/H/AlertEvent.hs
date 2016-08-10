{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |

module Network.Libtorrent.H.AlertEvent (AlertEvent(..)
                                       , alertEventHandlers
                                       ) where

import           Control.Monad.IO.Class   (MonadIO)

import           Network.Libtorrent.Alert

-- | Sum type for all libtorrent events
data AlertEvent =
  TorrentAlertEvent                !TorrentAlert
  | PeerAlertEvent                 !PeerAlert
  | TrackerAlertEvent              !TrackerAlert
  | TorrentAddedAlertEvent         !TorrentAddedAlert
  | TorrentRemovedAlertEvent       !TorrentRemovedAlert
  | ReadPieceAlertEvent            !ReadPieceAlert
  | FileCompletedAlertEvent        !FileCompletedAlert
  | FileRenamedAlertEvent          !FileRenamedAlert
  | FileRenameFailedAlertEvent     !FileRenameFailedAlert
  | PerformanceAlertEvent          !PerformanceAlert
  | StateChangedAlertEvent         !StateChangedAlert
  | TrackerErrorAlertEvent         !TrackerErrorAlert
  | TrackerWarningAlertEvent       !TrackerWarningAlert
  | ScrapeReplyAlertEvent          !ScrapeReplyAlert
  | ScrapeFailedAlertEvent         !ScrapeFailedAlert
  | TrackerReplyAlertEvent         !TrackerReplyAlert
  | DhtReplyAlertEvent             !DhtReplyAlert
  | TrackerAnnounceAlertEvent      !TrackerAnnounceAlert
  | HashFailedAlertEvent           !HashFailedAlert
  | PeerBanAlertEvent              !PeerBanAlert
  | PeerUnsnubbedAlertEvent        !PeerUnsnubbedAlert
  | PeerSnubbedAlertEvent          !PeerSnubbedAlert
  | PeerErrorAlertEvent            !PeerErrorAlert
  | PeerConnectAlertEvent          !PeerConnectAlert
  | PeerDisconnectedAlertEvent     !PeerDisconnectedAlert
  | InvalidRequestAlertEvent       !InvalidRequestAlert
  | TorrentFinishedAlertEvent      !TorrentFinishedAlert
  | PieceFinishedAlertEvent        !PieceFinishedAlert
  | RequestDroppedAlertEvent       !RequestDroppedAlert
  | BlockTimeoutAlertEvent         !BlockTimeoutAlert
  | BlockFinishedAlertEvent        !BlockFinishedAlert
  | BlockDownloadingAlertEvent     !BlockDownloadingAlert
  | UnwantedBlockAlertEvent        !UnwantedBlockAlert
  | StorageMovedAlertEvent         !StorageMovedAlert
  | StorageMovedFailedAlertEvent   !StorageMovedFailedAlert
  | TorrentDeletedAlertEvent       !TorrentDeletedAlert
  | TorrentDeleteFailedAlertEvent  !TorrentDeleteFailedAlert
  | SaveResumeDataAlertEvent       !SaveResumeDataAlert
  | SaveResumeDataFailedAlertEvent !SaveResumeDataFailedAlert
  | TorrentPausedAlertEvent        !TorrentPausedAlert
  | TorrentResumedAlertEvent       !TorrentResumedAlert
  | TorrentCheckedAlertEvent       !TorrentCheckedAlert
  | UrlSeedAlertEvent              !UrlSeedAlert
  | FileErrorAlertEvent            !FileErrorAlert
  | MetadataFailedAlertEvent       !MetadataFailedAlert
  | MetadataReceivedAlertEvent     !MetadataReceivedAlert
  | UdpErrorAlertEvent             !UdpErrorAlert
  | ExternalIpAlertEvent           !ExternalIpAlert
  | ListenFailedAlertEvent         !ListenFailedAlert
  | ListenSucceededAlertEvent      !ListenSucceededAlert
  | PortmapErrorAlertEvent         !PortmapErrorAlert
  | PortmapAlertEvent              !PortmapAlert
  | PortmapLogAlertEvent           !PortmapLogAlert
  | FastresumeRejectedAlertEvent   !FastresumeRejectedAlert
  | PeerBlockedAlertEvent          !PeerBlockedAlert
  | DhtAnnounceAlertEvent          !DhtAnnounceAlert
  | DhtGetPeersAlertEvent          !DhtGetPeersAlert
  | StatsAlertEvent                !StatsAlert
  | CacheFlushedAlertEvent         !CacheFlushedAlert
  | AnonymousModeAlertEvent        !AnonymousModeAlert
  | LsdPeerAlertEvent              !LsdPeerAlert
  | TrackeridAlertEvent            !TrackeridAlert
  | DhtBootstrapAlertEvent         !DhtBootstrapAlert
  | TorrentErrorAlertEvent         !TorrentErrorAlert
  | TorrentNeedCertAlertEvent      !TorrentNeedCertAlert
  | IncomingConnectionAlertEvent   !IncomingConnectionAlert
  | AddTorrentAlertEvent           !AddTorrentAlert
  | StateUpdateAlertEvent          !StateUpdateAlert
  | DhtErrorAlertEvent             !DhtErrorAlert
  | DhtImmutableItemAlertEvent     !DhtImmutableItemAlert
  | DhtMutableItemAlertEvent       !DhtMutableItemAlert
  | DhtPutAlertEvent               !DhtPutAlert
  | RssAlertEvent                  !RssAlert
  | RssItemAlertEvent              !RssItemAlert
  | I2pAlertEvent                  !I2pAlert
                deriving Show

-- | Handle all libtorrent alerts in one function as 'AlertEvent's
-- using 'handleAlerts' or 'sessionHandleAlerts'.
alertEventHandlers :: MonadIO m => (AlertEvent -> m a) -> [AlertHandler a m]
alertEventHandlers f = [
    AlertHandler $ \a -> f $ TorrentAddedAlertEvent         a
  , AlertHandler $ \a -> f $ TorrentRemovedAlertEvent       a
  , AlertHandler $ \a -> f $ ReadPieceAlertEvent            a
  , AlertHandler $ \a -> f $ FileCompletedAlertEvent        a
  , AlertHandler $ \a -> f $ FileRenamedAlertEvent          a
  , AlertHandler $ \a -> f $ FileRenameFailedAlertEvent     a
  , AlertHandler $ \a -> f $ PerformanceAlertEvent          a
  , AlertHandler $ \a -> f $ StateChangedAlertEvent         a
  , AlertHandler $ \a -> f $ TrackerErrorAlertEvent         a
  , AlertHandler $ \a -> f $ TrackerWarningAlertEvent       a
  , AlertHandler $ \a -> f $ ScrapeReplyAlertEvent          a
  , AlertHandler $ \a -> f $ ScrapeFailedAlertEvent         a
  , AlertHandler $ \a -> f $ TrackerReplyAlertEvent         a
  , AlertHandler $ \a -> f $ DhtReplyAlertEvent             a
  , AlertHandler $ \a -> f $ TrackerAnnounceAlertEvent      a
  , AlertHandler $ \a -> f $ HashFailedAlertEvent           a
  , AlertHandler $ \a -> f $ PeerBanAlertEvent              a
  , AlertHandler $ \a -> f $ PeerUnsnubbedAlertEvent        a
  , AlertHandler $ \a -> f $ PeerSnubbedAlertEvent          a
  , AlertHandler $ \a -> f $ PeerErrorAlertEvent            a
  , AlertHandler $ \a -> f $ PeerConnectAlertEvent          a
  , AlertHandler $ \a -> f $ PeerDisconnectedAlertEvent     a
  , AlertHandler $ \a -> f $ InvalidRequestAlertEvent       a
  , AlertHandler $ \a -> f $ TorrentFinishedAlertEvent      a
  , AlertHandler $ \a -> f $ PieceFinishedAlertEvent        a
  , AlertHandler $ \a -> f $ RequestDroppedAlertEvent       a
  , AlertHandler $ \a -> f $ BlockTimeoutAlertEvent         a
  , AlertHandler $ \a -> f $ BlockFinishedAlertEvent        a
  , AlertHandler $ \a -> f $ BlockDownloadingAlertEvent     a
  , AlertHandler $ \a -> f $ UnwantedBlockAlertEvent        a
  , AlertHandler $ \a -> f $ StorageMovedAlertEvent         a
  , AlertHandler $ \a -> f $ StorageMovedFailedAlertEvent   a
  , AlertHandler $ \a -> f $ TorrentDeletedAlertEvent       a
  , AlertHandler $ \a -> f $ TorrentDeleteFailedAlertEvent  a
  , AlertHandler $ \a -> f $ SaveResumeDataAlertEvent       a
  , AlertHandler $ \a -> f $ SaveResumeDataFailedAlertEvent a
  , AlertHandler $ \a -> f $ TorrentPausedAlertEvent        a
  , AlertHandler $ \a -> f $ TorrentResumedAlertEvent       a
  , AlertHandler $ \a -> f $ TorrentCheckedAlertEvent       a
  , AlertHandler $ \a -> f $ UrlSeedAlertEvent              a
  , AlertHandler $ \a -> f $ FileErrorAlertEvent            a
  , AlertHandler $ \a -> f $ MetadataFailedAlertEvent       a
  , AlertHandler $ \a -> f $ MetadataReceivedAlertEvent     a
  , AlertHandler $ \a -> f $ UdpErrorAlertEvent             a
  , AlertHandler $ \a -> f $ ExternalIpAlertEvent           a
  , AlertHandler $ \a -> f $ ListenFailedAlertEvent         a
  , AlertHandler $ \a -> f $ ListenSucceededAlertEvent      a
  , AlertHandler $ \a -> f $ PortmapErrorAlertEvent         a
  , AlertHandler $ \a -> f $ PortmapAlertEvent              a
  , AlertHandler $ \a -> f $ PortmapLogAlertEvent           a
  , AlertHandler $ \a -> f $ FastresumeRejectedAlertEvent   a
  , AlertHandler $ \a -> f $ PeerBlockedAlertEvent          a
  , AlertHandler $ \a -> f $ DhtAnnounceAlertEvent          a
  , AlertHandler $ \a -> f $ DhtGetPeersAlertEvent          a
  , AlertHandler $ \a -> f $ StatsAlertEvent                a
  , AlertHandler $ \a -> f $ CacheFlushedAlertEvent         a
  , AlertHandler $ \a -> f $ AnonymousModeAlertEvent        a
  , AlertHandler $ \a -> f $ LsdPeerAlertEvent              a
  , AlertHandler $ \a -> f $ TrackeridAlertEvent            a
  , AlertHandler $ \a -> f $ DhtBootstrapAlertEvent         a
  , AlertHandler $ \a -> f $ TorrentErrorAlertEvent         a
  , AlertHandler $ \a -> f $ TorrentNeedCertAlertEvent      a
  , AlertHandler $ \a -> f $ IncomingConnectionAlertEvent   a
  , AlertHandler $ \a -> f $ AddTorrentAlertEvent           a
  , AlertHandler $ \a -> f $ StateUpdateAlertEvent          a
  , AlertHandler $ \a -> f $ DhtErrorAlertEvent             a
  , AlertHandler $ \a -> f $ DhtImmutableItemAlertEvent     a
  , AlertHandler $ \a -> f $ DhtMutableItemAlertEvent       a
  , AlertHandler $ \a -> f $ DhtPutAlertEvent               a
  , AlertHandler $ \a -> f $ RssAlertEvent                  a
  , AlertHandler $ \a -> f $ RssItemAlertEvent              a
  , AlertHandler $ \a -> f $ I2pAlertEvent                  a
  , AlertHandler $ \a -> f $ PeerAlertEvent                 a
  , AlertHandler $ \a -> f $ TrackerAlertEvent              a
  , AlertHandler $ \a -> f $ TorrentAlertEvent              a
  ]
