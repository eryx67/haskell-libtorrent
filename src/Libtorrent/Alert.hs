{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#alert alert> structure for "Libtorrent"

module Libtorrent.Alert (IsAlert(..)
                        , IsTorrentAlert(..)
                        , IsTrackerAlert(..)
                        , IsPeerAlert(..)
                        , Alert
                        , AlertCategory(..)
                        , AlertHandler(..)
                        , PerformanceWarning(..)
                        , AnnounceEvent(..)
                        , SocketType(..)
                        , ListenOp(..)
                        , BlockedReason(..)
                        , StatsChannel(..)
                        , AnonymousModeKind(..)
                        , RssState(..)
                        , IncomingSocketType(..)
                        , DhtOperation(..)
                        , TorrentAlert
                        , PeerAlert
                        , TrackerAlert
                        , TorrentAddedAlert
                        , TorrentRemovedAlert
                        , ReadPieceAlert
                        , FileCompletedAlert
                        , FileRenamedAlert
                        , FileRenameFailedAlert
                        , PerformanceAlert
                        , StateChangedAlert
                        , TrackerErrorAlert
                        , TrackerWarningAlert
                        , ScrapeReplyAlert
                        , ScrapeFailedAlert
                        , TrackerReplyAlert
                        , DhtReplyAlert
                        , TrackerAnnounceAlert
                        , HashFailedAlert
                        , PeerBanAlert
                        , PeerUnsnubbedAlert
                        , PeerSnubbedAlert
                        , PeerErrorAlert
                        , PeerConnectAlert
                        , PeerDisconnectedAlert
                        , InvalidRequestAlert
                        , TorrentFinishedAlert
                        , PieceFinishedAlert
                        , RequestDroppedAlert
                        , BlockTimeoutAlert
                        , BlockFinishedAlert
                        , BlockDownloadingAlert
                        , UnwantedBlockAlert
                        , StorageMovedAlert
                        , StorageMovedFailedAlert
                        , TorrentDeletedAlert
                        , TorrentDeleteFailedAlert
                        , SaveResumeDataAlert
                        , SaveResumeDataFailedAlert
                        , TorrentPausedAlert
                        , TorrentResumedAlert
                        , TorrentCheckedAlert
                        , UrlSeedAlert
                        , FileErrorAlert
                        , MetadataFailedAlert
                        , MetadataReceivedAlert
                        , UdpErrorAlert
                        , ExternalIpAlert
                        , ListenFailedAlert
                        , ListenSucceededAlert
                        , PortmapErrorAlert
                        , PortmapAlert
                        , PortmapLogAlert
                        , FastresumeRejectedAlert
                        , PeerBlockedAlert
                        , DhtAnnounceAlert
                        , DhtGetPeersAlert
                        , StatsAlert
                        , CacheFlushedAlert
                        , AnonymousModeAlert
                        , LsdPeerAlert
                        , TrackeridAlert
                        , DhtBootstrapAlert
                        , TorrentErrorAlert
                        , TorrentNeedCertAlert
                        , IncomingConnectionAlert
                        , AddTorrentAlert
                        , StateUpdateAlert
                        , DhtErrorAlert
                        , DhtImmutableItemAlert
                        , DhtMutableItemAlert
                        , DhtPutAlert
                        , RssAlert
                        , RssItemAlert
                        , I2pAlert
                        , castAlert
                        , newAlertDeque
                        , alertTimestamp
                        , alertCategory
                        , getHandle
                        , peerAlertIp
                        , peerAlertPid
                        , trackerAlertUrl
                        , readPieceAlertEc
                        , readPieceAlertPiece
                        , readPieceAlertSize
                        , fileCompletedAlertIndex
                        , fileRenamedAlertIndex
                        , fileRenameFailedAlertIndex
                        , fileRenameFailedAlertError
                        , alertMessage
                        , handleAlert
                        , handleAlerts
                        , readPieceAlertBuffer
                        , performanceAlertWarningCode
                        , stateChangedAlertState
                        , stateChangedAlertPrevState
                        , trackerErrorAlertTimesInRow
                        , trackerErrorAlertStatusCode
                        , trackerErrorAlertErrorCode
                        , trackerErrorAlertMsg
                        , trackerWarningAlertMsg
                        , scrapeReplyAlertComplete
                        , scrapeReplyAlertIncomplete
                        , scrapeFailedAlertMsg
                        , trackerReplyAlertNumPeers
                        , dhtReplyAlertNumPeers
                        , trackerAnnounceAlertEvent
                        , hashFailedAlertPieceIndex
                        , peerErrorAlertError
                        , peerConnectAlertSocketType
                        , peerDisconnectedAlertError
                        , invalidRequestAlertRequest
                        , pieceFinishedAlertPieceIndex
                        , requestDroppedAlertBlockIndex
                        , requestDroppedAlertPieceIndex
                        , blockTimeoutAlertBlockIndex
                        , blockTimeoutAlertPieceIndex
                        , blockFinishedAlertBlockIndex
                        , blockFinishedAlertPieceIndex
                        , blockDownloadingAlertBlockIndex
                        , blockDownloadingAlertPieceIndex
                        , blockDownloadingAlertPeerSpeedmsg
                        , unwantedBlockAlertBlockIndex
                        , unwantedBlockAlertPieceIndex
                        , storageMovedAlertPath
                        , storageMovedFailedAlertError
                        , torrentDeletedAlertInfoHash
                        , torrentDeleteFailedAlertInfoHash
                        , torrentDeleteFailedAlertError
                        , saveResumeDataAlertResumeData
                        , saveResumeDataFailedAlertError
                        , urlSeedAlertMsg
                        , urlSeedAlertUrl
                        , fileErrorAlertError
                        , fileErrorAlertFile
                        , metadataFailedAlertError
                        , udpErrorAlertError
                        , externalIpAlertExternalAddress
                        , listenFailedAlertEndpoint
                        , listenFailedAlertError
                        , listenFailedAlertOperation
                        , listenFailedAlertSockType
                        , listenSucceededAlertEndpoint
                        , listenSucceededAlertSockType
                        , portmapErrorAlertError
                        , portmapErrorAlertMapType
                        , portmapErrorAlertMapping
                        , portmapAlertMapType
                        , portmapAlertMapping
                        , portmapAlertExternalPort
                        , portmapLogAlertMsg
                        , portmapLogAlertMapType
                        , fastresumeRejectedAlertError
                        , peerBlockedAlertIp
                        , peerBlockedAlertReason
                        , dhtAnnounceAlertIp
                        , dhtAnnounceAlertPort
                        , dhtAnnounceAlertInfoHash
                        , dhtGetPeersAlertInfoHash
                        , statsAlertTransferred
                        , statsAlertInterval
                        , anonymousModeAlertKind
                        , anonymousModeAlertStr
                        , trackeridAlertTrackerid
                        , rssAlertHandle
                        , rssAlertUrl
                        , rssAlertState
                        , rssAlertError
                        , torrentErrorAlertError
                        , torrentNeedCertAlertError
                        , incomingConnectionAlertSocketType
                        , incomingConnectionAlertIp
                        , addTorrentAlertParams
                        , addTorrentAlertError
                        , stateUpdateAlertStatus
                        , rssItemAlertHandle
                        , rssItemAlertItem
                        , dhtErrorAlertOperation
                        , dhtErrorAlertError
                        , dhtImmutableItemAlertTarget
                        , dhtImmutableItemAlertItem
                        , dhtMutableItemAlertKey
                        , dhtMutableItemAlertSignature
                        , dhtMutableItemAlertSalt
                        , dhtMutableItemAlertSeq
                        , dhtMutableItemAlertItem
                        , dhtPutAlertPublicKey
                        , dhtPutAlertSignature
                        , dhtPutAlertSalt
                        , dhtPutAlertSeq
                        , dhtPutAlertTarget
                        , i2pAlertError
                        ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Proxy(..))
import           Data.Word (Word64)
import           Foreign.C.String (peekCString)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, castForeignPtr )
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr ( Ptr, nullPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)


import           Libtorrent.Bencode
import           Libtorrent.ErrorCode
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.PeerRequest (PeerRequest)
import           Libtorrent.Rss (FeedHandle, FeedItem)
import           Libtorrent.Session.AddTorrentParams
import           Libtorrent.Sha1Hash (Sha1Hash, sha1HashToByteString)
import           Libtorrent.String
import           Libtorrent.TorrentHandle
import           Libtorrent.TorrentHandle.TorrentStatus (TorrentStatus, TorrentState)
import           Libtorrent.Types
import           Libtorrent.Types.ArrayLike (ArrayLike(..))
import           Libtorrent.TH (defineStdVector)


C.context libtorrentCtx

C.include "<libtorrent/alert.hpp>"
C.include "<libtorrent/alert_types.hpp>"
C.include "<libtorrent/torrent_handle.hpp>"
C.include "<libtorrent/bencode.hpp>"

C.include "torrent_handle.hpp"
C.include "session.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

C.verbatim "typedef std::vector<libtorrent::torrent_handle> VectorTorrentHandle;"
C.verbatim "typedef std::vector<libtorrent::torrent_status> VectorTorrentStatus;"

$(defineStdVector "torrent_status" "VectorTorrentStatus" ''C'TorrentStatus ''C'VectorTorrentStatus ''TorrentStatus)

$(defineStdVector "torrent_handle" "VectorTorrentHandle" ''C'TorrentHandle ''C'VectorTorrentHandle ''TorrentHandle)

data AlertCategory =
  AlertCategoryNone
  | ErrorNotification
  | PeerNotification
  | PortMappingNotification
  | StorageNotification
  | TrackerNotification
  | DebugNotification
  | StatusNotification
  | ProgressNotification
  | IpBlockNotification
  | PerformanceWarning
  | DhtNotification
  | StatsNotification
  | SessionLogNotification
  | TorrentLogNotification
  | PeerLogNotification
  | IncomingRequestNotification
  | DhtLogNotification
  | DhtOperationNotification
  | PortMappingLogNotification
  | PickerLogNotification
  | AllCategories
  deriving (Show, Enum, Bounded)

newtype SubAlert a = SubAlert { unSubAlert :: a }

instance Inlinable a => Inlinable (SubAlert a) where
  type (CType (SubAlert a)) = CType a

instance IsAlert a => WithPtr (SubAlert a) where
  withPtr a action =
    withForeignPtr (castForeignPtr . unAlert . toAlert $ unSubAlert a) action

class IsAlert a where
  typeOfAlert :: Proxy a -> CInt

  fromAlert :: Alert -> a
  toAlert :: a -> Alert

class IsTorrentAlert a where
  asTorrentAlert :: a -> (Ptr (CType TorrentAlert) -> IO b) -> IO b

  default asTorrentAlert :: IsAlert a => a -> (Ptr (CType TorrentAlert) -> IO b) -> IO b
  asTorrentAlert a action =
    withPtr (SubAlert . fromAlert $ toAlert a :: SubAlert TorrentAlert) action

class IsTorrentAlert a => IsPeerAlert a where
  asPeerAlert :: a -> (Ptr (CType PeerAlert) -> IO b) -> IO b

  default asPeerAlert :: IsAlert a => a -> (Ptr (CType PeerAlert) -> IO b) -> IO b
  asPeerAlert a action =
    withPtr (SubAlert . fromAlert $ toAlert a :: SubAlert PeerAlert) action

class IsTorrentAlert a => IsTrackerAlert a where
  asTrackerAlert :: a -> (Ptr (CType TrackerAlert) -> IO b) -> IO b

  default asTrackerAlert :: IsAlert a => a -> (Ptr (CType TrackerAlert) -> IO b) -> IO b
  asTrackerAlert a action =
    withPtr (SubAlert . fromAlert $ toAlert a :: SubAlert TrackerAlert) action

data AlertHandler a where
  AlertHandler :: IsAlert b => (b -> IO a) -> AlertHandler a

newtype Alert = Alert { unAlert :: ForeignPtr (CType Alert)}

instance IsAlert Alert where
  typeOfAlert _ = 0
  fromAlert = id
  toAlert = id

instance Show Alert where
  show a =
    "Alert " ++ (T.unpack . unsafePerformIO $ alertWhat a)

instance Inlinable Alert where
  type (CType Alert) = C'Alert

instance FromPtr Alert where
  fromPtr = objFromPtr Alert $ \ptr ->
    [CU.exp| void { delete $(alert * ptr); } |]

instance WithPtr Alert where
  withPtr (Alert fptr) = withForeignPtr fptr

instance Inlinable (StdDeque Alert) where
  type (CType (StdDeque Alert)) = C'DequeAlertPtr

instance FromPtr (StdDeque Alert) where
  fromPtr = objFromPtr StdDeque $ \ptr ->
    [CU.exp| void { delete $(DequeAlertPtr * ptr); } |]

instance WithPtr (StdDeque Alert) where
  withPtr (StdDeque fptr) = withForeignPtr fptr

instance ArrayLike (StdDeque Alert) where
  type ElemType (StdDeque Alert) = Alert

  getElem v i =
    withPtr v $ \vPtr -> do
      ptr <- [CU.block| alert * {
                 DequeAlertPtr v = *$(DequeAlertPtr * vPtr);
                 if ($(size_t i) >= v.size())
                   return NULL;
                 else
                   return v[$(size_t i)];
                }
             |]
      if ptr == nullPtr
      then return Nothing
      else Just <$> fromPtr (pure ptr)

newAlertDeque :: IO (StdDeque Alert)
newAlertDeque =
  fromPtr [CU.exp| DequeAlertPtr * { new std::deque<alert*>()} |]

alertTimestamp :: IsAlert a => a -> IO Word64
alertTimestamp ho =
  withPtr (toAlert ho) $ \hoPtr ->
  [CU.exp| uint64_t { $(alert * hoPtr)->timestamp().time } |]

alertType :: IsAlert a => a -> IO CInt
alertType ho =
  withPtr (toAlert ho) $ \hoPtr ->
  [CU.exp| int { $(alert * hoPtr)->type() } |]

alertWhat :: IsAlert a => a -> IO Text
alertWhat ho =
  withPtr (toAlert ho) $ \hoPtr ->
  fmap T.pack . peekCString =<< [CU.exp| const char * { $(alert * hoPtr)->what() } |]

alertMessage :: IsAlert a => a -> IO Text
alertMessage ho =
  withPtr (toAlert ho) $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(alert * hoPtr)->message()) } |]
  stdStringToText res

alertCategory :: IsAlert a => a -> IO (BitFlags AlertCategory)
alertCategory ho =
  withPtr (toAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(alert * hoPtr)->category() } |]

-- | Try to handle an alert of type @a@.
handleAlert :: IsAlert a => Alert -> (a -> IO r) -> IO (Maybe r)
handleAlert alert hlr =
  case castAlert alert of
    Just a ->
      Just <$> hlr a
    Nothing ->
      pure Nothing

-- | Try to handle an alert by finding a proper handler from a list of handlers.
handleAlerts :: Alert -> [AlertHandler r] -> IO (Maybe r)
handleAlerts alert hlrs =
  handle hlrs
  where
    handle [] = return Nothing
    handle ((AlertHandler hlr):rest) =
      case castAlert alert of
        Just a ->
          Just <$> hlr a
        Nothing ->
          handle rest

castAlert :: forall a . IsAlert a => Alert -> Maybe a
castAlert a =
  if (unsafePerformIO $ alertType a) == typeOfAlert (Proxy :: Proxy a)
  then Just $ fromAlert a
  else Nothing

newtype TorrentAlert = TorrentAlert { unTorrentAlert :: Alert}

instance IsTorrentAlert TorrentAlert

instance Show (TorrentAlert) where
  show = show . toAlert

instance Inlinable (TorrentAlert) where
  type (CType TorrentAlert) = C'TorrentAlert

instance IsAlert TorrentAlert where
  typeOfAlert _ = [CU.pure| int { torrent_alert::alert_type } |]
  fromAlert = TorrentAlert
  toAlert = unTorrentAlert

-- TODO in libtorrent 1.1.0
-- torrentName :: TorrentAlert -> IO Text
-- torrentName ho =
--   withPtr (SubAlert ho) $ \hoPtr ->
--   fmap T.pack . peekCString =<< [CU.exp| const char * { $(torrent_alert * hoPtr)->torrent_name() } |]

getHandle :: IsTorrentAlert ho => ho -> IO TorrentHandle
getHandle ho =
  asTorrentAlert ho $ \hoPtr ->
  fromPtr [CU.exp| torrent_handle * { new torrent_handle($(torrent_alert * hoPtr)->handle) } |]

newtype PeerAlert = PeerAlert { unPeerAlert :: Alert}

instance IsTorrentAlert PeerAlert
instance IsPeerAlert PeerAlert

instance Show (PeerAlert) where
  show = show . toAlert

instance Inlinable (PeerAlert) where
  type (CType (PeerAlert)) = C'PeerAlert

instance IsAlert PeerAlert where
  typeOfAlert _ = [CU.pure| int { peer_alert::alert_type } |]
  fromAlert = PeerAlert
  toAlert = unPeerAlert

peerAlertIp :: IsPeerAlert a => a -> IO (Text, C.CShort)
peerAlertIp ho =
  asPeerAlert ho $ \hoPtr -> do
  addr <- fromPtr [CU.block| string * {
                      tcp::endpoint ep = $(peer_alert * hoPtr)->ip;
                      return new std::string(ep.address().to_string());
                    }
                  |]
  port <- [CU.block| short {
                      tcp::endpoint ep = $(peer_alert * hoPtr)->ip;
                      return ep.port();
                    }
                  |]
  ( , port) <$> stdStringToText addr

peerAlertPid :: IsPeerAlert a => a -> IO Sha1Hash
peerAlertPid ho =
  asPeerAlert ho $ \hoPtr ->
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(peer_alert * hoPtr)->pid) } |]

newtype TrackerAlert = TrackerAlert { unTrackerAlert :: Alert}

instance IsTorrentAlert TrackerAlert
instance IsTrackerAlert TrackerAlert

instance Show (TrackerAlert) where
  show = show . toAlert

instance Inlinable (TrackerAlert) where
  type (CType (TrackerAlert)) = C'TrackerAlert

instance IsAlert TrackerAlert where
  typeOfAlert _ = [CU.pure| int { tracker_alert::alert_type } |]
  fromAlert = TrackerAlert
  toAlert = unTrackerAlert

trackerAlertUrl :: IsTrackerAlert a => a -> IO Text
trackerAlertUrl ho =
  asTrackerAlert ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(tracker_alert * hoPtr)->url) } |]
  stdStringToText res

-- TODO in libtorrent 1.1.0
-- trackerAlertUrl  :: TrackerAlert -> IO Text
-- trackerAlertUrl ho =
--   withPtr (SubAlert ho) $ \hoPtr ->
--   fmap T.pack . peekCString =<< [CU.exp| const char * { $(tracker_alert * hoPtr)->tracker_url() } |]

newtype TorrentAddedAlert = TorrentAddedAlert { unTorrentAddedAlert :: Alert}

instance IsTorrentAlert TorrentAddedAlert

instance Show (TorrentAddedAlert) where
  show = show . toAlert

instance Inlinable (TorrentAddedAlert) where
  type (CType (TorrentAddedAlert)) = C'TorrentAddedAlert

instance IsAlert TorrentAddedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_added_alert::alert_type } |]
  fromAlert = TorrentAddedAlert
  toAlert = unTorrentAddedAlert

newtype TorrentRemovedAlert = TorrentRemovedAlert { unTorrentRemovedAlert :: Alert}

instance IsTorrentAlert TorrentRemovedAlert

instance Show (TorrentRemovedAlert) where
  show = show . toAlert

instance Inlinable (TorrentRemovedAlert) where
  type (CType (TorrentRemovedAlert)) = C'TorrentRemovedAlert

instance IsAlert TorrentRemovedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_removed_alert::alert_type } |]
  fromAlert = TorrentRemovedAlert
  toAlert = unTorrentRemovedAlert

newtype ReadPieceAlert = ReadPieceAlert { unReadPieceAlert :: Alert}

instance IsTorrentAlert ReadPieceAlert

instance Show (ReadPieceAlert) where
  show = show . toAlert

instance Inlinable (ReadPieceAlert) where
  type (CType (ReadPieceAlert)) = C'ReadPieceAlert

instance IsAlert ReadPieceAlert where
  typeOfAlert _ = [CU.pure| int { read_piece_alert::alert_type } |]
  fromAlert = ReadPieceAlert
  toAlert = unReadPieceAlert

readPieceAlertEc :: ReadPieceAlert -> IO ErrorCode
readPieceAlertEc ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(read_piece_alert * hoPtr)->ec) } |]

readPieceAlertBuffer :: ReadPieceAlert -> IO ByteString
readPieceAlertBuffer ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    csize <- readPieceAlertPiece ho
    chars <- [CU.exp| char * { $(read_piece_alert * hoPtr)->buffer.get() } |]
    BS.packCStringLen (chars, fromIntegral csize)

readPieceAlertPiece :: ReadPieceAlert -> IO CInt
readPieceAlertPiece ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(read_piece_alert * hoPtr)->piece } |]

readPieceAlertSize :: ReadPieceAlert -> IO CInt
readPieceAlertSize ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(read_piece_alert * hoPtr)->size } |]

newtype FileCompletedAlert = FileCompletedAlert { unFileCompletedAlert :: Alert}

instance IsTorrentAlert FileCompletedAlert

instance Show (FileCompletedAlert) where
  show = show . toAlert

instance Inlinable (FileCompletedAlert) where
  type (CType (FileCompletedAlert)) = C'FileCompletedAlert

instance IsAlert FileCompletedAlert where
  typeOfAlert _ = [CU.pure| int { file_completed_alert::alert_type } |]
  fromAlert = FileCompletedAlert
  toAlert = unFileCompletedAlert

fileCompletedAlertIndex :: FileCompletedAlert -> IO CInt
fileCompletedAlertIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(file_completed_alert * hoPtr)->index } |]

newtype FileRenamedAlert = FileRenamedAlert { unFileRenamedAlert :: Alert}

instance IsTorrentAlert FileRenamedAlert

instance Show (FileRenamedAlert) where
  show = show . toAlert

instance Inlinable (FileRenamedAlert) where
  type (CType (FileRenamedAlert)) = C'FileRenamedAlert

instance IsAlert FileRenamedAlert where
  typeOfAlert _ = [CU.pure| int { file_renamed_alert::alert_type } |]
  fromAlert = FileRenamedAlert
  toAlert = unFileRenamedAlert

fileRenamedAlertIndex :: FileRenamedAlert -> IO CInt
fileRenamedAlertIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(file_renamed_alert * hoPtr)->index } |]

newtype FileRenameFailedAlert = FileRenameFailedAlert { unFileRenameFailedAlert :: Alert}

instance IsTorrentAlert FileRenameFailedAlert

instance Show (FileRenameFailedAlert) where
  show = show . toAlert

instance Inlinable (FileRenameFailedAlert) where
  type (CType (FileRenameFailedAlert)) = C'FileRenameFailedAlert

instance IsAlert FileRenameFailedAlert where
  typeOfAlert _ = [CU.pure| int { file_rename_failed_alert::alert_type } |]
  fromAlert = FileRenameFailedAlert
  toAlert = unFileRenameFailedAlert

fileRenameFailedAlertIndex :: FileRenameFailedAlert -> IO CInt
fileRenameFailedAlertIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(file_rename_failed_alert * hoPtr)->index } |]

fileRenameFailedAlertError :: FileRenameFailedAlert -> IO ErrorCode
fileRenameFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(file_rename_failed_alert * hoPtr)->error) } |]

data PerformanceWarning =
  OutstandingDiskBufferLimitReached
  | OutstandingRequestLimitReached
  | UploadLimitTooLow
  | DownloadLimitTooLow
  | SendBufferWatermarkTooLow
  | TooManyOptimisticUnchokeSlots
  | TooHighDiskQueueLimit
  | BittyrantWithNoUplimit
  | TooFewOutgoingPorts
  | TooFewFileDescriptors
  | NumWarnings
  deriving (Show, Enum, Bounded)

newtype PerformanceAlert = PerformanceAlert { unPerformanceAlert :: Alert}

instance IsTorrentAlert PerformanceAlert

instance Show (PerformanceAlert) where
  show = show . toAlert

instance Inlinable (PerformanceAlert) where
  type (CType (PerformanceAlert)) = C'PerformanceAlert

instance IsAlert PerformanceAlert where
  typeOfAlert _ = [CU.pure| int { performance_alert::alert_type } |]
  fromAlert = PerformanceAlert
  toAlert = unPerformanceAlert

performanceAlertWarningCode :: PerformanceAlert -> IO PerformanceWarning
performanceAlertWarningCode ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(performance_alert * hoPtr)->warning_code } |]

newtype StateChangedAlert =  StateChangedAlert { unStateChangedAlert :: Alert}

instance IsTorrentAlert StateChangedAlert

instance Show (StateChangedAlert) where
  show = show . toAlert

instance Inlinable (StateChangedAlert) where
  type (CType (StateChangedAlert)) = C'StateChangedAlert

instance IsAlert StateChangedAlert where
  typeOfAlert _ = [CU.pure| int { state_changed_alert::alert_type } |]
  fromAlert = StateChangedAlert
  toAlert = unStateChangedAlert

stateChangedAlertState :: StateChangedAlert -> IO TorrentState
stateChangedAlertState ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(state_changed_alert * hoPtr)->state } |]

stateChangedAlertPrevState :: StateChangedAlert -> IO TorrentState
stateChangedAlertPrevState ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(state_changed_alert * hoPtr)->prev_state } |]

newtype TrackerErrorAlert = TrackerErrorAlert { unTrackerErrorAlert :: Alert}

instance IsTorrentAlert TrackerErrorAlert
instance IsTrackerAlert TrackerErrorAlert

instance Show (TrackerErrorAlert) where
  show = show . toAlert

instance Inlinable (TrackerErrorAlert) where
  type (CType (TrackerErrorAlert)) = C'TrackerErrorAlert

instance IsAlert TrackerErrorAlert where
  typeOfAlert _ = [CU.pure| int { tracker_error_alert::alert_type } |]
  fromAlert = TrackerErrorAlert
  toAlert = unTrackerErrorAlert

trackerErrorAlertTimesInRow :: TrackerErrorAlert -> IO CInt
trackerErrorAlertTimesInRow ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(tracker_error_alert * hoPtr)->times_in_row } |]

trackerErrorAlertStatusCode :: TrackerErrorAlert -> IO CInt
trackerErrorAlertStatusCode ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(tracker_error_alert * hoPtr)->status_code } |]

trackerErrorAlertErrorCode :: TrackerErrorAlert -> IO ErrorCode
trackerErrorAlertErrorCode ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(tracker_error_alert * hoPtr)->error) } |]

trackerErrorAlertMsg :: TrackerErrorAlert -> IO Text
trackerErrorAlertMsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(tracker_error_alert * hoPtr)->msg) } |]
  stdStringToText res

newtype TrackerWarningAlert = TrackerWarningAlert { unTrackerWarningAlert :: Alert}

instance IsTorrentAlert TrackerWarningAlert
instance IsTrackerAlert TrackerWarningAlert

instance Show (TrackerWarningAlert) where
  show = show . toAlert

instance Inlinable (TrackerWarningAlert) where
  type (CType (TrackerWarningAlert)) = C'TrackerWarningAlert

instance IsAlert TrackerWarningAlert where
  typeOfAlert _ = [CU.pure| int { tracker_warning_alert::alert_type } |]
  fromAlert = TrackerWarningAlert
  toAlert = unTrackerWarningAlert

trackerWarningAlertMsg :: TrackerWarningAlert -> IO Text
trackerWarningAlertMsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(tracker_warning_alert * hoPtr)->msg) } |]
  stdStringToText res

newtype ScrapeReplyAlert = ScrapeReplyAlert { unScrapeReplyAlert :: Alert}

instance IsTorrentAlert ScrapeReplyAlert
instance IsTrackerAlert ScrapeReplyAlert

instance Show (ScrapeReplyAlert) where
  show = show . toAlert

instance Inlinable (ScrapeReplyAlert) where
  type (CType (ScrapeReplyAlert)) = C'ScrapeReplyAlert

instance IsAlert ScrapeReplyAlert where
  typeOfAlert _ = [CU.pure| int { scrape_reply_alert::alert_type } |]
  fromAlert = ScrapeReplyAlert
  toAlert = unScrapeReplyAlert

scrapeReplyAlertComplete :: ScrapeReplyAlert -> IO CInt
scrapeReplyAlertComplete ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(scrape_reply_alert * hoPtr)->complete } |]

scrapeReplyAlertIncomplete :: ScrapeReplyAlert -> IO CInt
scrapeReplyAlertIncomplete ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(scrape_reply_alert * hoPtr)->incomplete } |]

newtype ScrapeFailedAlert = ScrapeFailedAlert { unScrapeFailedAlert :: Alert}

instance IsTorrentAlert ScrapeFailedAlert
instance IsTrackerAlert ScrapeFailedAlert

instance Show (ScrapeFailedAlert) where
  show = show . toAlert

instance Inlinable (ScrapeFailedAlert) where
  type (CType (ScrapeFailedAlert)) = C'ScrapeFailedAlert

instance IsAlert ScrapeFailedAlert where
  typeOfAlert _ = [CU.pure| int { scrape_failed_alert::alert_type } |]
  fromAlert = ScrapeFailedAlert
  toAlert = unScrapeFailedAlert

scrapeFailedAlertMsg :: ScrapeFailedAlert -> IO Text
scrapeFailedAlertMsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    str <- fromPtr [CU.exp| string * { new std::string($(scrape_failed_alert * hoPtr)->msg) } |]
    stdStringToText str

newtype TrackerReplyAlert = TrackerReplyAlert { unTrackerReplyAlert :: Alert}

instance IsTorrentAlert TrackerReplyAlert
instance IsTrackerAlert TrackerReplyAlert

instance Show (TrackerReplyAlert) where
  show = show . toAlert

instance Inlinable (TrackerReplyAlert) where
  type (CType (TrackerReplyAlert)) = C'TrackerReplyAlert

instance IsAlert TrackerReplyAlert where
  typeOfAlert _ = [CU.pure| int { tracker_reply_alert::alert_type } |]
  fromAlert = TrackerReplyAlert
  toAlert = unTrackerReplyAlert

trackerReplyAlertNumPeers :: TrackerReplyAlert -> IO CInt
trackerReplyAlertNumPeers ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(tracker_reply_alert * hoPtr)->num_peers } |]

newtype DhtReplyAlert = DhtReplyAlert { unDhtReplyAlert :: Alert}

instance IsTorrentAlert DhtReplyAlert
instance IsTrackerAlert DhtReplyAlert

instance Show (DhtReplyAlert) where
  show = show . toAlert

instance Inlinable (DhtReplyAlert) where
  type (CType (DhtReplyAlert)) = C'DhtReplyAlert

instance IsAlert DhtReplyAlert where
  typeOfAlert _ = [CU.pure| int { dht_reply_alert::alert_type } |]
  fromAlert = DhtReplyAlert
  toAlert = unDhtReplyAlert

dhtReplyAlertNumPeers :: DhtReplyAlert -> IO CInt
dhtReplyAlertNumPeers ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(dht_reply_alert * hoPtr)->num_peers } |]

data AnnounceEvent =
  AnnounceNone
  | AnnounceCompleted
  | AnnounceStarted
  | AnnounceStopped
  deriving (Show, Enum, Bounded)

newtype TrackerAnnounceAlert = TrackerAnnounceAlert { unTrackerAnnounceAlert :: Alert}

instance IsTorrentAlert TrackerAnnounceAlert
instance IsTrackerAlert TrackerAnnounceAlert

instance Show (TrackerAnnounceAlert) where
  show = show . toAlert

instance Inlinable (TrackerAnnounceAlert) where
  type (CType (TrackerAnnounceAlert)) = C'TrackerAnnounceAlert

instance IsAlert TrackerAnnounceAlert where
  typeOfAlert _ = [CU.pure| int { tracker_announce_alert::alert_type } |]
  fromAlert = TrackerAnnounceAlert
  toAlert = unTrackerAnnounceAlert

trackerAnnounceAlertEvent :: TrackerAnnounceAlert -> IO AnnounceEvent
trackerAnnounceAlertEvent ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(tracker_announce_alert * hoPtr)->event } |]

newtype HashFailedAlert = HashFailedAlert { unHashFailedAlert :: Alert}

instance IsTorrentAlert HashFailedAlert

instance Show (HashFailedAlert) where
  show = show . toAlert

instance Inlinable (HashFailedAlert) where
  type (CType (HashFailedAlert)) = C'HashFailedAlert

instance IsAlert HashFailedAlert where
  typeOfAlert _ = [CU.pure| int { hash_failed_alert::alert_type } |]
  fromAlert = HashFailedAlert
  toAlert = unHashFailedAlert

hashFailedAlertPieceIndex :: HashFailedAlert -> IO CInt
hashFailedAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(hash_failed_alert * hoPtr)->piece_index } |]

newtype PeerBanAlert = PeerBanAlert { unPeerBanAlert :: Alert}

instance IsTorrentAlert PeerBanAlert
instance IsPeerAlert PeerBanAlert

instance Show (PeerBanAlert) where
  show = show . toAlert

instance Inlinable (PeerBanAlert) where
  type (CType (PeerBanAlert)) = C'PeerBanAlert

instance IsAlert PeerBanAlert where
  typeOfAlert _ = [CU.pure| int { peer_ban_alert::alert_type } |]
  fromAlert = PeerBanAlert
  toAlert = unPeerBanAlert

newtype PeerUnsnubbedAlert = PeerUnsnubbedAlert { unPeerUnsnubbedAlert :: Alert}

instance IsTorrentAlert PeerUnsnubbedAlert
instance IsPeerAlert PeerUnsnubbedAlert

instance Show (PeerUnsnubbedAlert) where
  show = show . toAlert

instance Inlinable (PeerUnsnubbedAlert) where
  type (CType (PeerUnsnubbedAlert)) = C'PeerUnsnubbedAlert

instance IsAlert PeerUnsnubbedAlert where
  typeOfAlert _ = [CU.pure| int { peer_unsnubbed_alert::alert_type } |]
  fromAlert = PeerUnsnubbedAlert
  toAlert = unPeerUnsnubbedAlert

newtype PeerSnubbedAlert = PeerSnubbedAlert { unPeerSnubbedAlert :: Alert}

instance IsTorrentAlert PeerSnubbedAlert
instance IsPeerAlert PeerSnubbedAlert

instance Show (PeerSnubbedAlert) where
  show = show . toAlert

instance Inlinable (PeerSnubbedAlert) where
  type (CType (PeerSnubbedAlert)) = C'PeerSnubbedAlert

instance IsAlert PeerSnubbedAlert where
  typeOfAlert _ = [CU.pure| int { peer_snubbed_alert::alert_type } |]
  fromAlert = PeerSnubbedAlert
  toAlert = unPeerSnubbedAlert

newtype PeerErrorAlert = PeerErrorAlert { unPeerErrorAlert :: Alert}

instance IsTorrentAlert PeerErrorAlert
instance IsPeerAlert PeerErrorAlert

instance Show (PeerErrorAlert) where
  show = show . toAlert

instance Inlinable (PeerErrorAlert) where
  type (CType (PeerErrorAlert)) = C'PeerErrorAlert

instance IsAlert PeerErrorAlert where
  typeOfAlert _ = [CU.pure| int { peer_error_alert::alert_type } |]
  fromAlert = PeerErrorAlert
  toAlert = unPeerErrorAlert

peerErrorAlertError :: PeerErrorAlert -> IO ErrorCode
peerErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(peer_error_alert * hoPtr)->error) } |]

newtype PeerConnectAlert = PeerConnectAlert { unPeerConnectAlert :: Alert}

instance IsTorrentAlert PeerConnectAlert
instance IsPeerAlert PeerConnectAlert

instance Show (PeerConnectAlert) where
  show = show . toAlert

instance Inlinable (PeerConnectAlert) where
  type (CType (PeerConnectAlert)) = C'PeerConnectAlert

instance IsAlert PeerConnectAlert where
  typeOfAlert _ = [CU.pure| int { peer_connect_alert::alert_type } |]
  fromAlert = PeerConnectAlert
  toAlert = unPeerConnectAlert

peerConnectAlertSocketType :: PeerConnectAlert -> IO CInt
peerConnectAlertSocketType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(peer_connect_alert * hoPtr)->socket_type } |]

newtype PeerDisconnectedAlert = PeerDisconnectedAlert { unPeerDisconnectedAlert :: Alert}

instance IsTorrentAlert PeerDisconnectedAlert
instance IsPeerAlert PeerDisconnectedAlert

instance Show (PeerDisconnectedAlert) where
  show = show . toAlert

instance Inlinable (PeerDisconnectedAlert) where
  type (CType (PeerDisconnectedAlert)) = C'PeerDisconnectedAlert

instance IsAlert PeerDisconnectedAlert where
  typeOfAlert _ = [CU.pure| int { peer_disconnected_alert::alert_type } |]
  fromAlert = PeerDisconnectedAlert
  toAlert = unPeerDisconnectedAlert

peerDisconnectedAlertError :: PeerDisconnectedAlert -> IO ErrorCode
peerDisconnectedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(peer_disconnected_alert * hoPtr)->error) } |]

newtype InvalidRequestAlert = InvalidRequestAlert { unInvalidRequestAlert :: Alert}

instance IsTorrentAlert InvalidRequestAlert
instance IsPeerAlert InvalidRequestAlert

instance Show (InvalidRequestAlert) where
  show = show . toAlert

instance Inlinable (InvalidRequestAlert) where
  type (CType (InvalidRequestAlert)) = C'InvalidRequestAlert

instance IsAlert InvalidRequestAlert where
  typeOfAlert _ = [CU.pure| int { invalid_request_alert::alert_type } |]
  fromAlert = InvalidRequestAlert
  toAlert = unInvalidRequestAlert

invalidRequestAlertRequest :: InvalidRequestAlert -> IO PeerRequest
invalidRequestAlertRequest ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| peer_request * { new peer_request($(invalid_request_alert * hoPtr)->request) } |]

newtype TorrentFinishedAlert = TorrentFinishedAlert { unTorrentFinishedAlert :: Alert}

instance IsTorrentAlert TorrentFinishedAlert

instance Show (TorrentFinishedAlert) where
  show = show . toAlert

instance Inlinable (TorrentFinishedAlert) where
  type (CType (TorrentFinishedAlert)) = C'TorrentFinishedAlert

instance IsAlert TorrentFinishedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_finished_alert::alert_type } |]
  fromAlert = TorrentFinishedAlert
  toAlert = unTorrentFinishedAlert


newtype PieceFinishedAlert = PieceFinishedAlert { unPieceFinishedAlert :: Alert}

instance IsTorrentAlert PieceFinishedAlert

instance Show (PieceFinishedAlert) where
  show = show . toAlert

instance Inlinable (PieceFinishedAlert) where
  type (CType (PieceFinishedAlert)) = C'PieceFinishedAlert

instance IsAlert PieceFinishedAlert where
  typeOfAlert _ = [CU.pure| int { piece_finished_alert::alert_type } |]
  fromAlert = PieceFinishedAlert
  toAlert = unPieceFinishedAlert

pieceFinishedAlertPieceIndex :: PieceFinishedAlert -> IO CInt
pieceFinishedAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(piece_finished_alert * hoPtr)->piece_index } |]

newtype RequestDroppedAlert = RequestDroppedAlert { unRequestDroppedAlert :: Alert}

instance IsTorrentAlert RequestDroppedAlert
instance IsPeerAlert RequestDroppedAlert

instance Show (RequestDroppedAlert) where
  show = show . toAlert

instance Inlinable (RequestDroppedAlert) where
  type (CType (RequestDroppedAlert)) = C'RequestDroppedAlert

instance IsAlert RequestDroppedAlert where
  typeOfAlert _ = [CU.pure| int { request_dropped_alert::alert_type } |]
  fromAlert = RequestDroppedAlert
  toAlert = unRequestDroppedAlert

requestDroppedAlertBlockIndex :: RequestDroppedAlert -> IO CInt
requestDroppedAlertBlockIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(request_dropped_alert * hoPtr)->block_index } |]

requestDroppedAlertPieceIndex :: RequestDroppedAlert -> IO CInt
requestDroppedAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(request_dropped_alert * hoPtr)->piece_index } |]

newtype BlockTimeoutAlert = BlockTimeoutAlert { unBlockTimeoutAlert :: Alert}

instance IsTorrentAlert BlockTimeoutAlert
instance IsPeerAlert BlockTimeoutAlert

instance Show (BlockTimeoutAlert) where
  show = show . toAlert

instance Inlinable (BlockTimeoutAlert) where
  type (CType (BlockTimeoutAlert)) = C'BlockTimeoutAlert

instance IsAlert BlockTimeoutAlert where
  typeOfAlert _ = [CU.pure| int { block_timeout_alert::alert_type } |]
  fromAlert = BlockTimeoutAlert
  toAlert = unBlockTimeoutAlert

blockTimeoutAlertBlockIndex :: BlockTimeoutAlert -> IO CInt
blockTimeoutAlertBlockIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_timeout_alert * hoPtr)->block_index } |]

blockTimeoutAlertPieceIndex :: BlockTimeoutAlert -> IO CInt
blockTimeoutAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_timeout_alert * hoPtr)->piece_index } |]

newtype BlockFinishedAlert = BlockFinishedAlert { unBlockFinishedAlert :: Alert}

instance IsTorrentAlert BlockFinishedAlert
instance IsPeerAlert BlockFinishedAlert

instance Show (BlockFinishedAlert) where
  show = show . toAlert

instance Inlinable (BlockFinishedAlert) where
  type (CType (BlockFinishedAlert)) = C'BlockFinishedAlert

instance IsAlert BlockFinishedAlert where
  typeOfAlert _ = [CU.pure| int { block_finished_alert::alert_type } |]
  fromAlert = BlockFinishedAlert
  toAlert = unBlockFinishedAlert

blockFinishedAlertBlockIndex :: BlockFinishedAlert -> IO CInt
blockFinishedAlertBlockIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_finished_alert * hoPtr)->block_index } |]

blockFinishedAlertPieceIndex :: BlockFinishedAlert -> IO CInt
blockFinishedAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_finished_alert * hoPtr)->piece_index } |]

newtype BlockDownloadingAlert = BlockDownloadingAlert { unBlockDownloadingAlert :: Alert}

instance IsTorrentAlert BlockDownloadingAlert
instance IsPeerAlert BlockDownloadingAlert

instance Show (BlockDownloadingAlert) where
  show = show . toAlert

instance Inlinable (BlockDownloadingAlert) where
  type (CType (BlockDownloadingAlert)) = C'BlockDownloadingAlert

instance IsAlert BlockDownloadingAlert where
  typeOfAlert _ = [CU.pure| int { block_downloading_alert::alert_type } |]
  fromAlert = BlockDownloadingAlert
  toAlert = unBlockDownloadingAlert

blockDownloadingAlertBlockIndex :: BlockDownloadingAlert -> IO CInt
blockDownloadingAlertBlockIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_downloading_alert * hoPtr)->block_index } |]

blockDownloadingAlertPieceIndex :: BlockDownloadingAlert -> IO CInt
blockDownloadingAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(block_downloading_alert * hoPtr)->piece_index } |]

blockDownloadingAlertPeerSpeedmsg :: BlockDownloadingAlert -> IO Text
blockDownloadingAlertPeerSpeedmsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    cstr <- [CU.exp| const char * { $(block_downloading_alert * hoPtr)->peer_speedmsg } |]
    T.pack <$> peekCString cstr

newtype UnwantedBlockAlert = UnwantedBlockAlert { unUnwantedBlockAlert :: Alert}

instance IsTorrentAlert UnwantedBlockAlert
instance IsPeerAlert UnwantedBlockAlert

instance Show (UnwantedBlockAlert) where
  show = show . toAlert

instance Inlinable (UnwantedBlockAlert) where
  type (CType (UnwantedBlockAlert)) = C'UnwantedBlockAlert

instance IsAlert UnwantedBlockAlert where
  typeOfAlert _ = [CU.pure| int { unwanted_block_alert::alert_type } |]
  fromAlert = UnwantedBlockAlert
  toAlert = unUnwantedBlockAlert

unwantedBlockAlertBlockIndex :: UnwantedBlockAlert -> IO CInt
unwantedBlockAlertBlockIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(unwanted_block_alert * hoPtr)->block_index } |]

unwantedBlockAlertPieceIndex :: UnwantedBlockAlert -> IO CInt
unwantedBlockAlertPieceIndex ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(unwanted_block_alert * hoPtr)->piece_index } |]

newtype StorageMovedAlert = StorageMovedAlert { unStorageMovedAlert :: Alert}

instance IsTorrentAlert StorageMovedAlert

instance Show (StorageMovedAlert) where
  show = show . toAlert

instance Inlinable (StorageMovedAlert) where
  type (CType (StorageMovedAlert)) = C'StorageMovedAlert

instance IsAlert StorageMovedAlert where
  typeOfAlert _ = [CU.pure| int { storage_moved_alert::alert_type } |]
  fromAlert = StorageMovedAlert
  toAlert = unStorageMovedAlert

storageMovedAlertPath :: StorageMovedAlert -> IO Text
storageMovedAlertPath ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(storage_moved_alert * hoPtr)->path) } |]
  stdStringToText str

newtype StorageMovedFailedAlert = StorageMovedFailedAlert { unStorageMovedFailedAlert :: Alert}

instance IsTorrentAlert StorageMovedFailedAlert

instance Show (StorageMovedFailedAlert) where
  show = show . toAlert

instance Inlinable (StorageMovedFailedAlert) where
  type (CType (StorageMovedFailedAlert)) = C'StorageMovedFailedAlert

instance IsAlert StorageMovedFailedAlert where
  typeOfAlert _ = [CU.pure| int { storage_moved_failed_alert::alert_type } |]
  fromAlert = StorageMovedFailedAlert
  toAlert = unStorageMovedFailedAlert

storageMovedFailedAlertError :: StorageMovedFailedAlert -> IO ErrorCode
storageMovedFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(storage_moved_failed_alert * hoPtr)->error) } |]

newtype TorrentDeletedAlert = TorrentDeletedAlert { unTorrentDeletedAlert :: Alert}

instance IsTorrentAlert TorrentDeletedAlert

instance Show (TorrentDeletedAlert) where
  show = show . toAlert

instance Inlinable (TorrentDeletedAlert) where
  type (CType (TorrentDeletedAlert)) = C'TorrentDeletedAlert

instance IsAlert TorrentDeletedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_deleted_alert::alert_type } |]
  fromAlert = TorrentDeletedAlert
  toAlert = unTorrentDeletedAlert

torrentDeletedAlertInfoHash :: TorrentDeletedAlert -> IO ByteString
torrentDeletedAlertInfoHash ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_deleted_alert * hoPtr)->info_hash) } |]
  sha1HashToByteString ih

newtype TorrentDeleteFailedAlert = TorrentDeleteFailedAlert { unTorrentDeleteFailedAlert :: Alert}

instance IsTorrentAlert TorrentDeleteFailedAlert

instance Show (TorrentDeleteFailedAlert) where
  show = show . toAlert

instance Inlinable (TorrentDeleteFailedAlert) where
  type (CType (TorrentDeleteFailedAlert)) = C'TorrentDeleteFailedAlert

instance IsAlert TorrentDeleteFailedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_delete_failed_alert::alert_type } |]
  fromAlert = TorrentDeleteFailedAlert
  toAlert = unTorrentDeleteFailedAlert

torrentDeleteFailedAlertInfoHash :: TorrentDeleteFailedAlert -> IO ByteString
torrentDeleteFailedAlertInfoHash ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(torrent_delete_failed_alert * hoPtr)->info_hash) } |]
  sha1HashToByteString ih

torrentDeleteFailedAlertError :: TorrentDeleteFailedAlert -> IO ErrorCode
torrentDeleteFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(torrent_delete_failed_alert * hoPtr)->error) } |]

newtype SaveResumeDataAlert = SaveResumeDataAlert { unSaveResumeDataAlert :: Alert}

instance IsTorrentAlert SaveResumeDataAlert

instance Show (SaveResumeDataAlert) where
  show = show . toAlert

instance Inlinable (SaveResumeDataAlert) where
  type (CType (SaveResumeDataAlert)) = C'SaveResumeDataAlert

instance IsAlert SaveResumeDataAlert where
  typeOfAlert _ = [CU.pure| int { save_resume_data_alert::alert_type } |]
  fromAlert = SaveResumeDataAlert
  toAlert = unSaveResumeDataAlert

saveResumeDataAlertResumeData :: SaveResumeDataAlert -> IO Bencoded
saveResumeDataAlertResumeData ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ePtr <- [CU.exp| entry * {$(save_resume_data_alert * hoPtr)->resume_data.get()} |]
  entryToBencoded ePtr

newtype SaveResumeDataFailedAlert = SaveResumeDataFailedAlert { unSaveResumeDataFailedAlert :: Alert}

instance IsTorrentAlert SaveResumeDataFailedAlert

instance Show (SaveResumeDataFailedAlert) where
  show = show . toAlert

instance Inlinable (SaveResumeDataFailedAlert) where
  type (CType (SaveResumeDataFailedAlert)) = C'SaveResumeDataFailedAlert

instance IsAlert SaveResumeDataFailedAlert where
  typeOfAlert _ = [CU.pure| int { save_resume_data_failed_alert::alert_type } |]
  fromAlert = SaveResumeDataFailedAlert
  toAlert = unSaveResumeDataFailedAlert

saveResumeDataFailedAlertError :: SaveResumeDataFailedAlert -> IO ErrorCode
saveResumeDataFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(save_resume_data_failed_alert * hoPtr)->error) } |]

newtype TorrentPausedAlert = TorrentPausedAlert { unTorrentPausedAlert :: Alert}

instance IsTorrentAlert TorrentPausedAlert

instance Show (TorrentPausedAlert) where
  show = show . toAlert

instance Inlinable (TorrentPausedAlert) where
  type (CType (TorrentPausedAlert)) = C'TorrentPausedAlert

instance IsAlert TorrentPausedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_paused_alert::alert_type } |]
  fromAlert = TorrentPausedAlert
  toAlert = unTorrentPausedAlert

newtype TorrentResumedAlert = TorrentResumedAlert { unTorrentResumedAlert :: Alert}

instance IsTorrentAlert TorrentResumedAlert

instance Show (TorrentResumedAlert) where
  show = show . toAlert

instance Inlinable (TorrentResumedAlert) where
  type (CType (TorrentResumedAlert)) = C'TorrentResumedAlert

instance IsAlert TorrentResumedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_resumed_alert::alert_type } |]
  fromAlert = TorrentResumedAlert
  toAlert = unTorrentResumedAlert

newtype TorrentCheckedAlert = TorrentCheckedAlert { unTorrentCheckedAlert :: Alert}

instance IsTorrentAlert TorrentCheckedAlert

instance Show (TorrentCheckedAlert) where
  show = show . toAlert

instance Inlinable (TorrentCheckedAlert) where
  type (CType (TorrentCheckedAlert)) = C'TorrentCheckedAlert

instance IsAlert TorrentCheckedAlert where
  typeOfAlert _ = [CU.pure| int { torrent_checked_alert::alert_type } |]
  fromAlert = TorrentCheckedAlert
  toAlert = unTorrentCheckedAlert

newtype UrlSeedAlert = UrlSeedAlert { unUrlSeedAlert :: Alert}

instance IsTorrentAlert UrlSeedAlert

instance Show (UrlSeedAlert) where
  show = show . toAlert

instance Inlinable (UrlSeedAlert) where
  type (CType (UrlSeedAlert)) = C'UrlSeedAlert

instance IsAlert UrlSeedAlert where
  typeOfAlert _ = [CU.pure| int { url_seed_alert::alert_type } |]
  fromAlert = UrlSeedAlert
  toAlert = unUrlSeedAlert

urlSeedAlertMsg :: UrlSeedAlert -> IO Text
urlSeedAlertMsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(url_seed_alert * hoPtr)->msg) } |]
  stdStringToText str

urlSeedAlertUrl :: UrlSeedAlert -> IO Text
urlSeedAlertUrl ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(url_seed_alert * hoPtr)->url) } |]
  stdStringToText str

newtype FileErrorAlert = FileErrorAlert { unFileErrorAlert :: Alert}

instance IsTorrentAlert FileErrorAlert

instance Show (FileErrorAlert) where
  show = show . toAlert

instance Inlinable (FileErrorAlert) where
  type (CType (FileErrorAlert)) = C'FileErrorAlert

instance IsAlert FileErrorAlert where
  typeOfAlert _ = [CU.pure| int { file_error_alert::alert_type } |]
  fromAlert = FileErrorAlert
  toAlert = unFileErrorAlert

fileErrorAlertError :: FileErrorAlert -> IO ErrorCode
fileErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(file_error_alert * hoPtr)->error) } |]

fileErrorAlertFile :: FileErrorAlert -> IO Text
fileErrorAlertFile ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new string($(file_error_alert * hoPtr)->file) } |]
  stdStringToText str

newtype MetadataFailedAlert = MetadataFailedAlert { unMetadataFailedAlert :: Alert}

instance IsTorrentAlert MetadataFailedAlert

instance Show (MetadataFailedAlert) where
  show = show . toAlert

instance Inlinable (MetadataFailedAlert) where
  type (CType (MetadataFailedAlert)) = C'MetadataFailedAlert

instance IsAlert MetadataFailedAlert where
  typeOfAlert _ = [CU.pure| int { metadata_failed_alert::alert_type } |]
  fromAlert = MetadataFailedAlert
  toAlert = unMetadataFailedAlert

metadataFailedAlertError :: MetadataFailedAlert -> IO ErrorCode
metadataFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(metadata_failed_alert * hoPtr)->error) } |]

newtype MetadataReceivedAlert = MetadataReceivedAlert { unMetadataReceivedAlert :: Alert}

instance IsTorrentAlert MetadataReceivedAlert

instance Show (MetadataReceivedAlert) where
  show = show . toAlert

instance Inlinable (MetadataReceivedAlert) where
  type (CType (MetadataReceivedAlert)) = C'MetadataReceivedAlert

instance IsAlert MetadataReceivedAlert where
  typeOfAlert _ = [CU.pure| int { metadata_received_alert::alert_type } |]
  fromAlert = MetadataReceivedAlert
  toAlert = unMetadataReceivedAlert

newtype UdpErrorAlert = UdpErrorAlert { unUdpErrorAlert :: Alert}

instance IsTorrentAlert UdpErrorAlert

instance Show (UdpErrorAlert) where
  show = show . toAlert

instance Inlinable (UdpErrorAlert) where
  type (CType (UdpErrorAlert)) = C'UdpErrorAlert

instance IsAlert UdpErrorAlert where
  typeOfAlert _ = [CU.pure| int { udp_error_alert::alert_type } |]
  fromAlert = UdpErrorAlert
  toAlert = unUdpErrorAlert

udpErrorAlertError :: UdpErrorAlert -> IO ErrorCode
udpErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(udp_error_alert * hoPtr)->error) } |]

newtype ExternalIpAlert = ExternalIpAlert { unExternalIpAlert :: Alert}

instance IsTorrentAlert ExternalIpAlert

instance Show (ExternalIpAlert) where
  show = show . toAlert

instance Inlinable (ExternalIpAlert) where
  type (CType (ExternalIpAlert)) = C'ExternalIpAlert

instance IsAlert ExternalIpAlert where
  typeOfAlert _ = [CU.pure| int { external_ip_alert::alert_type } |]
  fromAlert = ExternalIpAlert
  toAlert = unExternalIpAlert

externalIpAlertExternalAddress :: ExternalIpAlert -> IO Text
externalIpAlertExternalAddress ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(external_ip_alert * hoPtr)->external_address.to_string()) } |]
  stdStringToText str

newtype ListenFailedAlert = ListenFailedAlert { unListenFailedAlert :: Alert}

instance Show (ListenFailedAlert) where
  show = show . toAlert

instance Inlinable (ListenFailedAlert) where
  type (CType (ListenFailedAlert)) = C'ListenFailedAlert

instance IsAlert ListenFailedAlert where
  typeOfAlert _ = [CU.pure| int { listen_failed_alert::alert_type } |]
  fromAlert = ListenFailedAlert
  toAlert = unListenFailedAlert

data SocketType =
  SocketTcp
  | SocketTcpSsl
  | SocketUdp
  | SocketI2p
  | SocketSocks5
  deriving (Show, Enum, Bounded)

data ListenOp =
  ListenParseAddr
  | ListenOpen
  | ListenBind
  | ListenListen
  | ListenGetPeerName
  | ListenAccept
  deriving (Show, Enum, Bounded)

listenFailedAlertEndpoint :: ListenFailedAlert -> IO (Text, C.CShort)
listenFailedAlertEndpoint ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    addr <- fromPtr [CU.block| string * {
                        tcp::endpoint ep = $(listen_failed_alert * hoPtr)->endpoint;
                        return new std::string(ep.address().to_string());
                       }
                    |]
    port <- [CU.block| short {
                 tcp::endpoint ep = $(listen_failed_alert * hoPtr)->endpoint;
                 return ep.port();
               }
            |]
    ( , port) <$> stdStringToText addr

listenFailedAlertError :: ListenFailedAlert -> IO ErrorCode
listenFailedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
    fromPtr [CU.exp| error_code * { new error_code($(listen_failed_alert * hoPtr)->error) } |]

listenFailedAlertOperation :: ListenFailedAlert -> IO ListenOp
listenFailedAlertOperation ho =
  withPtr (SubAlert ho) $ \hoPtr ->
    toEnum . fromIntegral <$> [CU.exp| int { $(listen_failed_alert * hoPtr)->operation } |]

listenFailedAlertSockType :: ListenFailedAlert -> IO SocketType
listenFailedAlertSockType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
    toEnum . fromIntegral <$> [CU.exp| int { $(listen_failed_alert * hoPtr)->sock_type } |]

newtype ListenSucceededAlert = ListenSucceededAlert { unListenSucceededAlert :: Alert}

instance Show (ListenSucceededAlert) where
  show = show . toAlert

instance Inlinable (ListenSucceededAlert) where
  type (CType (ListenSucceededAlert)) = C'ListenSucceededAlert

instance IsAlert ListenSucceededAlert where
  typeOfAlert _ = [CU.pure| int { listen_succeeded_alert::alert_type } |]
  fromAlert = ListenSucceededAlert
  toAlert = unListenSucceededAlert

listenSucceededAlertEndpoint :: ListenSucceededAlert -> IO (Text, C.CShort)
listenSucceededAlertEndpoint ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    addr <- fromPtr [CU.block| string * {
                        tcp::endpoint ep = $(listen_succeeded_alert * hoPtr)->endpoint;
                        return new std::string(ep.address().to_string());
                       }
                    |]
    port <- [CU.block| short {
                 tcp::endpoint ep = $(listen_succeeded_alert * hoPtr)->endpoint;
                 return ep.port();
               }
            |]
    ( , port) <$> stdStringToText addr

listenSucceededAlertSockType :: ListenSucceededAlert -> IO SocketType
listenSucceededAlertSockType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
    toEnum . fromIntegral <$> [CU.exp| int { $(listen_succeeded_alert * hoPtr)->sock_type } |]

newtype PortmapErrorAlert = PortmapErrorAlert { unPortmapErrorAlert :: Alert}

instance Show (PortmapErrorAlert) where
  show = show . toAlert

instance Inlinable (PortmapErrorAlert) where
  type (CType (PortmapErrorAlert)) = C'PortmapErrorAlert

instance IsAlert PortmapErrorAlert where
  typeOfAlert _ = [CU.pure| int { portmap_error_alert::alert_type } |]
  fromAlert = PortmapErrorAlert
  toAlert = unPortmapErrorAlert

portmapErrorAlertError :: PortmapErrorAlert -> IO ErrorCode
portmapErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(portmap_error_alert * hoPtr)->error) } |]

portmapErrorAlertMapType :: PortmapErrorAlert -> IO CInt
portmapErrorAlertMapType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_error_alert * hoPtr)->map_type } |]

portmapErrorAlertMapping :: PortmapErrorAlert -> IO CInt
portmapErrorAlertMapping ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_error_alert * hoPtr)->mapping } |]

newtype PortmapAlert = PortmapAlert { unPortmapAlert :: Alert}

instance Show (PortmapAlert) where
  show = show . toAlert

instance Inlinable (PortmapAlert) where
  type (CType (PortmapAlert)) = C'PortmapAlert

instance IsAlert PortmapAlert where
  typeOfAlert _ = [CU.pure| int { portmap_alert::alert_type } |]
  fromAlert = PortmapAlert
  toAlert = unPortmapAlert

portmapAlertMapType :: PortmapAlert -> IO CInt
portmapAlertMapType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_alert * hoPtr)->map_type } |]

portmapAlertMapping :: PortmapAlert -> IO CInt
portmapAlertMapping ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_alert * hoPtr)->mapping } |]

portmapAlertExternalPort :: PortmapAlert -> IO CInt
portmapAlertExternalPort ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_alert * hoPtr)->external_port } |]

newtype PortmapLogAlert = PortmapLogAlert { unPortmapLogAlert :: Alert}

instance Show (PortmapLogAlert) where
  show = show . toAlert

instance Inlinable (PortmapLogAlert) where
  type (CType (PortmapLogAlert)) = C'PortmapLogAlert

instance IsAlert PortmapLogAlert where
  typeOfAlert _ = [CU.pure| int { portmap_log_alert::alert_type } |]
  fromAlert = PortmapLogAlert
  toAlert = unPortmapLogAlert

portmapLogAlertMsg :: PortmapLogAlert -> IO Text
portmapLogAlertMsg ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(portmap_log_alert * hoPtr)->msg) } |]
  stdStringToText str

portmapLogAlertMapType :: PortmapLogAlert -> IO CInt
portmapLogAlertMapType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(portmap_log_alert * hoPtr)->map_type } |]


newtype FastresumeRejectedAlert = FastresumeRejectedAlert { unFastresumeRejectedAlert :: Alert}

instance IsTorrentAlert FastresumeRejectedAlert

instance Show (FastresumeRejectedAlert) where
  show = show . toAlert

instance Inlinable (FastresumeRejectedAlert) where
  type (CType (FastresumeRejectedAlert)) = C'FastresumeRejectedAlert

instance IsAlert FastresumeRejectedAlert where
  typeOfAlert _ = [CU.pure| int { fastresume_rejected_alert::alert_type } |]
  fromAlert = FastresumeRejectedAlert
  toAlert = unFastresumeRejectedAlert

fastresumeRejectedAlertError :: FastresumeRejectedAlert -> IO ErrorCode
fastresumeRejectedAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(fastresume_rejected_alert * hoPtr)->error) } |]

data BlockedReason =
  IpFilter
  | PortFilter
  | I2pMixed
  | PrivilegedPorts
  | UtpDisabled
  | TcpDisabled
  deriving (Show, Enum, Bounded)

newtype PeerBlockedAlert = PeerBlockedAlert { unPeerBlockedAlert :: Alert}

instance IsTorrentAlert PeerBlockedAlert

instance Show (PeerBlockedAlert) where
  show = show . toAlert

instance Inlinable (PeerBlockedAlert) where
  type (CType (PeerBlockedAlert)) = C'PeerBlockedAlert

instance IsAlert PeerBlockedAlert where
  typeOfAlert _ = [CU.pure| int { peer_blocked_alert::alert_type } |]
  fromAlert = PeerBlockedAlert
  toAlert = unPeerBlockedAlert

peerBlockedAlertIp :: PeerBlockedAlert -> IO Text
peerBlockedAlertIp ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(peer_blocked_alert * hoPtr)->ip.to_string()) } |]
  stdStringToText str

peerBlockedAlertReason :: PeerBlockedAlert -> IO BlockedReason
peerBlockedAlertReason ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(peer_blocked_alert * hoPtr)->reason } |]

newtype DhtAnnounceAlert = DhtAnnounceAlert { unDhtAnnounceAlert :: Alert}

instance Show (DhtAnnounceAlert) where
  show = show . toAlert

instance Inlinable (DhtAnnounceAlert) where
  type (CType (DhtAnnounceAlert)) = C'DhtAnnounceAlert

instance IsAlert DhtAnnounceAlert where
  typeOfAlert _ = [CU.pure| int { dht_announce_alert::alert_type } |]
  fromAlert = DhtAnnounceAlert
  toAlert = unDhtAnnounceAlert

dhtAnnounceAlertIp :: DhtAnnounceAlert -> IO Text
dhtAnnounceAlertIp ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr[CU.exp| string * { new std::string($(dht_announce_alert * hoPtr)->ip.to_string()) } |]
  stdStringToText str

dhtAnnounceAlertPort :: DhtAnnounceAlert -> IO CInt
dhtAnnounceAlertPort ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(dht_announce_alert * hoPtr)->port } |]

dhtAnnounceAlertInfoHash :: DhtAnnounceAlert -> IO ByteString
dhtAnnounceAlertInfoHash ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(dht_announce_alert * hoPtr)->info_hash) } |]
  sha1HashToByteString ih

newtype DhtGetPeersAlert = DhtGetPeersAlert { unDhtGetPeersAlert :: Alert}

instance Show (DhtGetPeersAlert) where
  show = show . toAlert

instance Inlinable (DhtGetPeersAlert) where
  type (CType (DhtGetPeersAlert)) = C'DhtGetPeersAlert

instance IsAlert DhtGetPeersAlert where
  typeOfAlert _ = [CU.pure| int { dht_get_peers_alert::alert_type } |]
  fromAlert = DhtGetPeersAlert
  toAlert = unDhtGetPeersAlert

dhtGetPeersAlertInfoHash :: DhtGetPeersAlert -> IO ByteString
dhtGetPeersAlertInfoHash ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ih <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(dht_get_peers_alert * hoPtr)->info_hash) } |]
  sha1HashToByteString ih

data StatsChannel =
  UploadPayload
  | UploadProtocol
  | DownloadPayload
  | DownloadProtocol
  | UploadIpProtocol
  | UploadDhtProtocol
  | UploadTrackerProtocol
  | DownloadIpProtocol
  | DownloadDhtProtocol
  | DownloadTrackerProtocol
  | NumChannels
  deriving (Show, Enum, Bounded)

newtype StatsAlert = StatsAlert { unStatsAlert :: Alert}

instance IsTorrentAlert StatsAlert

instance Show (StatsAlert) where
  show = show . toAlert

instance Inlinable (StatsAlert) where
  type (CType (StatsAlert)) = C'StatsAlert

instance IsAlert StatsAlert where
  typeOfAlert _ = [CU.pure| int { stats_alert::alert_type } |]
  fromAlert = StatsAlert
  toAlert = unStatsAlert

statsAlertTransferred :: StatsAlert -> IO [(StatsChannel, CInt)]
statsAlertTransferred ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  trPtr <- [CU.exp| int * {$(stats_alert * hoPtr)->transferred } |]
  res <- peekArray (fromEnum NumChannels) trPtr
  return $ zip [minBound..] res

statsAlertInterval :: StatsAlert -> IO CInt
statsAlertInterval ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| int { $(stats_alert * hoPtr)->interval } |]

newtype CacheFlushedAlert = CacheFlushedAlert { unCacheFlushedAlert :: Alert}

instance IsTorrentAlert CacheFlushedAlert

instance Show (CacheFlushedAlert) where
  show = show . toAlert

instance Inlinable (CacheFlushedAlert) where
  type (CType (CacheFlushedAlert)) = C'CacheFlushedAlert

instance IsAlert CacheFlushedAlert where
  typeOfAlert _ = [CU.pure| int { cache_flushed_alert::alert_type } |]
  fromAlert = CacheFlushedAlert
  toAlert = unCacheFlushedAlert

data AnonymousModeKind =
  TrackerNotAnonymous
  deriving (Show, Enum, Bounded)

newtype AnonymousModeAlert = AnonymousModeAlert { unAnonymousModeAlert :: Alert}

instance IsTorrentAlert AnonymousModeAlert

instance Show (AnonymousModeAlert) where
  show = show . toAlert

instance Inlinable (AnonymousModeAlert) where
  type (CType (AnonymousModeAlert)) = C'AnonymousModeAlert

instance IsAlert AnonymousModeAlert where
  typeOfAlert _ = [CU.pure| int { anonymous_mode_alert::alert_type } |]
  fromAlert = AnonymousModeAlert
  toAlert = unAnonymousModeAlert

anonymousModeAlertKind :: AnonymousModeAlert -> IO AnonymousModeKind
anonymousModeAlertKind ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(anonymous_mode_alert * hoPtr)->kind } |]

anonymousModeAlertStr :: AnonymousModeAlert -> IO Text
anonymousModeAlertStr ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(anonymous_mode_alert * hoPtr)->str) } |]
  stdStringToText str

newtype LsdPeerAlert = LsdPeerAlert { unLsdPeerAlert :: Alert}

instance IsTorrentAlert LsdPeerAlert
instance IsPeerAlert LsdPeerAlert

instance Show (LsdPeerAlert) where
  show = show . toAlert

instance Inlinable (LsdPeerAlert) where
  type (CType (LsdPeerAlert)) = C'LsdPeerAlert

instance IsAlert LsdPeerAlert where
  typeOfAlert _ = [CU.pure| int { lsd_peer_alert::alert_type } |]
  fromAlert = LsdPeerAlert
  toAlert = unLsdPeerAlert

newtype TrackeridAlert = TrackeridAlert { unTrackeridAlert :: Alert}

instance IsTorrentAlert TrackeridAlert
instance IsTrackerAlert TrackeridAlert

instance Show (TrackeridAlert) where
  show = show . toAlert

instance Inlinable (TrackeridAlert) where
  type (CType (TrackeridAlert)) = C'TrackeridAlert

instance IsAlert TrackeridAlert where
  typeOfAlert _ = [CU.pure| int { trackerid_alert::alert_type } |]
  fromAlert = TrackeridAlert
  toAlert = unTrackeridAlert

trackeridAlertTrackerid :: TrackeridAlert -> IO Text
trackeridAlertTrackerid ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string ($(trackerid_alert * hoPtr)->trackerid) } |]
  stdStringToText str

newtype DhtBootstrapAlert = DhtBootstrapAlert { unDhtBootstrapAlert :: Alert}

instance Show (DhtBootstrapAlert) where
  show = show . toAlert

instance Inlinable (DhtBootstrapAlert) where
  type (CType (DhtBootstrapAlert)) = C'DhtBootstrapAlert

instance IsAlert DhtBootstrapAlert where
  typeOfAlert _ = [CU.pure| int { dht_bootstrap_alert::alert_type } |]
  fromAlert = DhtBootstrapAlert
  toAlert = unDhtBootstrapAlert

data RssState =
  StateUpdating
  | StateUpdated
  | StateError
  deriving (Show, Enum, Bounded)

newtype RssAlert = RssAlert { unRssAlert :: Alert}

instance Show (RssAlert) where
  show = show . toAlert

instance Inlinable (RssAlert) where
  type (CType (RssAlert)) = C'RssAlert

instance IsAlert RssAlert where
  typeOfAlert _ = [CU.pure| int { rss_alert::alert_type } |]
  fromAlert = RssAlert
  toAlert = unRssAlert

rssAlertHandle :: RssAlert -> IO FeedHandle
rssAlertHandle ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| feed_handle * { new feed_handle($(rss_alert * hoPtr)->handle) } |]

rssAlertUrl :: RssAlert -> IO Text
rssAlertUrl ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(rss_alert * hoPtr)->url) } |]
  stdStringToText str

rssAlertState :: RssAlert -> IO RssState
rssAlertState ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(rss_alert * hoPtr)->state } |]

rssAlertError :: RssAlert -> IO ErrorCode
rssAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(rss_alert * hoPtr)->error) } |]

newtype TorrentErrorAlert = TorrentErrorAlert { unTorrentErrorAlert :: Alert}

instance IsTorrentAlert TorrentErrorAlert

instance Show (TorrentErrorAlert) where
  show = show . toAlert

instance Inlinable (TorrentErrorAlert) where
  type (CType (TorrentErrorAlert)) = C'TorrentErrorAlert

instance IsAlert TorrentErrorAlert where
  typeOfAlert _ = [CU.pure| int { torrent_error_alert::alert_type } |]
  fromAlert = TorrentErrorAlert
  toAlert = unTorrentErrorAlert

torrentErrorAlertError :: TorrentErrorAlert -> IO ErrorCode
torrentErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(torrent_error_alert * hoPtr)->error) } |]

newtype TorrentNeedCertAlert = TorrentNeedCertAlert { unTorrentNeedCertAlert :: Alert}

instance IsTorrentAlert TorrentNeedCertAlert

instance Show (TorrentNeedCertAlert) where
  show = show . toAlert

instance Inlinable (TorrentNeedCertAlert) where
  type (CType (TorrentNeedCertAlert)) = C'TorrentNeedCertAlert

instance IsAlert TorrentNeedCertAlert where
  typeOfAlert _ = [CU.pure| int { torrent_need_cert_alert::alert_type } |]
  fromAlert = TorrentNeedCertAlert
  toAlert = unTorrentNeedCertAlert

torrentNeedCertAlertError :: TorrentNeedCertAlert -> IO ErrorCode
torrentNeedCertAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(torrent_need_cert_alert * hoPtr)->error) } |]

data IncomingSocketType =
  IncomingNone
  | IncomingTCP
  | IncomingSocks5
  | IncomingHTTP
  | IncomingUTP
  | IncomingI2p
  | IncomingSslTcp
  | IncomingSslSocks5
  | IncomingHTTPS
  | IncomingSslUTP
  deriving (Show, Enum, Bounded)

newtype IncomingConnectionAlert = IncomingConnectionAlert { unIncomingConnectionAlert :: Alert}

instance Show (IncomingConnectionAlert) where
  show = show . toAlert

instance Inlinable (IncomingConnectionAlert) where
  type (CType (IncomingConnectionAlert)) = C'IncomingConnectionAlert

instance IsAlert IncomingConnectionAlert where
  typeOfAlert _ = [CU.pure| int { incoming_connection_alert::alert_type } |]
  fromAlert = IncomingConnectionAlert
  toAlert = unIncomingConnectionAlert

incomingConnectionAlertSocketType :: IncomingConnectionAlert -> IO IncomingSocketType
incomingConnectionAlertSocketType ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(incoming_connection_alert * hoPtr)->socket_type } |]

incomingConnectionAlertIp :: IncomingConnectionAlert -> IO (Text, C.CShort)
incomingConnectionAlertIp ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
    addr <- fromPtr [CU.block| string * {
                        tcp::endpoint ep = $(incoming_connection_alert * hoPtr)->ip;
                        return new std::string(ep.address().to_string());
                       }
                    |]
    port <- [CU.block| short {
                 tcp::endpoint ep = $(incoming_connection_alert * hoPtr)->ip;
                 return ep.port();
               }
            |]
    ( , port) <$> stdStringToText addr

newtype AddTorrentAlert = AddTorrentAlert { unAddTorrentAlert :: Alert}

instance IsTorrentAlert AddTorrentAlert

instance Show (AddTorrentAlert) where
  show = show . toAlert

instance Inlinable (AddTorrentAlert) where
  type (CType (AddTorrentAlert)) = C'AddTorrentAlert

instance IsAlert AddTorrentAlert where
  typeOfAlert _ = [CU.pure| int { add_torrent_alert::alert_type } |]
  fromAlert = AddTorrentAlert
  toAlert = unAddTorrentAlert

addTorrentAlertParams :: AddTorrentAlert -> IO AddTorrentParams
addTorrentAlertParams ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| add_torrent_params * { new add_torrent_params($(add_torrent_alert * hoPtr)->params) } |]

addTorrentAlertError :: AddTorrentAlert -> IO ErrorCode
addTorrentAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(add_torrent_alert * hoPtr)->error) } |]

newtype StateUpdateAlert = StateUpdateAlert { unStateUpdateAlert :: Alert}

instance Show (StateUpdateAlert) where
  show = show . toAlert

instance Inlinable (StateUpdateAlert) where
  type (CType (StateUpdateAlert)) = C'StateUpdateAlert

instance IsAlert StateUpdateAlert where
  typeOfAlert _ = [CU.pure| int { state_update_alert::alert_type } |]
  fromAlert = StateUpdateAlert
  toAlert = unStateUpdateAlert

stateUpdateAlertStatus :: StateUpdateAlert -> IO (StdVector TorrentStatus)
stateUpdateAlertStatus ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| VectorTorrentStatus * { new VectorTorrentStatus($(state_update_alert * hoPtr)->status) } |]

newtype RssItemAlert = RssItemAlert { unRssItemAlert :: Alert}

instance Show (RssItemAlert) where
  show = show . toAlert

instance Inlinable (RssItemAlert) where
  type (CType (RssItemAlert)) = C'RssItemAlert

instance IsAlert RssItemAlert where
  typeOfAlert _ = [CU.pure| int { rss_item_alert::alert_type } |]
  fromAlert = RssItemAlert
  toAlert = unRssItemAlert

rssItemAlertHandle :: RssItemAlert -> IO FeedHandle
rssItemAlertHandle ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| feed_handle * { new feed_handle($(rss_item_alert * hoPtr)->handle) } |]

rssItemAlertItem :: RssItemAlert -> IO FeedItem
rssItemAlertItem ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| feed_item * { new feed_item($(rss_item_alert * hoPtr)->item) } |]

-- TODO: libtorrent 1.1
-- mmap_cache_alert
-- session_stats_alert

data DhtOperation =
  DhtOperationUnknown
  | DhtOperationHostLookup
  deriving (Show, Enum, Bounded)

newtype DhtErrorAlert = DhtErrorAlert { unDhtErrorAlert :: Alert}

instance Show (DhtErrorAlert) where
  show = show . toAlert

instance Inlinable (DhtErrorAlert) where
  type (CType (DhtErrorAlert)) = C'DhtErrorAlert

instance IsAlert DhtErrorAlert where
  typeOfAlert _ = [CU.pure| int { dht_error_alert::alert_type } |]
  fromAlert = DhtErrorAlert
  toAlert = unDhtErrorAlert

dhtErrorAlertOperation :: DhtErrorAlert -> IO DhtOperation
dhtErrorAlertOperation ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(dht_error_alert * hoPtr)->operation } |]

dhtErrorAlertError :: DhtErrorAlert -> IO ErrorCode
dhtErrorAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(dht_error_alert * hoPtr)->error) } |]

newtype DhtImmutableItemAlert = DhtImmutableItemAlert { unDhtImmutableItemAlert :: Alert}

instance Show (DhtImmutableItemAlert) where
  show = show . toAlert

instance Inlinable (DhtImmutableItemAlert) where
  type (CType (DhtImmutableItemAlert)) = C'DhtImmutableItemAlert

instance IsAlert DhtImmutableItemAlert where
  typeOfAlert _ = [CU.pure| int { dht_immutable_item_alert::alert_type } |]
  fromAlert = DhtImmutableItemAlert
  toAlert = unDhtImmutableItemAlert

dhtImmutableItemAlertTarget :: DhtImmutableItemAlert -> IO ByteString
dhtImmutableItemAlertTarget ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  tgt <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(dht_immutable_item_alert * hoPtr)->target) } |]
  sha1HashToByteString tgt

dhtImmutableItemAlertItem :: DhtImmutableItemAlert -> IO Bencoded
dhtImmutableItemAlertItem ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ePtr <- [CU.exp| entry * { new entry($(dht_immutable_item_alert * hoPtr)->item) } |]
  entryToBencoded ePtr

newtype DhtMutableItemAlert = DhtMutableItemAlert { unDhtMutableItemAlert :: Alert}

instance Show (DhtMutableItemAlert) where
  show = show . toAlert

instance Inlinable (DhtMutableItemAlert) where
  type (CType (DhtMutableItemAlert)) = C'DhtMutableItemAlert

instance IsAlert DhtMutableItemAlert where
  typeOfAlert _ = [CU.pure| int { dht_mutable_item_alert::alert_type } |]
  fromAlert = DhtMutableItemAlert
  toAlert = unDhtMutableItemAlert

dhtMutableItemAlertKey :: DhtMutableItemAlert -> IO ByteString
dhtMutableItemAlertKey ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  allocaArray 32 $ \aPtr -> do
    [CU.block| void {
        boost::array<char, 32> key =$(dht_mutable_item_alert * hoPtr)->key;
        for (int i = 0; i < 32; i++)
          $(char * aPtr)[i] = key[i];
       }
    |]
    BS.packCStringLen (aPtr, 32)

dhtMutableItemAlertSignature :: DhtMutableItemAlert -> IO ByteString
dhtMutableItemAlertSignature ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  allocaArray 64 $ \aPtr -> do
    [CU.block| void {
        boost::array<char, 64> signature =$(dht_mutable_item_alert * hoPtr)->signature;
        for (int i = 0; i < 64; i++)
          $(char * aPtr)[i] = signature[i];
       }
    |]
    BS.packCStringLen (aPtr, 64)

dhtMutableItemAlertSalt :: DhtMutableItemAlert -> IO Text
dhtMutableItemAlertSalt ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(dht_mutable_item_alert * hoPtr)->salt) } |]
  stdStringToText str

dhtMutableItemAlertSeq :: DhtMutableItemAlert -> IO Word64
dhtMutableItemAlertSeq ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| uint64_t { $(dht_mutable_item_alert * hoPtr)->seq } |]

dhtMutableItemAlertItem :: DhtMutableItemAlert -> IO Bencoded
dhtMutableItemAlertItem ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  ePtr <- [CU.exp| entry * { new entry($(dht_mutable_item_alert * hoPtr)->item) } |]
  entryToBencoded ePtr

newtype DhtPutAlert = DhtPutAlert { unDhtPutAlert :: Alert}

instance Show (DhtPutAlert) where
  show = show . toAlert

instance Inlinable (DhtPutAlert) where
  type (CType (DhtPutAlert)) = C'DhtPutAlert

instance IsAlert DhtPutAlert where
  typeOfAlert _ = [CU.pure| int { dht_put_alert::alert_type } |]
  fromAlert = DhtPutAlert
  toAlert = unDhtPutAlert

dhtPutAlertPublicKey :: DhtPutAlert -> IO ByteString
dhtPutAlertPublicKey ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  allocaArray 32 $ \aPtr -> do
    [CU.block| void {
        boost::array<char, 32> key =$(dht_put_alert * hoPtr)->public_key;
        for (int i = 0; i < 32; i++)
          $(char * aPtr)[i] = key[i];
       }
    |]
    BS.packCStringLen (aPtr, 32)

dhtPutAlertSignature :: DhtPutAlert -> IO ByteString
dhtPutAlertSignature ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  allocaArray 64 $ \aPtr -> do
    [CU.block| void {
        boost::array<char, 64> signature =$(dht_put_alert * hoPtr)->signature;
        for (int i = 0; i < 64; i++)
          $(char * aPtr)[i] = signature[i];
       }
    |]
    BS.packCStringLen (aPtr, 64)

dhtPutAlertSalt :: DhtPutAlert -> IO Text
dhtPutAlertSalt ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  str <- fromPtr [CU.exp| string * { new std::string($(dht_put_alert * hoPtr)->salt) } |]
  stdStringToText str

dhtPutAlertSeq :: DhtPutAlert -> IO Word64
dhtPutAlertSeq ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  [CU.exp| uint64_t { $(dht_put_alert * hoPtr)->seq } |]

dhtPutAlertTarget :: DhtPutAlert -> IO ByteString
dhtPutAlertTarget ho =
  withPtr (SubAlert ho) $ \hoPtr -> do
  tgt <- fromPtr [CU.exp| sha1_hash * { new sha1_hash($(dht_put_alert * hoPtr)->target) } |]
  sha1HashToByteString tgt

newtype I2pAlert = I2pAlert { unI2pAlert :: Alert}

instance IsTorrentAlert I2pAlert

instance Show (I2pAlert) where
  show = show . toAlert

instance Inlinable (I2pAlert) where
  type (CType (I2pAlert)) = C'I2pAlert

instance IsAlert I2pAlert where
  typeOfAlert _ = [CU.pure| int { i2p_alert::alert_type } |]
  fromAlert = I2pAlert
  toAlert = unI2pAlert

i2pAlertError :: I2pAlert -> IO ErrorCode
i2pAlertError ho =
  withPtr (SubAlert ho) $ \hoPtr ->
  fromPtr [CU.exp| error_code * { new error_code($(i2p_alert * hoPtr)->error) } |]


-- TODO: libtorrent 1.1
-- dht_outgoing_get_peers_alert
-- log_alert
-- torrent_log_alert
-- peer_log_alert
-- lsd_error_alert
-- dht_lookup
-- dht_routing_bucket
-- dht_stats_alert
-- incoming_request_alert
-- dht_log_alert
-- dht_pkt_alert
-- dht_get_peers_reply_alert
-- dht_direct_response_alert
-- picker_log_alert
