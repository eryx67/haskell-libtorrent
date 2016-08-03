{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | <http://www.libtorrent.org/reference-Core.html#peer-info peer_info> structure for "Libtorrent"

module Network.Libtorrent.PeerInfo (PeerFlags(..)
                           , PeerSourceFlags(..)
                           , ConnectionType(..)
                           , BwState(..)
                           , PeerInfo(..)
                           , getClient
                           , setClient
                           , getPeerInfoPieces
                           , setPeerInfoPieces
                           , getPeerInfoTotalDownload
                           , setPeerInfoTotalDownload
                           , getPeerInfoTotalUpload
                           , setPeerInfoTotalUpload
                           , getLastRequest
                           , setLastRequest
                           , getLastActive
                           , setLastActive
                           , getDownloadQueueTime
                           , setDownloadQueueTime
                           , getPeerInfoFlags
                           , setPeerInfoFlags
                           , getPeerInfoSource
                           , setPeerInfoSource
                           , getUpSpeed
                           , setUpSpeed
                           , getDownSpeed
                           , setDownSpeed
                           , getPayloadUpSpeed
                           , setPayloadUpSpeed
                           , getPayloadDownSpeed
                           , setPayloadDownSpeed
                           , getPid
                           , setPid
                           , getQueueBytes
                           , setQueueBytes
                           , getPeerInfoRequestTimeout
                           , setPeerInfoRequestTimeout
                           , getSendBufferSize
                           , setSendBufferSize
                           , getUsedSendBuffer
                           , setUsedSendBuffer
                           , getReceiveBufferSize
                           , setReceiveBufferSize
                           , getUsedReceiveBuffer
                           , setUsedReceiveBuffer
                           , getNumHashfails
                           , setNumHashfails
                           , getDownloadQueueLength
                           , setDownloadQueueLength
                           , getTimedOutRequests
                           , setTimedOutRequests
                           , getBusyRequests
                           , setBusyRequests
                           , getRequestsInBuffer
                           , setRequestsInBuffer
                           , getTargetDlQueueLength
                           , setTargetDlQueueLength
                           , getUploadQueueLength
                           , setUploadQueueLength
                           , getFailcount
                           , setFailcount
                           , getDownloadingPieceIndex
                           , setDownloadingPieceIndex
                           , getDownloadingBlockIndex
                           , setDownloadingBlockIndex
                           , getDownloadingProgress
                           , setDownloadingProgress
                           , getDownloadingTotal
                           , setDownloadingTotal
                           , getConnectionType
                           , setConnectionType
                           , getRemoteDlRate
                           , setRemoteDlRate
                           , getPendingDiskBytes
                           , setPendingDiskBytes
                           , getSendQuota
                           , setSendQuota
                           , getReceiveQuota
                           , setReceiveQuota
                           , getRtt
                           , setRtt
                           , getPeerInfoNumPieces
                           , setPeerInfoNumPieces
                           , getDownloadRatePeak
                           , setDownloadRatePeak
                           , getUploadRatePeak
                           , setUploadRatePeak
                           , getPeerInfoProgressPpm
                           , setPeerInfoProgressPpm
                           , getEstimatedReciprocationRate
                           , setEstimatedReciprocationRate
                           , getIp
                           , getLocalEndpoint
                           , getReadState
                           , setReadState
                           , getWriteState
                           , setWriteState
                           ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Array.BitArray         (BitArray)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text.Foreign           as TF
import           Data.Word                   (Word64)
import           Foreign.C.Types             (CInt)
import           Foreign.ForeignPtr          (ForeignPtr, withForeignPtr)
import qualified Language.C.Inline           as C
import qualified Language.C.Inline.Cpp       as C
import qualified Language.C.Inline.Unsafe    as CU

import           Network.Libtorrent.Bitfield
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Sha1Hash
import           Network.Libtorrent.String
import           Network.Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/peer_info.hpp>"
C.include "<libtorrent/torrent_handle.hpp>"

C.include "torrent_handle.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

data PeerFlags =
  Interesting
  | Choked
  | RemoteInterested
  | RemoteChoked
  | SupportsExtensions
  | LocalConnection
  | Handshake
  | Connecting
  | Queued
  | OnParole
  | Seed
  | OptimisticUnchoke
  | Snubbed
  | UploadOnly
  | EndgameMode
  | Holepunched
  | I2pSocket
  | UtpSocket
  | SslSocket
  | Rc4Encrypted
  | PlaintextEncrypted
  deriving (Show, Enum, Bounded, Eq, Ord)

data PeerSourceFlags =
  Tracker
  | Dht
  | Pex
  | Lsd
  | ResumeData
  | Incoming
  deriving (Show, Enum, Bounded, Eq, Ord)

data ConnectionType =
  StandardBittorrent
  | WebSeed
  | HttpSeed
  deriving (Show, Enum, Bounded, Eq, Ord)


data BwState =
  BwIdle
  | BwLimit
  | BwNetwork
  | BwDisk
  deriving (Show, Enum, Bounded, Eq, Ord)

newtype PeerInfo = PeerInfo { unPeerInfo :: ForeignPtr (CType PeerInfo)}

instance Show PeerInfo where
  show _ = "PeerInfo"

instance Inlinable PeerInfo where
  type (CType PeerInfo) = C'PeerInfo

instance FromPtr PeerInfo where
  fromPtr = objFromPtr PeerInfo $ \ptr ->
    [CU.exp| void { delete $(peer_info * ptr); } |]

instance WithPtr PeerInfo where
  withPtr (PeerInfo fptr) = withForeignPtr fptr

getClient :: MonadIO m => PeerInfo -> m Text
getClient ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(peer_info * hoPtr)->client) } |]
  stdStringToText res

setClient :: MonadIO m => PeerInfo -> Text -> m ()
setClient ho val = do
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    liftIO . withPtr ho $ \hoPtr ->
      [CU.exp| void { $(peer_info * hoPtr)->client = std::string($(const char * cstr), $(size_t clen))} |]

getPeerInfoPieces :: MonadIO m => PeerInfo -> m (BitArray Int)
getPeerInfoPieces ho =
  liftIO . withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(peer_info * hoPtr)->pieces) } |]
  bitfieldToBitArray bf

setPeerInfoPieces :: MonadIO m => PeerInfo -> (BitArray Int) -> m ()
setPeerInfoPieces ho ba = liftIO $ do
  bf <- bitArrayToBitfield ba
  withPtr bf $ \bfPtr ->
    withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->pieces = bitfield(*$(bitfield * bfPtr))} |]


getPeerInfoTotalDownload :: MonadIO m => PeerInfo -> m Int64
getPeerInfoTotalDownload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(peer_info * hoPtr)->total_download } |]

setPeerInfoTotalDownload :: MonadIO m => PeerInfo -> Int64 -> m ()
setPeerInfoTotalDownload ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->total_download = $(int64_t val)} |]

getPeerInfoTotalUpload :: MonadIO m => PeerInfo -> m Int64
getPeerInfoTotalUpload ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(peer_info * hoPtr)->total_upload } |]

setPeerInfoTotalUpload :: MonadIO m => PeerInfo -> Int64 -> m ()
setPeerInfoTotalUpload ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->total_upload = $(int64_t val)} |]

getLastRequest :: MonadIO m => PeerInfo -> m Word64
getLastRequest ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(peer_info * hoPtr)->last_request.diff } |]

setLastRequest :: MonadIO m => PeerInfo -> Word64 -> m ()
setLastRequest ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->last_request = time_duration($(uint64_t val))} |]

getLastActive :: MonadIO m => PeerInfo -> m Word64
getLastActive ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(peer_info * hoPtr)->last_active.diff } |]

setLastActive :: MonadIO m => PeerInfo -> Word64 -> m ()
setLastActive ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->last_active = time_duration($(uint64_t val))} |]

getDownloadQueueTime :: MonadIO m => PeerInfo -> m Word64
getDownloadQueueTime ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| uint64_t { $(peer_info * hoPtr)->download_queue_time.diff } |]

setDownloadQueueTime :: MonadIO m => PeerInfo -> Word64 -> m ()
setDownloadQueueTime ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(peer_info * hoPtr)->download_queue_time = time_duration($(uint64_t val))} |]

getPeerInfoFlags :: MonadIO m => PeerInfo -> m (BitFlags PeerFlags)
getPeerInfoFlags ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int32_t{ $(peer_info * hoPtr)->flags } |]

setPeerInfoFlags :: MonadIO m => PeerInfo -> BitFlags PeerFlags -> m ()
setPeerInfoFlags ho flags = do
  let val = fromIntegral $ fromEnum flags
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->flags = $(int32_t val)} |]

getPeerInfoSource :: MonadIO m => PeerInfo -> m (BitFlags PeerSourceFlags)
getPeerInfoSource ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int32_t { $(peer_info * hoPtr)->source } |]

setPeerInfoSource :: MonadIO m => PeerInfo -> BitFlags PeerSourceFlags -> m ()
setPeerInfoSource ho flags = do
  let val = fromIntegral $ fromEnum flags
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->source = $(int32_t val)} |]

getUpSpeed :: MonadIO m => PeerInfo -> m CInt
getUpSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->up_speed } |]

setUpSpeed :: MonadIO m => PeerInfo -> CInt -> m ()
setUpSpeed ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->up_speed = $(int val)} |]


getDownSpeed :: MonadIO m => PeerInfo -> m CInt
getDownSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->down_speed } |]

setDownSpeed :: MonadIO m => PeerInfo -> CInt -> m ()
setDownSpeed ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->down_speed = $(int val)} |]


getPayloadUpSpeed :: MonadIO m => PeerInfo -> m CInt
getPayloadUpSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->payload_up_speed } |]

setPayloadUpSpeed :: MonadIO m => PeerInfo -> CInt -> m ()
setPayloadUpSpeed ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->payload_up_speed = $(int val)} |]


getPayloadDownSpeed :: MonadIO m => PeerInfo -> m CInt
getPayloadDownSpeed ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->payload_down_speed } |]

setPayloadDownSpeed :: MonadIO m => PeerInfo -> CInt -> m ()
setPayloadDownSpeed ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->payload_down_speed = $(int val)} |]

getPid :: MonadIO m => PeerInfo -> m Sha1Hash
getPid ho =
  liftIO . withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(peer_info * hoPtr)->pid) } |]

setPid :: MonadIO m => PeerInfo -> Sha1Hash -> m ()
setPid ho val =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->pid = sha1_hash(*$(sha1_hash * valPtr)) } |]

getQueueBytes :: MonadIO m => PeerInfo -> m CInt
getQueueBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(peer_info * hoPtr)->queue_bytes } |]

setQueueBytes :: MonadIO m => PeerInfo -> CInt -> m ()
setQueueBytes ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(peer_info * hoPtr)->queue_bytes = $(int val)} |]

getPeerInfoRequestTimeout :: MonadIO m => PeerInfo -> m CInt
getPeerInfoRequestTimeout ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->request_timeout } |]

setPeerInfoRequestTimeout :: MonadIO m => PeerInfo -> CInt -> m ()
setPeerInfoRequestTimeout ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->request_timeout = $(int val)} |]

getSendBufferSize :: MonadIO m => PeerInfo -> m CInt
getSendBufferSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->send_buffer_size } |]

setSendBufferSize :: MonadIO m => PeerInfo -> CInt -> m ()
setSendBufferSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->send_buffer_size = $(int val)} |]

getUsedSendBuffer :: MonadIO m => PeerInfo -> m CInt
getUsedSendBuffer ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->used_send_buffer } |]

setUsedSendBuffer :: MonadIO m => PeerInfo -> CInt -> m ()
setUsedSendBuffer ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->used_send_buffer = $(int val)} |]

getReceiveBufferSize :: MonadIO m => PeerInfo -> m CInt
getReceiveBufferSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->receive_buffer_size } |]

setReceiveBufferSize :: MonadIO m => PeerInfo -> CInt -> m ()
setReceiveBufferSize ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->receive_buffer_size = $(int val)} |]

getUsedReceiveBuffer :: MonadIO m => PeerInfo -> m CInt
getUsedReceiveBuffer ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->used_receive_buffer } |]

setUsedReceiveBuffer :: MonadIO m => PeerInfo -> CInt -> m ()
setUsedReceiveBuffer ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->used_receive_buffer = $(int val)} |]

getNumHashfails :: MonadIO m => PeerInfo -> m CInt
getNumHashfails ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->num_hashfails } |]

setNumHashfails :: MonadIO m => PeerInfo -> CInt -> m ()
setNumHashfails ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->num_hashfails = $(int val)} |]

getDownloadQueueLength :: MonadIO m => PeerInfo -> m CInt
getDownloadQueueLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->download_queue_length } |]

setDownloadQueueLength :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadQueueLength ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->download_queue_length = $(int val)} |]

getTimedOutRequests :: MonadIO m => PeerInfo -> m CInt
getTimedOutRequests ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->timed_out_requests } |]

setTimedOutRequests :: MonadIO m => PeerInfo -> CInt -> m ()
setTimedOutRequests ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->timed_out_requests = $(int val)} |]

getBusyRequests :: MonadIO m => PeerInfo -> m CInt
getBusyRequests ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->busy_requests } |]

setBusyRequests :: MonadIO m => PeerInfo -> CInt -> m ()
setBusyRequests ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->busy_requests = $(int val)} |]

getRequestsInBuffer :: MonadIO m => PeerInfo -> m CInt
getRequestsInBuffer ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->requests_in_buffer } |]

setRequestsInBuffer :: MonadIO m => PeerInfo -> CInt -> m ()
setRequestsInBuffer ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->requests_in_buffer = $(int val)} |]

getTargetDlQueueLength :: MonadIO m => PeerInfo -> m CInt
getTargetDlQueueLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->target_dl_queue_length } |]

setTargetDlQueueLength :: MonadIO m => PeerInfo -> CInt -> m ()
setTargetDlQueueLength ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->target_dl_queue_length = $(int val)} |]

getUploadQueueLength :: MonadIO m => PeerInfo -> m CInt
getUploadQueueLength ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->upload_queue_length } |]

setUploadQueueLength :: MonadIO m => PeerInfo -> CInt -> m ()
setUploadQueueLength ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->upload_queue_length = $(int val)} |]

getFailcount :: MonadIO m => PeerInfo -> m CInt
getFailcount ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->failcount } |]

setFailcount :: MonadIO m => PeerInfo -> CInt -> m ()
setFailcount ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->failcount = $(int val)} |]

getDownloadingPieceIndex :: MonadIO m => PeerInfo -> m CInt
getDownloadingPieceIndex ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_piece_index } |]

setDownloadingPieceIndex :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadingPieceIndex ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_piece_index = $(int val)} |]

getDownloadingBlockIndex :: MonadIO m => PeerInfo -> m CInt
getDownloadingBlockIndex ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_block_index } |]

setDownloadingBlockIndex :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadingBlockIndex ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_block_index = $(int val)} |]

getDownloadingProgress :: MonadIO m => PeerInfo -> m CInt
getDownloadingProgress ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_progress } |]

setDownloadingProgress :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadingProgress ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_progress = $(int val)} |]

getDownloadingTotal :: MonadIO m => PeerInfo -> m CInt
getDownloadingTotal ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_total } |]

setDownloadingTotal :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadingTotal ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_total = $(int val)} |]

getConnectionType :: MonadIO m => PeerInfo -> m ConnectionType
getConnectionType ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(peer_info * hoPtr)->connection_type } |]

setConnectionType :: MonadIO m => PeerInfo -> ConnectionType -> m ()
setConnectionType ho flag = do
  let val = fromIntegral $ fromEnum flag
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->connection_type = $(int val)} |]

getRemoteDlRate :: MonadIO m => PeerInfo -> m CInt
getRemoteDlRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->remote_dl_rate } |]

setRemoteDlRate :: MonadIO m => PeerInfo -> CInt -> m ()
setRemoteDlRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->remote_dl_rate = $(int val)} |]

getPendingDiskBytes :: MonadIO m => PeerInfo -> m CInt
getPendingDiskBytes ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->pending_disk_bytes } |]

setPendingDiskBytes :: MonadIO m => PeerInfo -> CInt -> m ()
setPendingDiskBytes ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->pending_disk_bytes = $(int val)} |]

-- getPendingDiskReadBytes :: MonadIO m => PeerInfo -> m CInt
-- getPendingDiskReadBytes ho =
--   liftIO . withPtr ho $ \hoPtr ->
--   [CU.exp| int { $(peer_info * hoPtr)->pending_disk_read_bytes } |]

-- setPendingDiskReadBytes :: MonadIO m => PeerInfo -> CInt -> m ()
-- setPendingDiskReadBytes ho val =
--   liftIO . withPtr ho $ \hoPtr ->
--   [CU.exp| void { $(peer_info * hoPtr)->pending_disk_read_bytes = $(int val)} |]

getSendQuota :: MonadIO m => PeerInfo -> m CInt
getSendQuota ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->send_quota } |]

setSendQuota :: MonadIO m => PeerInfo -> CInt -> m ()
setSendQuota ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->send_quota = $(int val)} |]

getReceiveQuota :: MonadIO m => PeerInfo -> m CInt
getReceiveQuota ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->receive_quota } |]

setReceiveQuota :: MonadIO m => PeerInfo -> CInt -> m ()
setReceiveQuota ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->receive_quota = $(int val)} |]

getRtt :: MonadIO m => PeerInfo -> m CInt
getRtt ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->rtt } |]

setRtt :: MonadIO m => PeerInfo -> CInt -> m ()
setRtt ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->rtt = $(int val)} |]

getPeerInfoNumPieces :: MonadIO m => PeerInfo -> m CInt
getPeerInfoNumPieces ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->num_pieces } |]

setPeerInfoNumPieces :: MonadIO m => PeerInfo -> CInt -> m ()
setPeerInfoNumPieces ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->num_pieces = $(int val)} |]

getDownloadRatePeak :: MonadIO m => PeerInfo -> m CInt
getDownloadRatePeak ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->download_rate_peak } |]

setDownloadRatePeak :: MonadIO m => PeerInfo -> CInt -> m ()
setDownloadRatePeak ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->download_rate_peak = $(int val)} |]

getUploadRatePeak :: MonadIO m => PeerInfo -> m CInt
getUploadRatePeak ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->upload_rate_peak } |]

setUploadRatePeak :: MonadIO m => PeerInfo -> CInt -> m ()
setUploadRatePeak ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->upload_rate_peak = $(int val)} |]

getPeerInfoProgressPpm :: MonadIO m => PeerInfo -> m CInt
getPeerInfoProgressPpm ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->progress_ppm } |]

setPeerInfoProgressPpm :: MonadIO m => PeerInfo -> CInt -> m ()
setPeerInfoProgressPpm ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->progress_ppm = $(int val)} |]

getEstimatedReciprocationRate :: MonadIO m => PeerInfo -> m CInt
getEstimatedReciprocationRate ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->estimated_reciprocation_rate } |]

setEstimatedReciprocationRate :: MonadIO m => PeerInfo -> CInt -> m ()
setEstimatedReciprocationRate ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->estimated_reciprocation_rate = $(int val)} |]

getIp :: MonadIO m => PeerInfo -> m (Text, C.CShort)
getIp ho =
  liftIO . withPtr ho $ \hoPtr -> do
  addr <- fromPtr [CU.block| string * {
                      tcp::endpoint ep = $(peer_info * hoPtr)->ip;
                      return new std::string(ep.address().to_string());
                    }
                  |]
  port <- [CU.block| short {
                      tcp::endpoint ep = $(peer_info * hoPtr)->ip;
                      return ep.port();
                    }
                  |]
  ( , port) <$> stdStringToText addr


getLocalEndpoint :: MonadIO m => PeerInfo -> m (Text, C.CShort)
getLocalEndpoint ho =
  liftIO . withPtr ho $ \hoPtr -> do
  addr <- fromPtr [CU.block| string * {
                      tcp::endpoint ep = $(peer_info * hoPtr)->local_endpoint;
                      return new std::string(ep.address().to_string());
                    }
                  |]
  port <- [CU.block| short {
                      tcp::endpoint ep = $(peer_info * hoPtr)->local_endpoint;
                      return ep.port();
                    }
                  |]
  ( , port) <$> stdStringToText addr

getReadState :: MonadIO m => PeerInfo -> m (BitFlags BwState)
getReadState ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| char { $(peer_info * hoPtr)->read_state } |]

setReadState :: MonadIO m => PeerInfo -> BitFlags BwState -> m ()
setReadState ho flags = do
  let val = fromIntegral $ fromEnum flags
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->read_state = $(char val)} |]

getWriteState :: MonadIO m => PeerInfo -> m (BitFlags BwState)
getWriteState ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| char { $(peer_info * hoPtr)->write_state } |]

setWriteState :: MonadIO m => PeerInfo -> BitFlags BwState -> m ()
setWriteState ho flags = do
  let val = fromIntegral $ fromEnum flags
  liftIO . withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->write_state = $(char val)} |]

