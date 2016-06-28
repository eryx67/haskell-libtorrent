{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | <http://www.libtorrent.org/reference-Core.html#peer-info peer_info> structure for "Libtorrent"

module Libtorrent.PeerInfo (PeerFlags(..)
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

import           Data.Array.BitArray (BitArray)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Data.Word (Word64)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Bitfield
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.String
import           Libtorrent.Sha1Hash
import           Libtorrent.Types

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
  deriving (Show, Enum, Bounded)

data PeerSourceFlags =
  Tracker
  | Dht
  | Pex
  | Lsd
  | ResumeData
  | Incoming
  deriving (Show, Enum, Bounded)

data ConnectionType =
  StandardBittorrent
  | WebSeed
  | HttpSeed
  deriving (Show, Enum, Bounded)


data BwState =
  BwIdle
  | BwLimit
  | BwNetwork
  | BwDisk
  deriving (Show, Enum, Bounded)

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

getClient :: PeerInfo -> IO Text
getClient ho =
  withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(peer_info * hoPtr)->client) } |]
  stdStringToText res

setClient :: PeerInfo -> Text -> IO ()
setClient ho val = do
  TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(peer_info * hoPtr)->client = std::string($(const char * cstr), $(size_t clen))} |]

getPeerInfoPieces :: PeerInfo -> IO (BitArray Int)
getPeerInfoPieces ho =
  withPtr ho $ \hoPtr -> do
  bf <- fromPtr [CU.exp| bitfield * { new bitfield($(peer_info * hoPtr)->pieces) } |]
  bitfieldToBitArray bf

setPeerInfoPieces :: PeerInfo -> (BitArray Int) -> IO ()
setPeerInfoPieces ho ba = do
  bf <- bitArrayToBitfield ba
  withPtr bf $ \bfPtr ->
    withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->pieces = bitfield(*$(bitfield * bfPtr))} |]


getPeerInfoTotalDownload :: PeerInfo -> IO Int64
getPeerInfoTotalDownload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(peer_info * hoPtr)->total_download } |]

setPeerInfoTotalDownload :: PeerInfo -> Int64 -> IO ()
setPeerInfoTotalDownload ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->total_download = $(int64_t val)} |]

getPeerInfoTotalUpload :: PeerInfo -> IO Int64
getPeerInfoTotalUpload ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int64_t { $(peer_info * hoPtr)->total_upload } |]

setPeerInfoTotalUpload :: PeerInfo -> Int64 -> IO ()
setPeerInfoTotalUpload ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->total_upload = $(int64_t val)} |]

getLastRequest :: PeerInfo -> IO Word64
getLastRequest ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(peer_info * hoPtr)->last_request.diff } |]

setLastRequest :: PeerInfo -> Word64 -> IO ()
setLastRequest ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->last_request = time_duration($(uint64_t val))} |]

getLastActive :: PeerInfo -> IO Word64
getLastActive ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| uint64_t { $(peer_info * hoPtr)->last_active.diff } |]

setLastActive :: PeerInfo -> Word64 -> IO ()
setLastActive ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->last_active = time_duration($(uint64_t val))} |]

getDownloadQueueTime :: PeerInfo -> IO Word64
getDownloadQueueTime ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| uint64_t { $(peer_info * hoPtr)->download_queue_time.diff } |]

setDownloadQueueTime :: PeerInfo -> Word64 -> IO ()
setDownloadQueueTime ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(peer_info * hoPtr)->download_queue_time = time_duration($(uint64_t val))} |]

getPeerInfoFlags :: PeerInfo -> IO (BitFlags PeerFlags)
getPeerInfoFlags ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int32_t{ $(peer_info * hoPtr)->flags } |]

setPeerInfoFlags :: PeerInfo -> BitFlags PeerFlags -> IO ()
setPeerInfoFlags ho flags = do
  let val = fromIntegral $ fromEnum flags
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->flags = $(int32_t val)} |]

getPeerInfoSource :: PeerInfo -> IO (BitFlags PeerSourceFlags)
getPeerInfoSource ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int32_t { $(peer_info * hoPtr)->source } |]

setPeerInfoSource :: PeerInfo -> BitFlags PeerSourceFlags -> IO ()
setPeerInfoSource ho flags = do
  let val = fromIntegral $ fromEnum flags
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->source = $(int32_t val)} |]

getUpSpeed :: PeerInfo -> IO CInt
getUpSpeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->up_speed } |]

setUpSpeed :: PeerInfo -> CInt -> IO ()
setUpSpeed ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->up_speed = $(int val)} |]


getDownSpeed :: PeerInfo -> IO CInt
getDownSpeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->down_speed } |]

setDownSpeed :: PeerInfo -> CInt -> IO ()
setDownSpeed ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->down_speed = $(int val)} |]


getPayloadUpSpeed :: PeerInfo -> IO CInt
getPayloadUpSpeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->payload_up_speed } |]

setPayloadUpSpeed :: PeerInfo -> CInt -> IO ()
setPayloadUpSpeed ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->payload_up_speed = $(int val)} |]


getPayloadDownSpeed :: PeerInfo -> IO CInt
getPayloadDownSpeed ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->payload_down_speed } |]

setPayloadDownSpeed :: PeerInfo -> CInt -> IO ()
setPayloadDownSpeed ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->payload_down_speed = $(int val)} |]

getPid :: PeerInfo -> IO Sha1Hash
getPid ho =
  withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(peer_info * hoPtr)->pid) } |]

setPid :: PeerInfo -> Sha1Hash -> IO ()
setPid ho val =
  withPtr ho $ \hoPtr ->
  withPtr val $ \valPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->pid = sha1_hash(*$(sha1_hash * valPtr)) } |]

getQueueBytes :: PeerInfo -> IO CInt
getQueueBytes ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(peer_info * hoPtr)->queue_bytes } |]

setQueueBytes :: PeerInfo -> CInt -> IO ()
setQueueBytes ho val =
  withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(peer_info * hoPtr)->queue_bytes = $(int val)} |]

getPeerInfoRequestTimeout :: PeerInfo -> IO CInt
getPeerInfoRequestTimeout ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->request_timeout } |]

setPeerInfoRequestTimeout :: PeerInfo -> CInt -> IO ()
setPeerInfoRequestTimeout ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->request_timeout = $(int val)} |]

getSendBufferSize :: PeerInfo -> IO CInt
getSendBufferSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->send_buffer_size } |]

setSendBufferSize :: PeerInfo -> CInt -> IO ()
setSendBufferSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->send_buffer_size = $(int val)} |]

getUsedSendBuffer :: PeerInfo -> IO CInt
getUsedSendBuffer ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->used_send_buffer } |]

setUsedSendBuffer :: PeerInfo -> CInt -> IO ()
setUsedSendBuffer ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->used_send_buffer = $(int val)} |]

getReceiveBufferSize :: PeerInfo -> IO CInt
getReceiveBufferSize ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->receive_buffer_size } |]

setReceiveBufferSize :: PeerInfo -> CInt -> IO ()
setReceiveBufferSize ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->receive_buffer_size = $(int val)} |]

getUsedReceiveBuffer :: PeerInfo -> IO CInt
getUsedReceiveBuffer ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->used_receive_buffer } |]

setUsedReceiveBuffer :: PeerInfo -> CInt -> IO ()
setUsedReceiveBuffer ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->used_receive_buffer = $(int val)} |]

getNumHashfails :: PeerInfo -> IO CInt
getNumHashfails ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->num_hashfails } |]

setNumHashfails :: PeerInfo -> CInt -> IO ()
setNumHashfails ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->num_hashfails = $(int val)} |]

getDownloadQueueLength :: PeerInfo -> IO CInt
getDownloadQueueLength ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->download_queue_length } |]

setDownloadQueueLength :: PeerInfo -> CInt -> IO ()
setDownloadQueueLength ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->download_queue_length = $(int val)} |]

getTimedOutRequests :: PeerInfo -> IO CInt
getTimedOutRequests ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->timed_out_requests } |]

setTimedOutRequests :: PeerInfo -> CInt -> IO ()
setTimedOutRequests ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->timed_out_requests = $(int val)} |]

getBusyRequests :: PeerInfo -> IO CInt
getBusyRequests ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->busy_requests } |]

setBusyRequests :: PeerInfo -> CInt -> IO ()
setBusyRequests ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->busy_requests = $(int val)} |]

getRequestsInBuffer :: PeerInfo -> IO CInt
getRequestsInBuffer ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->requests_in_buffer } |]

setRequestsInBuffer :: PeerInfo -> CInt -> IO ()
setRequestsInBuffer ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->requests_in_buffer = $(int val)} |]

getTargetDlQueueLength :: PeerInfo -> IO CInt
getTargetDlQueueLength ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->target_dl_queue_length } |]

setTargetDlQueueLength :: PeerInfo -> CInt -> IO ()
setTargetDlQueueLength ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->target_dl_queue_length = $(int val)} |]

getUploadQueueLength :: PeerInfo -> IO CInt
getUploadQueueLength ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->upload_queue_length } |]

setUploadQueueLength :: PeerInfo -> CInt -> IO ()
setUploadQueueLength ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->upload_queue_length = $(int val)} |]

getFailcount :: PeerInfo -> IO CInt
getFailcount ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->failcount } |]

setFailcount :: PeerInfo -> CInt -> IO ()
setFailcount ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->failcount = $(int val)} |]

getDownloadingPieceIndex :: PeerInfo -> IO CInt
getDownloadingPieceIndex ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_piece_index } |]

setDownloadingPieceIndex :: PeerInfo -> CInt -> IO ()
setDownloadingPieceIndex ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_piece_index = $(int val)} |]

getDownloadingBlockIndex :: PeerInfo -> IO CInt
getDownloadingBlockIndex ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_block_index } |]

setDownloadingBlockIndex :: PeerInfo -> CInt -> IO ()
setDownloadingBlockIndex ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_block_index = $(int val)} |]

getDownloadingProgress :: PeerInfo -> IO CInt
getDownloadingProgress ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_progress } |]

setDownloadingProgress :: PeerInfo -> CInt -> IO ()
setDownloadingProgress ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_progress = $(int val)} |]

getDownloadingTotal :: PeerInfo -> IO CInt
getDownloadingTotal ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->downloading_total } |]

setDownloadingTotal :: PeerInfo -> CInt -> IO ()
setDownloadingTotal ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->downloading_total = $(int val)} |]

getConnectionType :: PeerInfo -> IO ConnectionType
getConnectionType ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(peer_info * hoPtr)->connection_type } |]

setConnectionType :: PeerInfo -> ConnectionType -> IO ()
setConnectionType ho flag = do
  let val = fromIntegral $ fromEnum flag
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->connection_type = $(int val)} |]

getRemoteDlRate :: PeerInfo -> IO CInt
getRemoteDlRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->remote_dl_rate } |]

setRemoteDlRate :: PeerInfo -> CInt -> IO ()
setRemoteDlRate ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->remote_dl_rate = $(int val)} |]

getPendingDiskBytes :: PeerInfo -> IO CInt
getPendingDiskBytes ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->pending_disk_bytes } |]

setPendingDiskBytes :: PeerInfo -> CInt -> IO ()
setPendingDiskBytes ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->pending_disk_bytes = $(int val)} |]

-- getPendingDiskReadBytes :: PeerInfo -> IO CInt
-- getPendingDiskReadBytes ho =
--   withPtr ho $ \hoPtr ->
--   [CU.exp| int { $(peer_info * hoPtr)->pending_disk_read_bytes } |]

-- setPendingDiskReadBytes :: PeerInfo -> CInt -> IO ()
-- setPendingDiskReadBytes ho val =
--   withPtr ho $ \hoPtr ->
--   [CU.exp| void { $(peer_info * hoPtr)->pending_disk_read_bytes = $(int val)} |]

getSendQuota :: PeerInfo -> IO CInt
getSendQuota ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->send_quota } |]

setSendQuota :: PeerInfo -> CInt -> IO ()
setSendQuota ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->send_quota = $(int val)} |]

getReceiveQuota :: PeerInfo -> IO CInt
getReceiveQuota ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->receive_quota } |]

setReceiveQuota :: PeerInfo -> CInt -> IO ()
setReceiveQuota ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->receive_quota = $(int val)} |]

getRtt :: PeerInfo -> IO CInt
getRtt ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->rtt } |]

setRtt :: PeerInfo -> CInt -> IO ()
setRtt ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->rtt = $(int val)} |]

getPeerInfoNumPieces :: PeerInfo -> IO CInt
getPeerInfoNumPieces ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->num_pieces } |]

setPeerInfoNumPieces :: PeerInfo -> CInt -> IO ()
setPeerInfoNumPieces ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->num_pieces = $(int val)} |]

getDownloadRatePeak :: PeerInfo -> IO CInt
getDownloadRatePeak ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->download_rate_peak } |]

setDownloadRatePeak :: PeerInfo -> CInt -> IO ()
setDownloadRatePeak ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->download_rate_peak = $(int val)} |]

getUploadRatePeak :: PeerInfo -> IO CInt
getUploadRatePeak ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->upload_rate_peak } |]

setUploadRatePeak :: PeerInfo -> CInt -> IO ()
setUploadRatePeak ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->upload_rate_peak = $(int val)} |]

getPeerInfoProgressPpm :: PeerInfo -> IO CInt
getPeerInfoProgressPpm ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->progress_ppm } |]

setPeerInfoProgressPpm :: PeerInfo -> CInt -> IO ()
setPeerInfoProgressPpm ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->progress_ppm = $(int val)} |]

getEstimatedReciprocationRate :: PeerInfo -> IO CInt
getEstimatedReciprocationRate ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(peer_info * hoPtr)->estimated_reciprocation_rate } |]

setEstimatedReciprocationRate :: PeerInfo -> CInt -> IO ()
setEstimatedReciprocationRate ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(peer_info * hoPtr)->estimated_reciprocation_rate = $(int val)} |]

getIp :: PeerInfo -> IO (Text, C.CShort)
getIp ho =
  withPtr ho $ \hoPtr -> do
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


getLocalEndpoint :: PeerInfo -> IO (Text, C.CShort)
getLocalEndpoint ho =
  withPtr ho $ \hoPtr -> do
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

getReadState :: PeerInfo -> IO (BitFlags BwState)
getReadState ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| char { $(peer_info * hoPtr)->read_state } |]

setReadState :: PeerInfo -> BitFlags BwState -> IO ()
setReadState ho flags = do
  let val = fromIntegral $ fromEnum flags
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->read_state = $(char val)} |]

getWriteState :: PeerInfo -> IO (BitFlags BwState)
getWriteState ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| char { $(peer_info * hoPtr)->write_state } |]

setWriteState :: PeerInfo -> BitFlags BwState -> IO ()
setWriteState ho flags = do
  let val = fromIntegral $ fromEnum flags
  withPtr ho $ \hoPtr ->
    [CU.exp| void { $(peer_info * hoPtr)->write_state = $(char val)} |]

