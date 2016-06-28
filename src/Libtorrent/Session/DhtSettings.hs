{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#dht_settings dht_settings> for "Libtorrent"

module Libtorrent.Session.DhtSettings (DhtSettings
                                        , unDhtSettings
                                        , newDhtSettings
                                        , getMaxPeersReply
                                        , setMaxPeersReply
                                        , getSearchBranching
                                        , setSearchBranching
                                        , getMaxFailCount
                                        , setMaxFailCount
                                        , getMaxTorrents
                                        , setMaxTorrents
                                        , getMaxDhtItems
                                        , setMaxDhtItems
                                        , getMaxTorrentSearchReply
                                        , setMaxTorrentSearchReply
                                        , getRestrictRoutingIps
                                        , setRestrictRoutingIps
                                        , getRestrictSearchIps
                                        , setRestrictSearchIps
                                        , getExtendedRoutingTable
                                        , setExtendedRoutingTable
                                        , getAggressiveLookups
                                        , setAggressiveLookups
                                        , getPrivacyLookups
                                        , setPrivacyLookups
                                        , getEnforceNodeId
                                        , setEnforceNodeId
                                        , getIgnoreDarkBoolernet
                                        , setIgnoreDarkBoolernet
                                        ) where

import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

newtype DhtSettings = DhtSettings { unDhtSettings :: ForeignPtr (CType DhtSettings)}

instance Show DhtSettings where
  show _ = "DhtSettings"

instance Inlinable DhtSettings where
  type (CType DhtSettings) = C'DhtSettings

instance FromPtr DhtSettings where
  fromPtr = objFromPtr DhtSettings $ \ptr ->
    [CU.exp| void { delete $(dht_settings * ptr); } |]

instance WithPtr DhtSettings where
  withPtr (DhtSettings fptr) = withForeignPtr fptr

newDhtSettings :: IO DhtSettings
newDhtSettings =
  fromPtr [CU.exp| dht_settings * { new dht_settings() }|]

getMaxPeersReply :: DhtSettings -> IO CInt
getMaxPeersReply ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_peers_reply } |]

setMaxPeersReply :: DhtSettings -> CInt -> IO ()
setMaxPeersReply ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_peers_reply = $(int val)} |]

getSearchBranching :: DhtSettings -> IO CInt
getSearchBranching ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->search_branching } |]

setSearchBranching :: DhtSettings -> CInt -> IO ()
setSearchBranching ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->search_branching = $(int val)} |]

getMaxFailCount :: DhtSettings -> IO CInt
getMaxFailCount ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_fail_count } |]

setMaxFailCount :: DhtSettings -> CInt -> IO ()
setMaxFailCount ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_fail_count = $(int val)} |]

getMaxTorrents :: DhtSettings -> IO CInt
getMaxTorrents ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_torrents } |]

setMaxTorrents :: DhtSettings -> CInt -> IO ()
setMaxTorrents ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_torrents = $(int val)} |]

getMaxDhtItems :: DhtSettings -> IO CInt
getMaxDhtItems ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_dht_items } |]

setMaxDhtItems :: DhtSettings -> CInt -> IO ()
setMaxDhtItems ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_dht_items = $(int val)} |]

getMaxTorrentSearchReply :: DhtSettings -> IO CInt
getMaxTorrentSearchReply ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_torrent_search_reply } |]

setMaxTorrentSearchReply :: DhtSettings -> CInt -> IO ()
setMaxTorrentSearchReply ho val =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_torrent_search_reply = $(int val)} |]

getRestrictRoutingIps :: DhtSettings -> IO Bool
getRestrictRoutingIps ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->restrict_routing_ips } |]

setRestrictRoutingIps :: DhtSettings -> Bool -> IO ()
setRestrictRoutingIps ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->restrict_routing_ips = $(bool val')} |]

getRestrictSearchIps :: DhtSettings -> IO Bool
getRestrictSearchIps ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->restrict_search_ips } |]

setRestrictSearchIps :: DhtSettings -> Bool -> IO ()
setRestrictSearchIps ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->restrict_search_ips = $(bool val')} |]

getExtendedRoutingTable :: DhtSettings -> IO Bool
getExtendedRoutingTable ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->extended_routing_table } |]

setExtendedRoutingTable :: DhtSettings -> Bool -> IO ()
setExtendedRoutingTable ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->extended_routing_table = $(bool val')} |]

getAggressiveLookups :: DhtSettings -> IO Bool
getAggressiveLookups ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->aggressive_lookups } |]

setAggressiveLookups :: DhtSettings -> Bool -> IO ()
setAggressiveLookups ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->aggressive_lookups = $(bool val')} |]

getPrivacyLookups :: DhtSettings -> IO Bool
getPrivacyLookups ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->privacy_lookups } |]

setPrivacyLookups :: DhtSettings -> Bool -> IO ()
setPrivacyLookups ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->privacy_lookups = $(bool val')} |]

getEnforceNodeId :: DhtSettings -> IO Bool
getEnforceNodeId ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->enforce_node_id } |]

setEnforceNodeId :: DhtSettings -> Bool -> IO ()
setEnforceNodeId ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->enforce_node_id = $(bool val')} |]

getIgnoreDarkBoolernet :: DhtSettings -> IO Bool
getIgnoreDarkBoolernet ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->ignore_dark_internet } |]

setIgnoreDarkBoolernet :: DhtSettings -> Bool -> IO ()
setIgnoreDarkBoolernet ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->ignore_dark_internet = $(bool val')} |]
