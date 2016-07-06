{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#dht_settings dht_settings> for "Libtorrent"

module Network.Libtorrent.Session.DhtSettings (DhtSettings
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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types

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

newDhtSettings :: MonadIO m =>  m DhtSettings
newDhtSettings =
  liftIO $ fromPtr [CU.exp| dht_settings * { new dht_settings() }|]

getMaxPeersReply :: MonadIO m =>  DhtSettings -> m CInt
getMaxPeersReply ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_peers_reply } |]

setMaxPeersReply :: MonadIO m =>  DhtSettings -> CInt -> m ()
setMaxPeersReply ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_peers_reply = $(int val)} |]

getSearchBranching :: MonadIO m =>  DhtSettings -> m CInt
getSearchBranching ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->search_branching } |]

setSearchBranching :: MonadIO m =>  DhtSettings -> CInt -> m ()
setSearchBranching ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->search_branching = $(int val)} |]

getMaxFailCount :: MonadIO m =>  DhtSettings -> m CInt
getMaxFailCount ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_fail_count } |]

setMaxFailCount :: MonadIO m =>  DhtSettings -> CInt -> m ()
setMaxFailCount ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_fail_count = $(int val)} |]

getMaxTorrents :: MonadIO m =>  DhtSettings -> m CInt
getMaxTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_torrents } |]

setMaxTorrents :: MonadIO m =>  DhtSettings -> CInt -> m ()
setMaxTorrents ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_torrents = $(int val)} |]

getMaxDhtItems :: MonadIO m =>  DhtSettings -> m CInt
getMaxDhtItems ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_dht_items } |]

setMaxDhtItems :: MonadIO m =>  DhtSettings -> CInt -> m ()
setMaxDhtItems ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_dht_items = $(int val)} |]

getMaxTorrentSearchReply :: MonadIO m =>  DhtSettings -> m CInt
getMaxTorrentSearchReply ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(dht_settings * hoPtr)->max_torrent_search_reply } |]

setMaxTorrentSearchReply :: MonadIO m =>  DhtSettings -> CInt -> m ()
setMaxTorrentSearchReply ho val =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(dht_settings * hoPtr)->max_torrent_search_reply = $(int val)} |]

getRestrictRoutingIps :: MonadIO m =>  DhtSettings -> m Bool
getRestrictRoutingIps ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->restrict_routing_ips } |]

setRestrictRoutingIps :: MonadIO m =>  DhtSettings -> Bool -> m ()
setRestrictRoutingIps ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->restrict_routing_ips = $(bool val')} |]

getRestrictSearchIps :: MonadIO m =>  DhtSettings -> m Bool
getRestrictSearchIps ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->restrict_search_ips } |]

setRestrictSearchIps :: MonadIO m =>  DhtSettings -> Bool -> m ()
setRestrictSearchIps ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->restrict_search_ips = $(bool val')} |]

getExtendedRoutingTable :: MonadIO m =>  DhtSettings -> m Bool
getExtendedRoutingTable ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->extended_routing_table } |]

setExtendedRoutingTable :: MonadIO m =>  DhtSettings -> Bool -> m ()
setExtendedRoutingTable ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->extended_routing_table = $(bool val')} |]

getAggressiveLookups :: MonadIO m =>  DhtSettings -> m Bool
getAggressiveLookups ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->aggressive_lookups } |]

setAggressiveLookups :: MonadIO m =>  DhtSettings -> Bool -> m ()
setAggressiveLookups ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->aggressive_lookups = $(bool val')} |]

getPrivacyLookups :: MonadIO m =>  DhtSettings -> m Bool
getPrivacyLookups ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->privacy_lookups } |]

setPrivacyLookups :: MonadIO m =>  DhtSettings -> Bool -> m ()
setPrivacyLookups ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->privacy_lookups = $(bool val')} |]

getEnforceNodeId :: MonadIO m =>  DhtSettings -> m Bool
getEnforceNodeId ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->enforce_node_id } |]

setEnforceNodeId :: MonadIO m =>  DhtSettings -> Bool -> m ()
setEnforceNodeId ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->enforce_node_id = $(bool val')} |]

getIgnoreDarkBoolernet :: MonadIO m =>  DhtSettings -> m Bool
getIgnoreDarkBoolernet ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(dht_settings * hoPtr)->ignore_dark_internet } |]

setIgnoreDarkBoolernet :: MonadIO m =>  DhtSettings -> Bool -> m ()
setIgnoreDarkBoolernet ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(dht_settings * hoPtr)->ignore_dark_internet = $(bool val')} |]
