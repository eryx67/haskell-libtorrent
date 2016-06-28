{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | 

module Libtorrent.Session (Session
                          , DhtItemKey(..)
                          , newDhtItemKey
                          , SaveStateFlags(..)
                          , ListenOnFlags(..)
                          , RemoveTorrentOptions(..)
                          , SessionFlags(..)
                          , ProtocolType(..)
                          , newSession
                          , addTorrent
                          , asyncAddTorrent
                          , popAlerts
                          , unSession
                          , postTorrentUpdates
                          , findTorrent
                          , getTorrents
                          , sessionResume
                          , sessionPause
                          , isPaused
                          , isDhtRunning
                          , startDht
                          , stopDht
                          , addDhtRouter
                          , addDhtNode
                          , dhtGetItem
                          , dhtGetItem'
                          , loadCountryDb
                          , loadAsnumDb
                          , sessionId
                          , setPeerId
                          , setKey
                          , listenOn
                          , isListening
                          , listenPort
                          , sslListenPort
                          , removeTorrent
                          , setAlertMask
                          -- , setAlertDispatch
                          , stopLsd
                          , startLsd
                          , stopUpnp
                          , startUpnp
                          , deletePortMapping
                          , stopNatpmp
                          , startNatpmp
                          , loadState
                          , saveState
                          , refreshTorrentStatus
                          , getTorrentStatus
                          , sessionStatus
                          , addFeed
                          , removeFeed
                          , getFeeds
                          , getDhtSettings
                          , setDhtSettings
                          , getPeSettings
                          , setPeSettings
                          , getSessionSettings
                          , setSessionSettings
                          , dhtPutItem
                          , addExtension
                          , addTorrentExtension
                          -- , setAlertDispatch
                          ) where


import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.C.String (withCString)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import           Foreign.Ptr ( Ptr, FunPtr, freeHaskellFunPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Alert (newAlertDeque, Alert, AlertCategory)
import           Libtorrent.Bencode
import           Libtorrent.ErrorCode
import           Libtorrent.Exceptions
import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Rss
import           Libtorrent.Session.AddTorrentParams
import           Libtorrent.Session.DhtSettings (DhtSettings)
import           Libtorrent.Session.PeSettings (PeSettings)
import           Libtorrent.Session.SessionSettings (SessionSettings)
import           Libtorrent.Session.SessionStatus (SessionStatus)
import           Libtorrent.Sha1Hash (Sha1Hash)
import           Libtorrent.String
import           Libtorrent.TorrentHandle
import           Libtorrent.TorrentHandle.TorrentStatus
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/session.hpp>"
C.include "<libtorrent/add_torrent_params.hpp>"
C.include "<libtorrent/bencode.hpp>"
C.include "<libtorrent/extensions.hpp>"

C.include "session.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

C.verbatim "typedef std::vector<libtorrent::torrent_status> VectorTorrentStatus;"
C.verbatim "typedef std::vector<libtorrent::torrent_handle> VectorTorrentHandle;"
C.verbatim "typedef std::vector<libtorrent::feed_handle> VectorFeedHandle;"

newtype DhtItemKey = DhtItemKey ByteString

-- | Create 'DhtItemKey' from 'ByteString'. Bytestring must be 32 byte length.
newDhtItemKey :: ByteString -> Maybe DhtItemKey
newDhtItemKey bs
  | BS.length bs /= 32 = Nothing
  | otherwise = Just $ DhtItemKey bs

data SaveStateFlags =
  SaveSettings
  | SaveDhtSettings
  | SaveDhtState
  | SaveProxy
  | SaveI2pProxy
  | SaveEncryptionSettings
  | SaveFeeds
  deriving (Show, Enum, Bounded)

data ListenOnFlags =
  ListenNoSystemPort
  deriving (Show, Enum, Bounded)

data RemoveTorrentOptions =
  DeleteFiles
  deriving (Show, Enum, Bounded)

data SessionFlags =
  AddDefaultPlugins
  | StartDefaultFeatures
  deriving (Show, Enum, Bounded)

data ProtocolType =
  Udp
  | Tcp
  deriving (Show, Enum, Bounded)

newtype Session = Session { unSession :: ForeignPtr (CType Session)}

instance Show Session where
  show _ = "Session"

instance Inlinable Session where
  type (CType Session) = C'Session

instance FromPtr Session where
  fromPtr = objFromPtr Session $ \ptr ->
    [C.block| void {
        session_proxy spx = $(session * ptr)->abort();
        delete $(session * ptr);
        spx.~session_proxy();
      }
    |]

instance WithPtr Session where
  withPtr (Session fptr) = withForeignPtr fptr

-- | Create new session.
newSession :: IO Session
newSession =
  fromPtr [C.exp| session * { new session() } |]

-- | Add torrent to session. It throws 'LibtorrentException'.
-- all 'TorrentHandle's must be destructed before the 'Session' is destructed
addTorrent :: Session -> AddTorrentParams -> IO TorrentHandle
addTorrent ses atp =
  withErrorCode AddTorrentError $ \ecPtr ->
  withPtr ses $ \sesPtr ->
  withPtr atp $ \atPtr -> do
    fromPtr [CU.block| torrent_handle * {
                error_code *ec = $(error_code * ecPtr);
                torrent_handle th = $(session * sesPtr)->add_torrent(*$(add_torrent_params * atPtr), *ec);
                return new torrent_handle(th);
               }
            |]

-- | Notification of the torrent being added is sent as 'add_torrent_alert'.
asyncAddTorrent :: Session -> AddTorrentParams -> IO ()
asyncAddTorrent ses atp =
  withPtr ses $ \sesPtr ->
  withPtr atp $ \atPtr -> do
    [CU.exp| void { $(session * sesPtr)->add_torrent(*$(add_torrent_params * atPtr)) } |]

-- | Load state from bencoded bytestring. Can throw 'LibtorrentException'.
loadState :: Session -> ByteString -> IO ()
loadState ho state =
  withPtr ho $ \hoPtr ->
  withErrorCode BDecodeError $ \ecPtr ->
  [C.block| void {
      lazy_entry e;
      lazy_bdecode($bs-ptr:state, $bs-ptr:state + $bs-len:state, e, *$(error_code * ecPtr));
      if ($(error_code * ecPtr)->value() == 0)
        $(session * hoPtr)->load_state(e);
    }
  |]

-- | Save state to bencoded bytestring.
saveState :: Session -> BitFlags SaveStateFlags -> IO Bencoded
saveState ho flags =
  withPtr ho $ \hoPtr ->
  bracket
  [CU.exp| entry * { new entry() }|]
  (\ePtr -> [CU.exp| void { delete $(entry * ePtr)} |]) $
  \ePtr -> do
    let flags' = fromIntegral $ fromEnum flags
    [C.exp| void { $(session * hoPtr)->save_state(*$(entry *ePtr), $(uint32_t flags')) } |]
    entryToBencoded ePtr

refreshTorrentStatus :: Session -> StdVector TorrentStatus -> Maybe (BitFlags StatusFlags)
                        -> IO (StdVector TorrentStatus)
refreshTorrentStatus ho vts flags =
  withPtr ho $ \hoPtr -> do
    let flags' = maybe 0 (fromIntegral . fromEnum) flags
    withPtr vts $ \vtsPtr ->
      [CU.exp| void { $(session * hoPtr)->refresh_torrent_status($(VectorTorrentStatus * vtsPtr), $(uint32_t flags')) } |]
    return vts

getTorrentStatus :: Session -> (TorrentStatus -> Bool) -> Maybe (BitFlags StatusFlags)
                    -> IO (StdVector TorrentStatus)
getTorrentStatus ho tsFilter flags =
  withPtr ho $ \hoPtr -> do
  let tsFilter' = \ts -> objFromPtr_ TorrentStatus (pure ts) >>= (return . tsFilter)
  let flags' = maybe 0 (fromIntegral . fromEnum) flags
  bracket
    ($(C.mkFunPtr [t| C'TorrentStatusFilter |]) tsFilter')
    freeHaskellFunPtr $
    \tsFilterPtr ->
    fromPtr  [CU.block| VectorTorrentStatus * {
                 std::vector<torrent_status> *res = new std::vector<torrent_status>();
                 boost::function<bool(torrent_status const &)> pred;
                 pred = [=](torrent_status ts) { return $(TorrentStatusFilter tsFilterPtr)(&ts); };
                 $(session * hoPtr)->get_torrent_status(res, pred, $(uint32_t flags'));
                 return res;
               }
             |]

postTorrentUpdates :: Session -> IO ()
postTorrentUpdates ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->post_torrent_updates() } |]

findTorrent :: Session -> Sha1Hash -> IO TorrentHandle
findTorrent ho info_hash =
  withPtr ho $ \hoPtr ->
  withPtr info_hash $ \ihPtr ->
  fromPtr [CU.exp| torrent_handle * { new torrent_handle($(session * hoPtr)->find_torrent(*$(sha1_hash * ihPtr))) } |]

getTorrents :: Session -> IO (StdVector TorrentHandle)
getTorrents ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| VectorTorrentHandle * { new std::vector<torrent_handle>($(session * hoPtr)->get_torrents()) } |]

sessionResume :: Session -> IO ()
sessionResume ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->resume() } |]

sessionPause :: Session -> IO ()
sessionPause ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->pause() } |]

isPaused :: Session -> IO Bool
isPaused ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_paused() } |]

sessionStatus :: Session -> IO SessionStatus
sessionStatus ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| session_status * { new session_status($(session * hoPtr)->status()) } |]

-- TODO:
-- cache_status get_cache_status () const;
-- void get_cache_info (sha1_hash const& ih
--    , std::vector<cached_piece_info>& ret) const;

addFeed :: Session -> FeedSettings -> IO FeedHandle
addFeed ho fs =
  withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  fromPtr [CU.exp| feed_handle * { new feed_handle($(session * hoPtr)->add_feed(*$(feed_settings * fsPtr))) } |]

removeFeed :: Session -> FeedHandle -> IO ()
removeFeed ho fh =
  withPtr ho $ \hoPtr ->
  withPtr fh $ \fhPtr ->
  [CU.exp| void { $(session * hoPtr)->remove_feed(*$(feed_handle * fhPtr)) } |]

getFeeds :: Session -> IO (StdVector FeedHandle)
getFeeds ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.block| VectorFeedHandle * {
              VectorFeedHandle *res = new std::vector<feed_handle>();
              $(session * hoPtr)->get_feeds(*res);
              return res;
             }
          |]


isDhtRunning :: Session -> IO Bool
isDhtRunning ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_dht_running() } |]

getDhtSettings :: Session -> IO DhtSettings
getDhtSettings ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| dht_settings * { new dht_settings($(session * hoPtr)->get_dht_settings()) } |]

setDhtSettings :: Session -> DhtSettings -> IO ()
setDhtSettings ho ds =
  withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [CU.exp| void { $(session * hoPtr)->set_dht_settings(*$(dht_settings * dsPtr)) } |]

startDht :: Session -> IO ()
startDht ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->start_dht() } |]

stopDht :: Session -> IO ()
stopDht ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->stop_dht() } |]

addDhtRouter :: Session -> Text -> C.CInt -> IO ()
addDhtRouter ho host port =
  withPtr ho $ \hoPtr -> do
    hstr <- textToStdString host
    withPtr hstr $ \hstrPtr ->
      [CU.block| void {
          std::pair<string, int> addr = std::pair<string, int>(*$(string * hstrPtr), $(int port));
          $(session * hoPtr)->add_dht_router(addr);
         }
      |]

addDhtNode :: Session -> Text -> C.CInt -> IO ()
addDhtNode ho host port =
  withPtr ho $ \hoPtr -> do
    hstr <- textToStdString host
    withPtr hstr $ \hstrPtr ->
      [CU.block| void {
          std::pair<string, int> addr = std::pair<string, int>(*$(string * hstrPtr), $(int port));
          $(session * hoPtr)->add_dht_node(addr);
         }
      |]

dhtGetItem :: Session -> Sha1Hash -> IO ()
dhtGetItem ho ih =
  withPtr ho $ \hoPtr ->
  withPtr ih $ \ihPtr ->
  [CU.exp| void { $(session * hoPtr)->dht_get_item(*$(sha1_hash * ihPtr)) } |]

dhtGetItem' :: Session -> DhtItemKey -> Maybe ByteString -> IO ()
dhtGetItem' ho (DhtItemKey key) salt =
  withPtr ho $ \hoPtr -> do
    let salt' = fromMaybe BS.empty salt
    [CU.block| void {
        boost::array<char, 32> key;
        for (int i = 0; i < 32; i++)
          key[i] = $bs-ptr:key[i];
        $(session * hoPtr)->dht_get_item(key, std::string($bs-ptr:salt', $bs-len:salt'));
       }
    |]

-- | Put dht item from bencoded data.
dhtPutItem :: Session -> ByteString -> IO Sha1Hash
dhtPutItem ho bencData =
  withPtr ho $ \hoPtr ->
  fromPtr [C.block| sha1_hash * {
              entry e = bdecode($bs-ptr:bencData, $bs-ptr:bencData + $bs-len:bencData);
              return new sha1_hash($(session * hoPtr)->dht_put_item(e));
            }
          |]

-- TODO:
-- void dht_put_item (boost::array<char, 32> key
--    , boost::function<void(entry&, boost::array<char,64>&
--    , boost::uint64_t&, std::string const&)> cb
--    , std::string salt = std::string());

addExtension :: Session -> Ptr C'Plugin -> IO ()
addExtension ho plugin =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->add_extension(*$(Plugin * plugin)) } |]

addTorrentExtension :: Session -> Ptr C'TorrentPlugin -> IO ()
addTorrentExtension ho plugin =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->add_extension(*$(TorrentPlugin * plugin)) } |]

loadCountryDb :: Session -> Text -> IO ()
loadCountryDb ho file =
  withPtr ho $ \hoPtr -> do
  fstr <- textToStdString file
  withPtr fstr $ \filePtr ->
    [CU.exp| void { $(session * hoPtr)->load_country_db($(string * filePtr)->c_str()) } |]

loadAsnumDb :: Session -> Text -> IO ()
loadAsnumDb ho file =
  withPtr ho $ \hoPtr -> do
  fstr <- textToStdString file
  withPtr fstr $ \filePtr ->
    [CU.exp| void { $(session * hoPtr)->load_asnum_db($(string * filePtr)->c_str()) } |]

-- TODO:
-- int as_for_ip (address const& addr);
-- ip_filter get_ip_filter () const;
-- void set_ip_filter (ip_filter const& f);
-- void set_port_filter (port_filter const& f);

sessionId :: Session -> IO Sha1Hash
sessionId ho =
  withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(session * hoPtr)->id()) } |]

setPeerId :: Session -> Sha1Hash -> IO ()
setPeerId ho ih =
  withPtr ho $ \hoPtr ->
  withPtr ih $ \ihPtr ->
  [CU.exp| void { $(session * hoPtr)->set_peer_id(sha1_hash(*$(sha1_hash * ihPtr))) } |]

setKey :: Session -> C.CInt -> IO ()
setKey ho key =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->set_key($(int key)) } |]

listenOn :: Session -> (C.CInt, C.CInt) -> Maybe Text -> IO ErrorCode
listenOn ho (fromPort, toPort) iface =
  withPtr ho $ \hoPtr -> do
  ec <- newErrorCode
  withPtr ec $ \ecPtr ->
    case iface of
      Just iface' ->
        withCString (T.unpack iface') $ \ifacePtr ->
        [CU.exp| void { $(session * hoPtr)->listen_on( std::pair<int, int>($(int fromPort), $(int toPort)), *$(error_code * ecPtr), $(char * ifacePtr)) } |]
      Nothing ->
        [CU.exp| void { $(session * hoPtr)->listen_on( std::pair<int, int>($(int fromPort), $(int toPort)), *$(error_code * ecPtr)) } |]
  return ec

isListening :: Session -> IO Bool
isListening ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_listening() } |]

listenPort :: Session -> IO C.CUShort
listenPort ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| unsigned short { $(session * hoPtr)->listen_port() } |]

sslListenPort :: Session -> IO C.CUShort
sslListenPort ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| unsigned short { $(session * hoPtr)->ssl_listen_port() } |]

removeTorrent :: Session -> TorrentHandle -> Maybe (BitFlags RemoveTorrentOptions) -> IO ()
removeTorrent ho th opts =
  withPtr ho $ \hoPtr ->
  withPtr th $ \thPtr -> do
  let opts' = fromIntegral $ maybe 0 fromEnum opts
  [CU.exp| void { $(session * hoPtr)->remove_torrent(*$(torrent_handle * thPtr), $(int opts')) } |]

getSessionSettings :: Session -> IO SessionSettings
getSessionSettings ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| session_settings * { new session_settings($(session * hoPtr)->settings()) } |]

setSessionSettings :: Session -> SessionSettings -> IO ()
setSessionSettings ho ds =
  withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [CU.exp| void { $(session * hoPtr)->set_settings(*$(session_settings * dsPtr)) } |]

getPeSettings :: Session -> IO PeSettings
getPeSettings ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.exp| pe_settings * { new pe_settings($(session * hoPtr)->get_pe_settings()) } |]

setPeSettings :: Session -> PeSettings -> IO ()
setPeSettings ho ds =
  withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [CU.exp| void { $(session * hoPtr)->set_pe_settings(*$(pe_settings * dsPtr)) } |]

-- TODO:
-- void set_proxy (proxy_settings const& s);
-- proxy_settings proxy () const;
-- proxy_settings i2p_proxy () const;
-- void set_i2p_proxy (proxy_settings const& s);

popAlerts :: Session -> IO (StdDeque Alert)
popAlerts ho =
  withPtr ho $ \hoPtr -> do
  res <- newAlertDeque
  withPtr res $ \resPtr ->
    [C.exp| void { $(session * hoPtr)->pop_alerts($(DequeAlertPtr * resPtr)) } |]
  return res

-- alert const* wait_for_alert (time_duration max_wait);

setAlertMask :: Session -> BitFlags AlertCategory -> IO ()
setAlertMask ho flags =
  withPtr ho $ \hoPtr -> do
  let val = fromIntegral $ fromEnum flags
  [CU.exp| void { $(session * hoPtr)->set_alert_mask($(uint32_t val)) } |]

-- FIXME: it causes a deadlock in C code but it'll be removed in next release of libtorrent.
-- -- | Set alerts handler for 'Session'. Returns finalizer for allocated 'FunPtr'.
-- setAlertDispatch :: Session -> (Alert -> IO ()) -> IO (FunPtr C'AlertDispatchCallback)
-- setAlertDispatch ses cb = do
--   let c'cb = \aPtr -> (fromPtr $ pure aPtr) >>= cb
--   c'setAlertDispatch ses c'cb

-- c'setAlertDispatch :: Session -> C'AlertDispatchCallback -> IO (FunPtr C'AlertDispatchCallback)
-- c'setAlertDispatch ho cb =
--   withPtr ho $ \hoPtr -> do
--     cbPtr <- $(C.mkFunPtr [t| C'AlertDispatchCallback |]) cb
--     [C.block| void {
--         $(session * hoPtr)->set_alert_dispatch([=](auto_ptr<alert> aptr) { $(AlertDispatchCallback cbPtr)(aptr.release());});
--        }
--     |]
--     return cbPtr

stopLsd :: Session -> IO ()
stopLsd ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->stop_lsd() } |]

startLsd :: Session -> IO ()
startLsd ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->start_lsd() } |]

stopUpnp :: Session -> IO ()
stopUpnp ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->stop_upnp() } |]

startUpnp :: Session -> IO ()
startUpnp ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->start_upnp() } |]

deletePortMapping :: Session -> C.CInt -> IO ()
deletePortMapping ho handle =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->delete_port_mapping($(int handle)) } |]

-- TODO:
-- int add_port_mapping (protocol_type t, int external_port, int local_port);

stopNatpmp :: Session -> IO ()
stopNatpmp ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->stop_natpmp() } |]

startNatpmp :: Session -> IO ()
startNatpmp ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->start_natpmp() } |]

