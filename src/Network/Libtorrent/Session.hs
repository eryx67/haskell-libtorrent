{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | 

module Network.Libtorrent.Session (Session
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
                          , sessionHandleAlerts
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
                          , proxy
                          , setProxy
                          , i2pProxy
                          , setI2pProxy
                          ) where


import           Control.Monad (forM)
import           Control.Monad.Catch (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.C.String (withCString)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool)
import           Foreign.Ptr ( Ptr, FunPtr, freeHaskellFunPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Alert (Alert, AlertCategory, AlertHandler,
                                   newAlertDeque, handleAlerts)
import           Network.Libtorrent.Bencode
import           Network.Libtorrent.Exceptions
import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Rss
import           Network.Libtorrent.Session.AddTorrentParams
import           Network.Libtorrent.Session.DhtSettings (DhtSettings)
import           Network.Libtorrent.Session.PeSettings (PeSettings)
import           Network.Libtorrent.Session.ProxySettings (ProxySettings)
import           Network.Libtorrent.Session.SessionSettings (SessionSettings)
import           Network.Libtorrent.Session.SessionStatus (SessionStatus)
import           Network.Libtorrent.Sha1Hash (Sha1Hash)
import           Network.Libtorrent.String
import           Network.Libtorrent.TorrentHandle
import           Network.Libtorrent.TorrentHandle.TorrentStatus
import           Network.Libtorrent.Types
import           Network.Libtorrent.Types.ArrayLike (toList)

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
  deriving (Show, Enum, Bounded, Eq)

data ListenOnFlags =
  ListenNoSystemPort
  deriving (Show, Enum, Bounded, Eq)

data RemoveTorrentOptions =
  DeleteFiles
  deriving (Show, Enum, Bounded, Eq)

data SessionFlags =
  AddDefaultPlugins
  | StartDefaultFeatures
  deriving (Show, Enum, Bounded, Eq)

data ProtocolType =
  Udp
  | Tcp
  deriving (Show, Enum, Bounded, Eq)

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
newSession :: MonadIO m => m Session
newSession =
  liftIO $ fromPtr [C.exp| session * { new session() } |]

-- | Add torrent to session. It throws 'LibtorrentException'.
-- all 'TorrentHandle's must be destructed before the 'Session' is destructed
addTorrent :: MonadIO m => Session -> AddTorrentParams -> m TorrentHandle
addTorrent ses atp =
  liftIO . withErrorCode AddTorrentError $ \ecPtr ->
  withPtr ses $ \sesPtr ->
  withPtr atp $ \atPtr -> do
    fromPtr [C.block| torrent_handle * {
                error_code *ec = $(error_code * ecPtr);
                torrent_handle th = $(session * sesPtr)->add_torrent(*$(add_torrent_params * atPtr), *ec);
                return new torrent_handle(th);
               }
            |]

-- | Notification of the torrent being added is sent as 'add_torrent_alert'.
asyncAddTorrent :: MonadIO m => Session -> AddTorrentParams -> m ()
asyncAddTorrent ses atp =
  liftIO . withPtr ses $ \sesPtr ->
  withPtr atp $ \atPtr -> do
    [CU.exp| void { $(session * sesPtr)->async_add_torrent(*$(add_torrent_params * atPtr)) } |]

-- | Load state from bencoded bytestring. Can throw 'LibtorrentException'.
loadState :: MonadIO m => Session -> ByteString -> m ()
loadState ho state =
  liftIO . withPtr ho $ \hoPtr ->
  withErrorCode BDecodeError $ \ecPtr ->
  [C.block| void {
      lazy_entry e;
      lazy_bdecode($bs-ptr:state, $bs-ptr:state + $bs-len:state, e, *$(error_code * ecPtr));
      if ($(error_code * ecPtr)->value() == 0)
        $(session * hoPtr)->load_state(e);
    }
  |]

-- | Save state to bencoded bytestring.
saveState :: MonadIO m => Session -> BitFlags SaveStateFlags -> m Bencoded
saveState ho flags =
  liftIO . withPtr ho $ \hoPtr ->
  bracket
  [CU.exp| entry * { new entry() }|]
  (\ePtr -> [CU.exp| void { delete $(entry * ePtr)} |]) $
  \ePtr -> do
    let flags' = fromIntegral $ fromEnum flags
    [C.exp| void { $(session * hoPtr)->save_state(*$(entry *ePtr), $(uint32_t flags')) } |]
    entryToBencoded ePtr

-- | Helper fo handling session alerts with 'popAlert'.
-- Returns results of affected 'AlertHandler's.
sessionHandleAlerts :: MonadIO m => Session -> [AlertHandler a m] -> m [a]
sessionHandleAlerts ses alertHlrs = do
  alerts <-  popAlerts ses >>= liftIO . toList
  fmap catMaybes $ forM alerts (flip handleAlerts alertHlrs)

refreshTorrentStatus :: MonadIO m => Session -> StdVector TorrentStatus -> Maybe (BitFlags StatusFlags)
                        -> m (StdVector TorrentStatus)
refreshTorrentStatus ho vts flags =
  liftIO . withPtr ho $ \hoPtr -> do
    let flags' = maybe 0 (fromIntegral . fromEnum) flags
    withPtr vts $ \vtsPtr ->
      [C.exp| void { $(session * hoPtr)->refresh_torrent_status($(VectorTorrentStatus * vtsPtr), $(uint32_t flags')) } |]
    return vts

getTorrentStatus :: MonadIO m => Session -> (TorrentStatus -> Bool) -> Maybe (BitFlags StatusFlags)
                    -> m (StdVector TorrentStatus)
getTorrentStatus ho tsFilter flags =
  liftIO . withPtr ho $ \hoPtr -> do
  let tsFilter' = \ts -> objFromPtr_ TorrentStatus (pure ts) >>= (return . tsFilter)
  let flags' = maybe 0 (fromIntegral . fromEnum) flags
  bracket
    ($(C.mkFunPtr [t| C'TorrentStatusFilter |]) tsFilter')
    freeHaskellFunPtr $
    \tsFilterPtr ->
    fromPtr  [C.block| VectorTorrentStatus * {
                 std::vector<torrent_status> *res = new std::vector<torrent_status>();
                 boost::function<bool(torrent_status const &)> pred;
                 pred = [=](torrent_status ts) { return $(TorrentStatusFilter tsFilterPtr)(&ts); };
                 $(session * hoPtr)->get_torrent_status(res, pred, $(uint32_t flags'));
                 return res;
               }
             |]

postTorrentUpdates :: MonadIO m => Session -> m ()
postTorrentUpdates ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->post_torrent_updates() } |]

findTorrent :: MonadIO m => Session -> Sha1Hash -> m TorrentHandle
findTorrent ho info_hash =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr info_hash $ \ihPtr ->
  fromPtr [C.exp| torrent_handle * { new torrent_handle($(session * hoPtr)->find_torrent(*$(sha1_hash * ihPtr))) } |]

getTorrents :: MonadIO m => Session -> m (StdVector TorrentHandle)
getTorrents ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| VectorTorrentHandle * { new std::vector<torrent_handle>($(session * hoPtr)->get_torrents()) } |]

sessionResume :: MonadIO m => Session -> m ()
sessionResume ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->resume() } |]

sessionPause :: MonadIO m => Session -> m ()
sessionPause ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->pause() } |]

isPaused :: MonadIO m => Session -> m Bool
isPaused ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_paused() } |]

sessionStatus :: MonadIO m => Session -> m SessionStatus
sessionStatus ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| session_status * { new session_status($(session * hoPtr)->status()) } |]

-- TODO:
-- cache_status get_cache_status () const;
-- void get_cache_info (sha1_hash const& ih
--    , std::vector<cached_piece_info>& ret) const;

addFeed :: MonadIO m => Session -> FeedSettings -> m FeedHandle
addFeed ho fs =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr fs $ \fsPtr ->
  fromPtr [C.exp| feed_handle * { new feed_handle($(session * hoPtr)->add_feed(*$(feed_settings * fsPtr))) } |]

removeFeed :: MonadIO m => Session -> FeedHandle -> m ()
removeFeed ho fh =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr fh $ \fhPtr ->
  [C.exp| void { $(session * hoPtr)->remove_feed(*$(feed_handle * fhPtr)) } |]

getFeeds :: MonadIO m => Session -> m (StdVector FeedHandle)
getFeeds ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.block| VectorFeedHandle * {
              VectorFeedHandle *res = new std::vector<feed_handle>();
              $(session * hoPtr)->get_feeds(*res);
              return res;
             }
          |]


isDhtRunning :: MonadIO m => Session -> m Bool
isDhtRunning ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_dht_running() } |]

getDhtSettings :: MonadIO m => Session -> m DhtSettings
getDhtSettings ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| dht_settings * { new dht_settings($(session * hoPtr)->get_dht_settings()) } |]

setDhtSettings :: MonadIO m => Session -> DhtSettings -> m ()
setDhtSettings ho ds =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [C.exp| void { $(session * hoPtr)->set_dht_settings(*$(dht_settings * dsPtr)) } |]

startDht :: MonadIO m => Session -> m ()
startDht ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->start_dht() } |]

stopDht :: MonadIO m => Session -> m ()
stopDht ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->stop_dht() } |]

addDhtRouter :: MonadIO m => Session -> Text -> C.CInt -> m ()
addDhtRouter ho host port =
  liftIO . withPtr ho $ \hoPtr -> do
    hstr <- textToStdString host
    withPtr hstr $ \hstrPtr ->
      [C.block| void {
          std::pair<string, int> addr = std::pair<string, int>(*$(string * hstrPtr), $(int port));
          $(session * hoPtr)->add_dht_router(addr);
         }
      |]

addDhtNode :: MonadIO m => Session -> Text -> C.CInt -> m ()
addDhtNode ho host port =
  liftIO . withPtr ho $ \hoPtr -> do
    hstr <- textToStdString host
    withPtr hstr $ \hstrPtr ->
      [C.block| void {
          std::pair<string, int> addr = std::pair<string, int>(*$(string * hstrPtr), $(int port));
          $(session * hoPtr)->add_dht_node(addr);
         }
      |]

dhtGetItem :: MonadIO m => Session -> Sha1Hash -> m ()
dhtGetItem ho ih =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ih $ \ihPtr ->
  [C.exp| void { $(session * hoPtr)->dht_get_item(*$(sha1_hash * ihPtr)) } |]

dhtGetItem' :: MonadIO m => Session -> DhtItemKey -> Maybe ByteString -> m ()
dhtGetItem' ho (DhtItemKey key) salt =
  liftIO . withPtr ho $ \hoPtr -> do
    let salt' = fromMaybe BS.empty salt
    [C.block| void {
        boost::array<char, 32> key;
        for (int i = 0; i < 32; i++)
          key[i] = $bs-ptr:key[i];
        $(session * hoPtr)->dht_get_item(key, std::string($bs-ptr:salt', $bs-len:salt'));
       }
    |]

-- | Put dht item from bencoded data.
dhtPutItem :: MonadIO m => Session -> ByteString -> m Sha1Hash
dhtPutItem ho bencData =
  liftIO . withPtr ho $ \hoPtr ->
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

addExtension :: MonadIO m => Session -> Ptr C'Plugin -> m ()
addExtension ho plugin =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->add_extension(*$(Plugin * plugin)) } |]

addTorrentExtension :: MonadIO m => Session -> Ptr C'TorrentPlugin -> m ()
addTorrentExtension ho plugin =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| void { $(session * hoPtr)->add_extension(*$(TorrentPlugin * plugin)) } |]

loadCountryDb :: MonadIO m => Session -> Text -> m ()
loadCountryDb ho file =
  liftIO . withPtr ho $ \hoPtr -> do
  fstr <- textToStdString file
  liftIO . withPtr fstr $ \filePtr ->
    [C.exp| void { $(session * hoPtr)->load_country_db($(string * filePtr)->c_str()) } |]

loadAsnumDb :: MonadIO m => Session -> Text -> m ()
loadAsnumDb ho file =
  liftIO . withPtr ho $ \hoPtr -> do
  fstr <- textToStdString file
  withPtr fstr $ \filePtr ->
    [C.exp| void { $(session * hoPtr)->load_asnum_db($(string * filePtr)->c_str()) } |]

-- TODO:
-- int as_for_ip (address const& addr);
-- ip_filter get_ip_filter () const;
-- void set_ip_filter (ip_filter const& f);
-- void set_port_filter (port_filter const& f);

sessionId :: MonadIO m => Session -> m Sha1Hash
sessionId ho =
  liftIO . withPtr ho $ \hoPtr -> do
  fromPtr [CU.exp| sha1_hash * { new sha1_hash($(session * hoPtr)->id()) } |]

setPeerId :: MonadIO m => Session -> Sha1Hash -> m ()
setPeerId ho ih =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ih $ \ihPtr ->
  [C.exp| void { $(session * hoPtr)->set_peer_id(sha1_hash(*$(sha1_hash * ihPtr))) } |]

setKey :: MonadIO m => Session -> C.CInt -> m ()
setKey ho key =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->set_key($(int key)) } |]

-- | It can throw 'LibtorrentException'.
listenOn :: MonadIO m => Session -> (C.CInt, C.CInt) -> Maybe Text -> m ()
listenOn ho (fromPort, toPort) iface =
  liftIO . withPtr ho $ \hoPtr -> do
    withErrorCode SessionError $ \ecPtr ->
      case iface of
        Just iface' ->
          withCString (T.unpack iface') $ \ifacePtr ->
          [C.exp| void { $(session * hoPtr)->listen_on( std::pair<int, int>($(int fromPort), $(int toPort)), *$(error_code * ecPtr), $(char * ifacePtr)) } |]
        Nothing ->
          [C.exp| void { $(session * hoPtr)->listen_on( std::pair<int, int>($(int fromPort), $(int toPort)), *$(error_code * ecPtr)) } |]

isListening :: MonadIO m => Session -> m Bool
isListening ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(session * hoPtr)->is_listening() } |]

listenPort :: MonadIO m => Session -> m C.CUShort
listenPort ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| unsigned short { $(session * hoPtr)->listen_port() } |]

sslListenPort :: MonadIO m => Session -> m C.CUShort
sslListenPort ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| unsigned short { $(session * hoPtr)->ssl_listen_port() } |]

removeTorrent :: MonadIO m => Session -> TorrentHandle -> Maybe (BitFlags RemoveTorrentOptions) -> m ()
removeTorrent ho th opts =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr th $ \thPtr -> do
  let opts' = fromIntegral $ maybe 0 fromEnum opts
  [C.exp| void { $(session * hoPtr)->remove_torrent(*$(torrent_handle * thPtr), $(int opts')) } |]

getSessionSettings :: MonadIO m => Session -> m SessionSettings
getSessionSettings ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| session_settings * { new session_settings($(session * hoPtr)->settings()) } |]

setSessionSettings :: MonadIO m => Session -> SessionSettings -> m ()
setSessionSettings ho ds =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [C.exp| void { $(session * hoPtr)->set_settings(*$(session_settings * dsPtr)) } |]

getPeSettings :: MonadIO m => Session -> m PeSettings
getPeSettings ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| pe_settings * { new pe_settings($(session * hoPtr)->get_pe_settings()) } |]

setPeSettings :: MonadIO m => Session -> PeSettings -> m ()
setPeSettings ho ds =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ds $ \dsPtr ->
  [C.exp| void { $(session * hoPtr)->set_pe_settings(*$(pe_settings * dsPtr)) } |]

setProxy :: MonadIO m => Session -> ProxySettings -> m ()
setProxy ho ps =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ps $ \psPtr ->
  [C.exp| void { $(session * hoPtr)->set_proxy(*$(proxy_settings * psPtr)) } |]

proxy :: MonadIO m => Session -> m ProxySettings
proxy ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| proxy_settings * { new proxy_settings($(session * hoPtr)->proxy()) } |]

setI2pProxy :: MonadIO m => Session -> ProxySettings -> m ()
setI2pProxy ho ps =
  liftIO . withPtr ho $ \hoPtr ->
  withPtr ps $ \psPtr ->
  [C.exp| void { $(session * hoPtr)->set_i2p_proxy(*$(proxy_settings * psPtr)) } |]

i2pProxy :: MonadIO m => Session -> m ProxySettings
i2pProxy ho =
  liftIO . withPtr ho $ \hoPtr ->
  fromPtr [C.exp| proxy_settings * { new proxy_settings($(session * hoPtr)->i2p_proxy()) } |]

popAlerts :: MonadIO m => Session -> m (StdDeque Alert)
popAlerts ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- newAlertDeque
  withPtr res $ \resPtr ->
    [C.exp| void { $(session * hoPtr)->pop_alerts($(DequeAlertPtr * resPtr)) } |]
  return res

-- alert const* wait_for_alert (time_duration max_wait);

setAlertMask :: MonadIO m => Session -> BitFlags AlertCategory -> m ()
setAlertMask ho flags =
  liftIO . withPtr ho $ \hoPtr -> do
  let val = fromIntegral $ fromEnum flags
  [C.exp| void { $(session * hoPtr)->set_alert_mask($(uint32_t val)) } |]

-- FIXME: it causes a deadlock in C code but it'll be removed in next release of libtorrent.
-- -- | Set alerts handler for 'Session'. Returns finalizer for allocated 'FunPtr'.
-- setAlertDispatch :: MonadIO m => Session -> (Alert -> IO ()) -> m (FunPtr C'AlertDispatchCallback)
-- setAlertDispatch ses cb = do
--   let c'cb = \aPtr -> (fromPtr $ pure aPtr) >>= cb
--   liftIO $ c'setAlertDispatch ses c'cb

-- c'setAlertDispatch :: Session -> C'AlertDispatchCallback -> IO (FunPtr C'AlertDispatchCallback)
-- c'setAlertDispatch ho cb =
--   withPtr ho $ \hoPtr -> do
--     cbPtr <- $(C.mkFunPtr [t| C'AlertDispatchCallback |]) cb
--     [C.block| void {
--         $(session * hoPtr)->set_alert_dispatch([=](auto_ptr<alert> aptr) { $(AlertDispatchCallback cbPtr)(aptr.release());});
--        }
--     |]
--     return cbPtr

stopLsd :: MonadIO m => Session -> m ()
stopLsd ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->stop_lsd() } |]

startLsd :: MonadIO m => Session -> m ()
startLsd ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->start_lsd() } |]

stopUpnp :: MonadIO m => Session -> m ()
stopUpnp ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->stop_upnp() } |]

startUpnp :: MonadIO m => Session -> m ()
startUpnp ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->start_upnp() } |]

deletePortMapping :: MonadIO m => Session -> C.CInt -> m ()
deletePortMapping ho handle =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->delete_port_mapping($(int handle)) } |]

-- TODO:
-- int add_port_mapping (protocol_type t, int external_port, int local_port);

stopNatpmp :: MonadIO m => Session -> m ()
stopNatpmp ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->stop_natpmp() } |]

startNatpmp :: MonadIO m => Session -> m ()
startNatpmp ho =
  liftIO . withPtr ho $ \hoPtr ->
  [C.exp| void { $(session * hoPtr)->start_natpmp() } |]
