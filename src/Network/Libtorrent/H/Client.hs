{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |

module Network.Libtorrent.H.Client (ClientT(..)
                                   , MonadClient(..)
                                   , MonadClientBase
                                   , MonadClientRunBase
                                   , ClientOpts
                                   , ClientState
                                   , TorrentMetadata
                                   , optsAlertMask
                                   , optsAppDir
                                   , optsDhtRouters
                                   , optsDownloadDir
                                   , optsListenPorts
                                   , optsMetadataDir
                                   , optsSessionOpts
                                   , optsSessionStateFile
                                   , optsUseDht
                                   , optsUseLsd
                                   , optsUseNatPnp
                                   , optsUseUpnp
                                   , optsTorrentOpts
                                   , torrentOpts
                                   , alertChannel
                                   , metadataChannel
                                   , appDir
                                   , downloadDir
                                   , metadataDir
                                   , session
                                   , sessionStateFile
                                   , runClientT
                                   , addAlertHandler
                                   , addMetadataHandler
                                   , tmMetadata
                                   , tmInfoHash
                                   , addTorrent
                                   , addTorrentWith
                                   , asyncAddTorrent
                                   , asyncAddTorrentWith
                                   , sessionSettings
                                   , sessionState
                                   , torrentStatuses
                                   ) where

import           Control.Concurrent                  (ThreadId, forkIO,
                                                      threadDelay)
import qualified Control.Concurrent.Async.Lifted     as AL
import           Control.Concurrent.STM.TMChan
import           Control.Error.Util
import           Control.Exception                   (IOException)
import           Control.Lens                        hiding ((<.>))
import           Control.Monad                       (forM_, void, when,
                                                      zipWithM_)
import           Control.Monad.Base                  (MonadBase (..))
import           Control.Monad.Catch
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Loops                 (untilJust, whileJust_,
                                                      whileM_)
import           Control.Monad.Reader                (MonadReader, ReaderT (..),
                                                      asks, runReaderT)
import qualified Control.Monad.Reader                as Reader
import qualified Control.Monad.State.Lazy            as LS
import qualified Control.Monad.State.Strict          as S
import           Control.Monad.STM
import           Control.Monad.Trans.Class           (MonadTrans, lift)
import           Control.Monad.Trans.Control         (MonadBaseControl (..))
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Bifunctor                      (second)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as BS16
import           Data.Default
import           Data.Maybe                          (fromMaybe)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import           Foreign.C.Types                     (CInt)
import           Formatting
import qualified GHC.Exts                            as Exts
import           GHC.Generics                        (Generic)
import           System.EasyFile                     (combine,
                                                      createDirectoryIfMissing,
                                                      (<.>), (</>))
import           System.Timeout.Lifted               (timeout)

import           Network.Libtorrent                  hiding (addTorrent,
                                                      asyncAddTorrent)
import qualified Network.Libtorrent                  as LT
import           Network.Libtorrent.H.AddTorrentOpts
import           Network.Libtorrent.H.AlertEvent
import           Network.Libtorrent.H.Rays
import           Network.Libtorrent.H.SessionOpts
import           Network.Libtorrent.H.Types

data ClientOpts = ClientOpts {
  _optsAppDir             :: !FilePath
  , _optsSessionStateFile :: !FilePath
  , _optsDownloadDir      :: !FilePath
  , _optsMetadataDir      :: !FilePath
  , _optsListenPorts      :: !(Int, Int)
  , _optsUseUpnp          :: !Bool
  , _optsUseNatPnp        :: !Bool
  , _optsUseLsd           :: !Bool
  , _optsUseDht           :: !Bool
  , _optsDhtRouters       :: ![(Text, Int)]
  , _optsAlertMask        :: ![AlertCategory]
  , _optsSessionOpts      :: !SessionOpts
  , _optsTorrentOpts      :: !AddTorrentOpts
  }
            deriving (Show, Generic)

instance Default ClientOpts where
  def = ClientOpts {
    _optsAppDir = "."
    , _optsSessionStateFile = "session.state"
    , _optsDownloadDir = "download"
    , _optsMetadataDir = "metadata"
    , _optsListenPorts = (6881, 6891)
    , _optsUseUpnp          = True
    , _optsUseNatPnp        = True
    , _optsUseLsd           = True
    , _optsUseDht           = True
    , _optsDhtRouters  = [
        ("router.bittorrent.com", 6881)
        , ("router.utorrent.com", 6881)
        , ("router.bitcomet.com", 6881)
        , ("dht.transmissionbt.com", 6881)
        ]
    , _optsAlertMask = [minBound..]
    , _optsSessionOpts = def
    , _optsTorrentOpts = def
    }

makeLenses ''ClientOpts

instance ToJSON ClientOpts  where
   toJSON = genericToJSON $ aesonDrop 5 snakeCase
instance FromJSON ClientOpts where
   parseJSON = genericParseJSON $ aesonDrop 5 snakeCase

data TorrentMetadata = TorrentMetadata {
  _tmInfoHash   :: !Text
  -- ^ Hex-encoded infohash
  , _tmMetadata :: !ByteString
  -- ^ Metadata bencoded dictionary
  }
  deriving Show

makeLenses ''TorrentMetadata

-- | State for 'ClientT' monad
data ClientState = ClientState {
  _appDir             :: !FilePath
    -- ^ Top directory where all other files are expected
  , _sessionStateFile :: !FilePath
    -- ^ File for saving/load session state
  , _downloadDir      :: !FilePath
    -- ^ Directory where uploaded/dowloaded torrents are located
  , _metadataDir      :: !FilePath
    -- ^ directory for metadata - torrent files, etc
  , _session          :: !Session
    -- ^ libtorrent session
  , _torrentOpts      :: !AddTorrentOpts
  , _alertChannel     :: !(TMChan AlertEvent)
    -- ^ channel where all alerts are published
  , _metadataChannel  :: !(TMChan TorrentMetadata)
    -- ^ channel where all alerts are published

  }

makeLenses ''ClientState

newtype ClientT m a = ClientT { unClientT :: ReaderT ClientState m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadMask
           , MonadCatch
           , MonadTrans
           , MonadReader ClientState
           )

instance (MonadBase b m) => MonadBase b (ClientT m) where
  liftBase = ClientT . liftBase

instance (Monad m, MonadBaseControl b m) => MonadBaseControl b (ClientT m) where
  type StM (ClientT m) a = StM (ReaderT ClientState m) a
  liftBaseWith f = ClientT $ liftBaseWith $ \run -> f (run . unClientT)
  restoreM = ClientT . restoreM

instance MonadLogger m => MonadLogger (ClientT m) where
  monadLoggerLog a b c d = ClientT $ monadLoggerLog a b c d

instance MonadLoggerIO m => MonadLoggerIO (ClientT m) where
  askLoggerIO = ClientT askLoggerIO

type MonadClientBase m = (Functor m, Applicative m, Monad m, MonadIO m, MonadThrow m, MonadCatch m)
type MonadClientRunBase m = (MonadClientBase m, MonadMask m, MonadLoggerIO m, MonadBaseControl IO m)

-- | Monads in which 'ClientT' actions may be embedded.
class MonadClientBase m => MonadClient b m | m -> b
  where
    -- | Lift a computation to the 'ClientT' monad.
    liftClientT :: ClientT b a -> m a
    -- | Execute an action with a modified 'ClientState'.
    localState :: (ClientState -> ClientState) -> m a -> m a

instance MonadClientBase m => MonadClient m (ClientT m) where
    liftClientT = id
    localState  = Reader.local

instance (MonadClientBase m, MonadClient b m) => MonadClient b (ReaderT r m) where
    liftClientT    = lift . liftClientT
    localState f m = ReaderT (localState f . runReaderT m)

instance MonadClient b m => MonadClient b (S.StateT s m) where
    liftClientT    = lift . liftClientT
    localState f m = S.StateT (localState f . S.runStateT m)

instance MonadClient b m => MonadClient b (LS.StateT s m) where
    liftClientT    = lift . liftClientT
    localState f m = LS.StateT (localState f . LS.runStateT m)

instance MonadClient b m => MonadClient b (ExceptT e m) where
    liftClientT    = lift . liftClientT
    localState f m = ExceptT $ localState f (runExceptT m)

runClientT :: MonadClientRunBase m =>
              ClientOpts -> ClientT m a -> m a
runClientT ClientOpts{..} action = do
  ses <- newSession
  ac <- startAlertListener ses
  mc <- startMetadataListener ac
  logger <- askLoggerIO

  cs@ClientState{..} <- optsToState ses ac mc
  createLayout cs

  void . runExceptT $ loadSessionState ses _sessionStateFile `catchE` $(logError)
  setAlertMask ses $ Exts.fromList _optsAlertMask
  listenOn ses (bimap fromIntegral fromIntegral _optsListenPorts) Nothing
  startServices ses ac

  let shutdown = do
        saveSessionState ses _sessionStateFile
        stopAlertListener ac
  (unClientT (addMetadataHandler (metadataWriter _metadataDir logger) (pure ()) >> action)
    `runReaderT` cs)
    `finally` shutdown

  where
    optsToState ses ac mc = do
      ss <- getSessionSettings ses
      sessionOptsToSettings _optsSessionOpts ss
      setSessionSettings ses ss
      let ddir = combine _optsAppDir _optsDownloadDir
          tpFlags = Exts.fromList [Paused, AutoManaged, UpdateSubscribe, ApplyIpFilter
                                  , DuplicateIsError, OverrideResumeData
                                  ]
          topts = mergeOpts _optsTorrentOpts $ def
                  & torrentSavePath .~ (Just $ T.pack ddir)
                  & flags .~ Just tpFlags
      return ClientState {
        _appDir               = _optsAppDir
        , _sessionStateFile   = combine _optsAppDir _optsSessionStateFile
        , _downloadDir        = ddir
        , _metadataDir        = combine _optsAppDir _optsMetadataDir
        , _session            = ses
        , _torrentOpts        = topts
        , _alertChannel       = ac
        , _metadataChannel    = mc
        }

    createLayout ClientState{..} = do
      liftIO . forM_ [_appDir, _downloadDir, _metadataDir] $
        createDirectoryIfMissing True
    startServices ses ac = do
      dhtAC <- liftIO . atomically $ dupTMChan ac
      zipWithM_ (\p a -> when p a)
        [_optsUseUpnp, _optsUseNatPnp, _optsUseLsd , _optsUseDht]
        [startNatpmp ses
        , startUpnp ses
        , startLsd ses
        , void $ bootDht ses dhtAC (map (second fromIntegral) _optsDhtRouters)
        ]

bootDht :: MonadLoggerIO m =>
           Session -> TMChan AlertEvent -> [(Text, CInt)] -> m ThreadId
bootDht ses ac routers = do
  logger <- askLoggerIO
  ss <- getSessionSettings ses
  startDht ses
  forM_  routers (uncurry $ addDhtRouter ses)

  liftIO . forkIO . void $
    (runExceptT $ bootstrapDht ss `catchE` $(logError)) `runLoggingT` logger
  where
    bootstrapDht ss = do
      res <- lift $ bracket
             (getDhtAnnounceInterval ss)
             (\ai -> do
                 ss' <- getSessionSettings ses
                 setDhtAnnounceInterval ss' ai
                 setSessionSettings ses ss'
             ) $ \_ -> do
               setDhtAnnounceInterval ss (60 * 1000000)
               setSessionSettings ses ss
               timeout (960 * 1000000) waitAlert
      failWith "dht bootstrap failed" res
    waitAlert =
      untilJust $ do
        e <- liftIO . atomically $ readTMChan ac
        return $ case e of
          Nothing -> Just ()
          Just (DhtBootstrapAlertEvent _) -> Just ()
          _ -> Nothing

-- | It throws 'LibtorrentException'.
addTorrent :: MonadIO m =>
              TorrentSrc -> ClientT m TorrentHandle
addTorrent src =
  addTorrentWith src def

-- | It throws 'LibtorrentException'.
addTorrentWith :: MonadIO m =>
                  TorrentSrc -> AddTorrentOpts -> ClientT m TorrentHandle
addTorrentWith src opts = do
  ses <- asks _session
  topts <- mergeOpts opts <$> asks (^. torrentOpts)
  tp <- newAddTorrentParams src >>= addTorrentOptsToParams topts
  LT.addTorrent ses tp

asyncAddTorrent :: MonadIO m =>
                   TorrentSrc -> ClientT m ()
asyncAddTorrent src =
  asyncAddTorrentWith src def

asyncAddTorrentWith :: MonadIO m =>
                       TorrentSrc -> AddTorrentOpts -> ClientT m ()
asyncAddTorrentWith src opts = do
  ses <- asks _session
  topts <- mergeOpts opts <$> asks (^. torrentOpts)
  tp <- newAddTorrentParams src >>= addTorrentOptsToParams topts
  LT.asyncAddTorrent ses tp


sessionSettings :: (MonadIO m, Traversable f) => Ray (ClientT m) f () SessionOpts SessionOpts
sessionSettings f = do
  ses <- asks _session
  st <- getSessionSettings ses
  sopts <- sessionSettingsToOpts st
  traverse (\so -> sessionOptsToSettings so st >> setSessionSettings ses st) $ f sopts

sessionState :: (MonadIO m, Traversable f) =>
                Maybe (BitFlags SaveStateFlags)
             -> Ray (ClientT m) f () ByteString ByteString
sessionState saveFlags f = do
  ses <- asks _session
  traverse (loadState ses) . f . bencodedData
    =<< saveState ses (Exts.fromList [minBound..] `fromMaybe` saveFlags)

torrentStatuses :: MonadIO m =>
                   Maybe (TorrentStatus -> Bool)
                -> Maybe (BitFlags StatusFlags)
                -> Maybe (StdVector TorrentStatus)
                -> ClientT m (StdVector TorrentStatus)
torrentStatuses statusFilter statusFlags statusV = do
  ses <- asks _session
  let sfltr = fromMaybe (const True) statusFilter
  case statusV of
    Nothing ->
      getTorrentStatus ses sfltr statusFlags
    Just v ->
      refreshTorrentStatus ses v statusFlags


-- | Add alert handler which will run until 'ClientT' monad is finished.
-- addAlertHandler :: (Monad m, MonadIO m) =>
--                     (AlertEvent -> IO a)
--                     -- ^ alert handler
--                  -> IO b
--                     -- ^ called when alert handler finished
--                  -> ClientT m ThreadId
addAlertHandler :: (MonadIO m, MonadBaseControl IO m, MonadMask m) =>
                   (AlertEvent -> ClientT m a) -> ClientT m b -> ClientT m (AL.Async (StM m ()))
addAlertHandler hlr finalizer = do
  ac <- asks _alertChannel
  acd <- liftIO . atomically $ dupTMChan ac
  AL.async . void $ listen acd `finally` finalizer
  where
    listen acd =
      whileJust_ (liftIO .atomically $ readTMChan acd) hlr

addMetadataHandler :: (MonadIO m, MonadBaseControl IO m, MonadMask m) =>
                      (TorrentMetadata -> ClientT m a)
                      -- ^ metadata handler
                   -> ClientT m b
                      -- ^ called when metadata handler finished
                   -> ClientT m (AL.Async (StM m ()))
addMetadataHandler hlr finalizer = do
  mc <- asks _metadataChannel
  mcd <- liftIO . atomically $ dupTMChan mc
  AL.async . void $ listen mcd `finally` finalizer
  where
    listen mcd =
      whileJust_ (liftIO . atomically $ readTMChan mcd) hlr

startAlertListener :: MonadIO m => Session -> m (TMChan AlertEvent)
startAlertListener ses = liftIO $ do
  ac <- newBroadcastTMChanIO
  void . forkIO $ listener ac
  return ac
  where
    listener ac =
      whileM_ (not <$> alertsClosed ac) $ do
      void . sessionHandleAlerts ses . alertEventHandlers $ \e ->
        atomically $ writeTMChan ac e
      threadDelay 200000

stopAlertListener :: MonadIO m => TMChan a -> m ()
stopAlertListener ac =
  liftIO . atomically $ closeTMChan ac

alertsClosed :: MonadIO m => TMChan a -> m Bool
alertsClosed ac =
  liftIO . atomically $ isClosedTMChan ac

metadataWriter :: (MonadIO m, MonadCatch m) =>
                  FilePath
               -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
               -> TorrentMetadata
               -> m (Maybe ())
metadataWriter dir logger (TorrentMetadata ih meta) =
  flip runLoggingT logger $ do
  let fn = (dir </> T.unpack ih <.> "torrent")
  (liftIO $ BS.writeFile fn meta) `catchIOError` \e ->
    $(logError) $ sformat ("writing metadata for " % string % " " % shown) fn e
  return $ Just ()

startMetadataListener :: MonadIO m =>
                         TMChan AlertEvent -> m (TMChan TorrentMetadata)
startMetadataListener ac = liftIO $ do
  acd <- atomically $ dupTMChan ac
  mc <- newBroadcastTMChanIO
  void . forkIO $ listener acd mc
  return mc
  where
    listener acd mc = do
      whileJust_ (atomically $ readTMChan acd) $ \case
        MetadataReceivedAlertEvent a -> do
          h <- getHandle a
          sh <- infoHash h
          ih <- sha1HashToByteString sh
          ti <- torrentFile h
          mi <- torrentInfoMetadata ti
          let ihHex = TE.decodeUtf8 $ BS16.encode ih
          atomically . writeTMChan mc $ TorrentMetadata ihHex mi
        _ -> return ()
      atomically $ closeTMChan mc

loadSessionState :: (MonadIO m, MonadCatch m) =>
                    Session -> FilePath -> ExceptT Text m ()
loadSessionState ses fp = do
  st <- liftIO $ BS.readFile fp
  loadState ses st
    `catches` [Handler (\(e::IOException) -> throwE . T.pack $ show e)
              , Handler (\(e::LibtorrentException) -> throwE . T.pack $ show e)
              ]

saveSessionState :: (MonadIO m, MonadCatch m, MonadLogger m) =>
                    Session -> FilePath -> m ()
saveSessionState ses fp = do
  bs <- bencodedData <$> saveState ses (Exts.fromList [minBound..])
  handleIOError
    (\e ->
      $(logError) $ sformat (shown % " when saving session state to " % shown) e fp) $
    liftIO (BS.writeFile fp bs)
