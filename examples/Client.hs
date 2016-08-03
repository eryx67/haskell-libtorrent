{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
-- |
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent (throwTo, myThreadId)
import           Control.Concurrent.Async
import           Control.Exception (AsyncException(UserInterrupt))
import           Control.Monad (forever, when, unless, void,
                                forM_, forM, filterM, zipWithM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Writer as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import           Data.Char (toLower, isUpper)
import           Data.Default
import           Data.List (find)
import           Data.Maybe (isJust)
import           Data.Set ((\\), union)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector.Storable as V
import           Data.Word (Word16)
import           Foreign.C.Types (CInt, CFloat)
import           Formatting
import           Formatting.ShortFormatters (pf, f)
import           Options.Applicative
import qualified System.Console.ANSI as Console
import           System.EasyFile
import           System.IO (BufferMode(..),
                            hIsTerminalDevice , hFlush, hSetEcho,
                            stdin, stdout, stderr, hSetBuffering)
import           System.IO.Error (isDoesNotExistError)
import           System.Timeout.Lifted (timeout)
import           Text.Read (readMaybe)
import qualified GHC.Exts as Exts


import           Network.Libtorrent


data Config = Config {
  _port            :: !CInt
  , _savePath        :: !Text
  , _maxDownloadRate :: !(Maybe CInt) -- kilobytes
  , _maxUploadRate   :: !(Maybe CInt) -- kilobytes
  , _httpProxy       :: !(Maybe (Text, Word16))
  , _sources         :: ![Text]
  }
            deriving Show

instance Default Config where
  def = Config {
    _port              = 6881
    , _savePath        = "."
    , _maxDownloadRate = Nothing
    , _maxUploadRate   = Nothing
    , _httpProxy       = Nothing
    , _sources         = []
  }

main :: IO ()
main = runStderrLoggingT $ do
  isTerm <- liftIO $ hIsTerminalDevice stdout
  unless isTerm $
    error "Program must be run in terminal."

  liftIO $ hSetBuffering stdin NoBuffering >> hSetEcho stdin False

  cfg@Config{..} <- liftIO $ execParser opts

  ses <- newSession

  sets <- getSessionSettings ses
  setDownloadRateLimit sets $ maybe (-1) (* 1000) _maxDownloadRate
  setUploadRateLimit sets $ maybe (-1) (* 1000) _maxUploadRate
  setUserAgent sets $ T.append "torrent/" version
  setSessionSettings ses sets

  case _httpProxy of
    Just (hn, port) -> do
      ps <- newProxySettings
      setProxyType ps ProxyHttp
      setProxyHostname ps hn
      setProxyPort ps port
      setProxy ses ps
    Nothing ->
      pure ()

  listenOn ses (_port, _port + 10) Nothing
  setAlertMask ses $ Exts.fromList [minBound..]

  let dhtRouters ::[(Text, CInt)] = [
        ("router.bittorrent.com", 6881)
        , ("router.utorrent.com", 6881)
        , ("router.bitcomet.com", 6881)
        , ("dht.transmissionbt.com", 6881)
        ]
  forM_  dhtRouters (uncurry $ addDhtRouter ses)
  startDht ses

  tid <- liftIO myThreadId
  logger <- askLoggerIO

  atpSrcs <- forM _sources createAtpSrc
  atps <- forM atpSrcs (createAtp cfg)
  bracket (zipWithM (addTorrent' cfg ses) atps atpSrcs)
    (\ths -> do
        void . timeout (10 * 1000000) $ saveFastresume ses ths
        liftIO resetScreen) $ \ths ->
    liftIO $ race_
    (forever $ runLoggingT (handleKeyboard $ keyHlrs ths tid) logger)
    (forever $ updateScreen ses ths)
  where
    opts = info (helper <*> optParser)
      ( fullDesc
        <> progDesc "BitTorrent client"
        <> header "client - bittorrent client")
    keyHlrs ths tid =
      [keyboardHandler 'q'  $ do
          $(logDebug) "user request quit"
          liftIO $ throwTo tid UserInterrupt
      , keyboardHandler 'r' $ do
          $(logDebug) "reannouncing"
          forM_ ths (\th -> forceReannounce th Nothing Nothing)
      , keyboardHandler 'p' $ do
          $(logDebug) "pausing"
          forM_ ths (\th -> autoManaged th False >> pause th Nothing)
      , keyboardHandler 'u' $ do
          $(logDebug) "resuming"
          forM_ ths (\th -> autoManaged th True >> resume th)
      ]
    createAtpSrc src
      | isJust $ find (flip T.isPrefixOf src) ["magnet:", "http://", "https://"] =
        return $ UrlSrc src
      | otherwise = do
        ti <- newTorrentInfo src
        return $ TorrentInfoSrc ti
    createAtp Config{..} src = do
      atp <- newAddTorrentParams src
      setStorageMode atp StorageModeSparse
      (BitFlags atpFlags) <- getFlags atp
      setFlags atp  . BitFlags . union (Exts.fromList [AutoManaged, DuplicateIsError]) $
        atpFlags \\ (Exts.fromList [Paused])
      setTorrentSavePath atp _savePath
      setMaxConnections atp 60
      setMaxUploads atp (-1)
      return atp
    addTorrent' Config{..} ses atp (TorrentInfoSrc ti) = do
      tiName <- torrentInfoName ti
      let rfn = resumeFileName _savePath tiName
      $(logInfo) $ sformat ("loading torrent " % stext) tiName
      handleIOError
        (\e ->
          if isDoesNotExistError e
          then $(logDebug) $ sformat ("no resume data for " % shown) rfn
          else throwM e) $ do
          resumeData <- liftIO $ BS.readFile rfn
          liftIO $ removeFile rfn
          setResumeData atp resumeData
      addTorrent ses atp
    addTorrent' _ ses atp _ = do
      addTorrent ses atp
    updateScreen ses ths = do
      clearScreen
      txt <- W.execWriterT $ do
        forM_ ths $ \th -> do
          pis <- getPeerInfo th >>= liftIO . toList
          dq <- getDownloadQueue th >>= liftIO . toList
          torrentProgress th
          printPeerInfo pis
          printDownloadQueue dq
          filesProgress th
        printAlerts ses
        printMenu
      TL.putStrLn txt
      hFlush stdout
      threadDelay 500000

optParser :: Parser Config
optParser = Config
            <$> option auto
            (long "port"
             <> short 'p'
             <> metavar "PORT"
             <> help "set listening port"
             <> value (_port def))
            <*> (T.pack <$> strOption
                 (long "save-path"
                  <> short 's'
                  <> metavar "SAVE-PATH"
                  <> help "the path where the downloaded file/folder should be placed"
                  <> value (T.unpack $ _savePath def)))
            <*> (Just <$> (option auto
                           (long "max-download-rate"
                            <> short 'd'
                            <> metavar "DOWNLOAD-RATE"
                            <> help "the maximum download rate given in kB/s"
                           ))
                <|> pure Nothing)
            <*> (Just <$> (option auto
                          (long "max-upload-rate"
                           <> short 'u'
                           <> metavar "UPLOAD-RATE"
                           <> help "the maximum upload rate given in kB/s"
                          ))
                 <|> pure Nothing)
            <*> (Just <$> (option hostReader
                           (long "proxy-host"
                            <> short 'r'
                            <> metavar "HTTP-PROXY"
                            <> help "sets HTTP proxy host and port (separated by ':')"
                           ))
                <|> pure Nothing)

            <*> ((map T.pack) <$> (some (argument str
                                         (metavar "FILE-OR-URL"
                                          <> help "torrent file name, magnet or http url"))))
  where
    hostReader = eitherReader $ \hostStr ->
      case span (/= ':') hostStr of
        ("", _) -> Left "bad hostname, no host"
        (_, "") -> Left "bad hostname, no port"
        (hn, portStr) ->
          maybe (Left "bad hostname, invalid port") (Right . (T.pack hn, )) $
          readMaybe portStr

keyboardHandler :: Monad m => Char -> m () -> (Char -> Maybe (m ()))
keyboardHandler c hlr =
  \cin ->
  if cin == c then Just hlr
  else Nothing

handleKeyboard :: MonadIO m => [Char -> Maybe (m ())] -> m ()
handleKeyboard hlrs = do
  c <- liftIO getChar
  go c hlrs
  where
    go _ [] = pure ()
    go c (th:ths) =
      case th c of
        Just h ->
          h
        Nothing ->
          go c ths

saveFastresume :: forall m . (MonadIO m, MonadBaseControl IO m) =>
                  Session -> [TorrentHandle] -> LoggingT m ()
saveFastresume ses ths = do
  sessionPause ses
  saveThs <- filterM isValid ths >>= filterM isToSave
  forM_ saveThs $ flip saveResumeData Nothing
  waitAlerts $ length saveThs
  where
    isToSave th = do
      st <- torrentStatus th Nothing
      hm <- getHasMetadata st
      ns <- needSaveResumeData th
      return $ hm && ns
    waitAlerts 0 = pure ()
    waitAlerts n = do
      let alertHlrs = [AlertHandler saveResume, AlertHandler saveResumeFailed]
      processed <- fmap length $ sessionHandleAlerts ses alertHlrs
      waitAlerts $ 0 `max` n - processed
    saveResume a = do
      th <- getHandle a
      st <- torrentStatus th . Just $ Exts.fromList [QuerySavePath, QueryName]
      rd <- bencodedData <$> saveResumeDataAlertResumeData a
      sp <- getSavePath st
      tn <- getName st
      let rfn = resumeFileName sp tn
      liftIO $ BS.writeFile rfn rd
      $(logInfo) $ sformat ("resume data saved for " % stext % " to " % stext) tn sp
    saveResumeFailed a = do
      th <- getHandle a
      st <- torrentStatus th . Just $ Exts.fromList [QuerySavePath, QueryName]
      tn <- getName st
      ec <- saveResumeDataFailedAlertError a
      $(logError) $ sformat ("resume data save failed for " % stext % " reason " % shown) tn ec

printMenu :: MonadIO m => W.WriterT TL.Text m ()
printMenu = do
  outln hr
  outln "(q)uit), (p)ause), (u)npause), (r)eannounce"
  outln hr
  where
    hr = TL.replicate 76 "-"

printAlerts :: MonadIO m => Session -> W.WriterT TL.Text m ()
printAlerts ses = do
  as <- popAlerts ses >>= liftIO . toList
  forM_ as $ \a -> do
    msg <- alertMessage a
    outln $ format stext msg

torrentProgress :: MonadIO m => TorrentHandle -> W.WriterT TL.Text m ()
torrentProgress th = do
  s <- torrentStatus th Nothing
  hm <- getHasMetadata s

  name <- if hm
          then torrentFile th >>= torrentInfoName >>= return . T.take 40
          else pure "-"
  outln $ format ((left 40 ' ') %. stext) name

  state <- getState s
  when (state /= Seeding) $ do
    progress <- getProgress s
    outln $ format (string % " " % (pf 4) % "%") (camelToSpaces $ show state) (progress * 100)
    td <- getTotalDone s
    np <- getNumPeers s
    ns <- getNumSeeds s
    dc <- getDistributedCopies s

    outln $ format ("total downloaded: " % int % "Bytes") td
    outln $ format ("peers: " % int % " seeds: " % int % " distributed copies: " % (f 2))
      np ns dc

  dr   <- getDownloadRate s
  tdld <- getTotalDownload s
  ur   <- getUploadRate s
  tuld <- getTotalUpload s
  pg   <- getProgress s

  outln $ format ("download: " % (bytes (pf 6)) % "/s (" % int % ")") dr tdld
  outln $ format ("upload: " % (bytes (pf 6)) % "/s (" % int % ")") ur tuld

  when (state /= Seeding) $ do
    ih <- getTorrentStatusInfoHash s >>= liftIO . sha1HashToByteString
    na <- getTorrentStatusNextAnnounce s
    ct <- getCurrentTracker s
    outln $ format ("info-hash: " % stext) (TE.decodeUtf8 $ BS16.encode ih)
    outln $ format ("next announce: " % int) na
    outln $ format ("tracker: " % stext) ct
    outln . format stext $ progressBar pg 49

filesProgress :: MonadIO m => TorrentHandle -> W.WriterT TL.Text m ()
filesProgress th = do
  s <- torrentStatus th Nothing
  state <- getState s
  hm <- getHasMetadata s

  when (hm && state /= Seeding) $ do
    pgs :: [CFloat] <- (map fromIntegral . V.toList) <$> fileProgress th Nothing
    ti <- torrentFile th
    fs <- files ti
    fn <- fileStorageNumFiles fs
    fes <- mapM (fileEntryAt fs) [0..fn - 1]
    forM_ (zip pgs fes) $ \(pg, fe) -> do
      fp <- getPath fe
      fsz <- fromIntegral <$> getSize fe
      outln $ format (stext %  " " % stext) (progressBar (pg / fsz) 20) fp

printPeerInfo :: MonadIO m => [PeerInfo] -> W.WriterT TL.Text m ()
printPeerInfo pis = do
  outln "down       (total)     up       (total)   q  r  flags  block progress  client"
  forM_ pis $ \pi -> do
    ds <- getDownSpeed pi
    tdld <- getPeerInfoTotalDownload pi
    us <- getUpSpeed pi
    tuld <- getPeerInfoTotalUpload pi
    dql <- getDownloadQueueLength pi
    uql <- getUploadQueueLength pi
    (BitFlags flags) <- getPeerInfoFlags pi
    client <- getClient pi
    dpi    <- getDownloadingPieceIndex pi
    ps     <- getDownloadingProgress pi
    dldt <- getDownloadingTotal pi
    let flagsTxt =
          T.pack $
          zipWith (\flg c ->
                    if elem flg flags then c else '.')
          [Interesting, Choked, RemoteInterested, RemoteChoked, SupportsExtensions, LocalConnection]
          ['I', 'C', 'i', 'c', 'e', 'l']
        progress = if dpi >= 0
                   then progressBar (fromIntegral ps / fromIntegral dldt) 14
                   else progressBar 0 14
        clientTxt = case True of
          _ | elem Handshake flags -> "waiting for handshake"
          _ | elem Connecting flags -> "connecting to peer"
          _ | elem Queued flags -> "queued"
          _ -> client
    outln $ format ((right 8 ' ' %. (bytes (pf 5))) % "/s(" % (left 8 ' ' %. (bytes (pf 5))) % ") "
                  % (right 8 ' ' %. (bytes (pf 5))) % "/s(" % (left 8 ' ' %. (bytes (pf 5))) % ") "
                  % ((left 2 '0') %. int) % " " % ((left 2 '0') %. int) % " "
                  % stext % " "
                  % stext % " "
                  % stext)
      ds tdld us tuld dql uql flagsTxt progress (T.take 10 clientTxt)

printDownloadQueue :: MonadIO m => [PartialPieceInfo] -> W.WriterT TL.Text m ()
printDownloadQueue dq = forM_ dq $ \ppi -> do
  pidx <- getPieceIndex ppi
  blocks <- getBlocks ppi >>= liftIO .toList
  blocksTxt <- T.pack <$> forM blocks
               (\b -> do
                   bs <- getBlockInfoState b
                   return $ case bs of
                     BlockRequested -> '-'
                     BlockWriting   -> '='
                     BlockFinished  -> '#'
                     BlockNone      -> ' '
               )
  outln $ format (((left 4 ' ') %. int) % "[" % stext % "]") pidx blocksTxt

progressBar :: CFloat -> Int -> Text
progressBar progress width =
  T.append (T.replicate pcNum "#") (T.replicate (width - pcNum) "-")
  where
    pcNum = truncate $ progress * (fromIntegral width) + 0.5

outln :: MonadIO m => TL.Text -> W.WriterT TL.Text m ()
outln txt = W.tell txt >> W.tell "\n"

resumeFileName :: Text -> Text -> FilePath
resumeFileName savePath torrentName =
  T.unpack savePath </> T.unpack torrentName <.> "fastresume"

camelToSpaces :: String -> String
camelToSpaces "" = ""
camelToSpaces (c:rst) = toLower c:go rst
  where
    go [] = []
    go (x:rest)
      | isUpper x = ' ':toLower x:go rest
      | otherwise = x:go rest

clearScreen :: IO ()
clearScreen = do
  hFlush stdout
  hFlush stderr
  Console.clearScreen
  Console.setCursorPosition 0 0

resetScreen :: IO ()
resetScreen = do
  hFlush stdout
  hFlush stderr
  Console.clearScreen
  Console.setSGR [Console.Reset]
  Console.setCursorPosition 0 0
