{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |

module Network.Libtorrent.H.AddTorrentOpts (AddTorrentOpts
                                           , addTorrentOptsToParams
                                           , addTorrentParamsToOpts
                                           , torrentFilePriorities
                                           , torrentTrackers
                                           , url
                                           , flags
                                           , downloadLimit
                                           , maxConnections
                                           , maxUploads
                                           , resumeData
                                           , sourceFeedUrl
                                           , storageMode
                                           , torrentName
                                           , torrentSavePath
                                           , trackerid
                                           , uploadLimit
                                           , urlSeeds
                                           , uuid
                                           , torrentInfoHash

                                           )where

import           Control.Applicative        ((<|>))
import           Control.Lens               hiding ((.=))
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as BS16
import qualified Data.ByteString.Base64     as BS64
import qualified Data.Default               as D
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Vector.Storable       (Vector)
import           Data.Word                  (Word8)
import           GHC.Generics               (Generic)

import           Network.Libtorrent
import           Network.Libtorrent.H.Types

data AddTorrentOpts = AddTorrentOpts {
  _flags                   :: !(Maybe (BitFlags AddTorrentFlags))
  , _torrentName           :: !(Maybe Text)
  , _torrentSavePath       :: !(Maybe Text)
  , _torrentTrackers       :: !(Maybe [Text])
  , _urlSeeds              :: !(Maybe [Text])
  , _resumeData            :: !(Maybe ByteString)
  , _storageMode           :: !(Maybe StorageMode)
  , _torrentFilePriorities :: !(Maybe (Vector Word8))
  , _trackerid             :: !(Maybe Text)
  , _url                   :: !(Maybe Text)
  , _uuid                  :: !(Maybe Text)
  , _sourceFeedUrl         :: !(Maybe Text)
  , _torrentInfoHash       :: !(Maybe ByteString)
  , _maxUploads            :: !(Maybe Int)
  , _maxConnections        :: !(Maybe Int)
  , _uploadLimit           :: !(Maybe Int)
  , _downloadLimit         :: !(Maybe Int)

  }
  deriving (Show, Generic)

instance MergeableOpts AddTorrentOpts where
  mergeOpts o1 o2 =
    AddTorrentOpts
    (_flags           o1 <|> _flags           o2)
    (_torrentName     o1 <|> _torrentName     o2)
    (_torrentSavePath o1 <|> _torrentSavePath o2)
    (_torrentTrackers        o1 <|> _torrentTrackers        o2)
    (_urlSeeds        o1 <|> _urlSeeds        o2)
    (_resumeData      o1 <|> _resumeData      o2)
    (_storageMode     o1 <|> _storageMode     o2)
    (_torrentFilePriorities  o1 <|> _torrentFilePriorities  o2)
    (_trackerid       o1 <|> _trackerid       o2)
    (_url             o1 <|> _url             o2)
    (_uuid            o1 <|> _uuid            o2)
    (_sourceFeedUrl   o1 <|> _sourceFeedUrl   o2)
    (_torrentInfoHash        o1 <|> _torrentInfoHash        o2)
    (_maxUploads      o1 <|> _maxUploads      o2)
    (_maxConnections  o1 <|> _maxConnections  o2)
    (_uploadLimit     o1 <|> _uploadLimit     o2)
    (_downloadLimit   o1 <|> _downloadLimit   o2)

makeLenses ''AddTorrentOpts

instance D.Default AddTorrentOpts where
  def = AddTorrentOpts
    Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing

instance ToJSON AddTorrentOpts  where
   toJSON AddTorrentOpts{..} = object [
       "flags"            .= (listFlags <$> _flags)
     , "name"             .= _torrentName
     , "save_path"        .= _torrentSavePath
     , "trackers"         .= _torrentTrackers
     , "url_seeds"        .= _urlSeeds
     , "resume_data"      .= (TE.decodeUtf8 . BS64.encode <$> _resumeData)
     , "storage_mode"     .= _storageMode
     , "file_priorities"  .= _torrentFilePriorities
     , "trackerid"        .= _trackerid
     , "url"              .= _url
     , "uuid"             .= _uuid
     , "source_feed_url"  .= _sourceFeedUrl
     , "info_hash"        .= (TE.decodeUtf8 . BS16.encode <$> _torrentInfoHash)
     , "max_uploads"      .= _maxUploads
     , "max_connections"  .= _maxConnections
     , "upload_limit"     .= _uploadLimit
     , "download_limit"   .= _downloadLimit
     ]
     where
       listFlags (BitFlags vs) = vs

instance FromJSON AddTorrentOpts where
   parseJSON = withObject "AddTorrentOpts" $ \o ->
     AddTorrentOpts
     <$> (fmap BitFlags <$> (o .:? "flags"))
     <*> o .:? "name"
     <*> o .:? "save_path"
     <*> o .:? "trackers"
     <*> o .:? "url_seeds"
     <*> (do
             mv <- fmap (BS64.decode . TE.encodeUtf8) <$> (o .:? "resume_data")
             case  mv of
               Nothing -> pure Nothing
               Just (Right v) -> pure $ Just v
               Just (Left e) -> fail e
         )
     <*> o .:? "storage_mode"
     <*> o .:? "file_priorities"
     <*> o .:? "trackerid"
     <*> o .:? "url"
     <*> o .:? "uuid"
     <*> o .:? "source_feed_url"
     <*> (do
             mv <- fmap (BS16.decode . TE.encodeUtf8) <$> (o .:? "info_hash")
             case  mv of
               Nothing -> pure Nothing
               Just (v, "") -> pure $ Just v
               Just _ -> fail "invalid info_hash"
         )

     <*> o .:? "max_uploads"
     <*> o .:? "max_connections"
     <*> o .:? "upload_limit"
     <*> o .:? "download_limit"

addTorrentOptsToParams :: MonadIO m => AddTorrentOpts -> AddTorrentParams -> m AddTorrentParams
addTorrentOptsToParams AddTorrentOpts{..} ps = liftIO $ do
  when (isJust _flags          ) $ setFlags ps $ fromJust _flags
  when (isJust _torrentName    ) $ setTorrentName ps $ fromJust _torrentName
  when (isJust _torrentSavePath    ) $ setTorrentSavePath    ps $ fromJust _torrentSavePath
  when (isJust _torrentTrackers       ) $
    setTrackers ps =<< fromList =<< (mapM textToStdString $ fromJust _torrentTrackers)
  when (isJust _urlSeeds       ) $ do
    setUrlSeeds ps =<< fromList =<< (mapM textToStdString $ fromJust _urlSeeds)
  when (isJust _resumeData     ) $ setResumeData     ps $ fromJust _resumeData
  when (isJust _storageMode    ) $ setStorageMode    ps $ fromJust _storageMode
  when (isJust _torrentFilePriorities ) $ setFilePriorities ps $ fromJust _torrentFilePriorities
  when (isJust _trackerid      ) $ setTrackerid      ps $ fromJust _trackerid
  when (isJust _url            ) $ setUrl            ps $ fromJust _url
  when (isJust _uuid           ) $ setUuid           ps $ fromJust _uuid
  when (isJust _sourceFeedUrl  ) $ setSourceFeedUrl  ps $ fromJust _sourceFeedUrl
  when (isJust _torrentInfoHash       ) $ do
    sha1 <- newSha1Hash $ fromJust _torrentInfoHash
    when (isJust sha1) $
      setInfoHash ps $ fromJust sha1
  when (isJust _maxUploads     ) $ setMaxUploads     ps $ (fromIntegral $ fromJust _maxUploads    )
  when (isJust _maxConnections ) $ setMaxConnections ps $ (fromIntegral $ fromJust _maxConnections)
  when (isJust _uploadLimit    ) $ setUploadLimit    ps $ (fromIntegral $ fromJust _uploadLimit   )
  when (isJust _downloadLimit  ) $ setDownloadLimit  ps $ (fromIntegral $ fromJust _downloadLimit )

  return ps

addTorrentParamsToOpts :: MonadIO m => AddTorrentParams -> m AddTorrentOpts
addTorrentParamsToOpts ps =
  liftIO $ AddTorrentOpts
  <$> (Just <$> getFlags ps)
  <*> (Just <$> getTorrentName ps)
  <*> (Just <$> getTorrentSavePath ps)
  <*> (fmap Just $ mapM stdStringToText =<< toList =<< getTrackers ps)
  <*> (fmap Just $ mapM stdStringToText =<< toList =<< getUrlSeeds ps)
  <*> (Just <$> getResumeData     ps)
  <*> (Just <$> getStorageMode    ps)
  <*> (Just <$> getFilePriorities ps)
  <*> (Just <$> getTrackerid      ps)
  <*> (Just <$> getUrl            ps)
  <*> (Just <$> getUuid           ps)
  <*> (Just <$> getSourceFeedUrl  ps)
  <*> (fmap Just $ sha1HashToByteString =<< getInfoHash ps)
  <*> (Just . fromIntegral <$> getMaxUploads     ps)
  <*> (Just . fromIntegral <$> getMaxConnections ps)
  <*> (Just . fromIntegral <$> getUploadLimit    ps)
  <*> (Just . fromIntegral <$> getDownloadLimit  ps)
