### Libtorrent

[libtorrent](http://www.libtorrent.org) bindings for Haskell.

### Examples

In *examples* directory there are translations of original Python examples.

These bindings are mostly one to one translation.
[Example](http://www.libtorrent.org/reference-Core.html#torrent_handle) of saving resume data from official documentation could look as:

```haskell
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
      st <- torrentStatus th . Just $ BitFlags [QuerySavePath, QueryName]
      rd <- bencodedData <$> saveResumeDataAlertResumeData a
      sp <- getSavePath st
      tn <- getName st
      let rfn = resumeFileName sp tn
      liftIO $ BS.writeFile rfn rd
      $(logInfo) $ sformat ("resume data saved for " % stext % " to " % stext) tn sp
    saveResumeFailed a = do
      th <- getHandle a
      st <- torrentStatus th . Just $ BitFlags [QuerySavePath, QueryName]
      tn <- getName st
      ec <- saveResumeDataFailedAlertError a
      $(logError) $ sformat ("resume data save failed for " % stext % " reason " % shown) tn ec
```

### Documentation

For usage see examples in *examples* directory.
For documentation see haddock generated documentation.

### Maintainer <eryx67@gmail.com>
