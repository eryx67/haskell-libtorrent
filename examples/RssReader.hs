{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
-- |
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad (forever, unless, forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Loops (whileM_)
import qualified Control.Monad.Trans.Writer as W
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Formatting
import           Options.Applicative
import           System.IO (hFlush, hPutStr, stdout)

import           Libtorrent

data Config = Config {
  _source         :: !Text
  }
            deriving Show

instance Default Config where
  def = Config {
    _source = ""
    }

main :: IO ()
main = do
  Config{..} <- execParser opts

  ses <- newSession
  fsets <- newFeedSettings
  setFeedSettingsUrl fsets _source

  fh <- addFeed ses fsets
  liftIO $ race_
    (forever $ spinner 100000)
    (whileM_ (checkFeed fh) $ threadDelay 100000)

  txt <- W.execWriterT $ do
    fs <- getFeedStatus fh
    url <- getFeedStatusUrl fs
    ec <- getFeedStatusError fs
    title <- getFeedStatusTitle fs
    desc  <- getFeedStatusDescription fs
    ttl  <- getFeedStatusTtl fs
    ecv <- errorCodeValue ec
    items <- getFeedStatusItems fs >>= liftIO . toList

    outln $ format ("\n\nFEED:" % stext) url
    unless (ecv == 0) $ do
      outln $ format ("Error:" % shown) ec
    outln $ format ("   " % stext) title
    outln $ format ("   " % stext) desc
    outln $ format ("   ttl: " % int % " minutes") ttl

    forM_ items $ \item -> do
      ititle <- getFeedItemTitle item
      iurl <- getFeedItemUrl item
      isize <- getFeedItemSize item
      iuuid <- getFeedItemUuid item
      idesc <- getFeedItemDescription item
      icom <- getFeedItemComment item
      icat <- getFeedItemCategory item

      outln $ format (stext % "\n------------------------------------------------------") ititle
      outln $ format ("   url: " % stext % "\n   size: " % int %
                      "\n   uuid: " % stext % "\n   description: " % stext)
        iurl  isize iuuid idesc
      outln $ format ("   comment: " % stext % "\n   category: " % stext)
        icom icat
  TL.putStrLn txt
  hFlush stdout
  where
    opts = info (helper <*> optParser)
      ( fullDesc
        <> progDesc "Torrent RSS feed reader"
        <> header "rss-reader - torrent rss feed url reader")
    checkFeed fh = do
      fs <- getFeedStatus fh
      getFeedStatusUpdating fs
    spinner tm =
      go ['|', '/', '-', '\\']
      where
        go [] = pure ()
        go (c:rst) = do
          hPutStr stdout ['\b', c]
          hFlush stdout
          threadDelay tm
          go rst

optParser :: Parser Config
optParser = Config
            <$> (T.pack <$> (argument str
                              (metavar "RSS-FEED-URL"
                                <> help "RSS feed url")))

outln :: MonadIO m => TL.Text -> W.WriterT TL.Text m ()
outln txt = W.tell txt >> W.tell "\n"
