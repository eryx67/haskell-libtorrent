{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
-- |
module Main where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forM_, when)
import           Control.Monad.Loops (untilM_)
import           Data.Char           (isUpper, toLower)
import           Data.Default
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy.IO   as TL
import           Formatting
import           Options.Applicative
import           System.IO           (hFlush, stdout)

import           Network.Libtorrent

data Config = Config {
  _source :: !Text
  }
            deriving Show

instance Default Config where
  def = Config {
    _source         = ""
    }

main :: IO ()
main = do
  Config{..} <- execParser opts

  ses <- newSession
  listenOn ses (6881, 6891) Nothing

  infoSrc <- TorrentInfoSrc <$> newTorrentInfo _source
  atp <- newAddTorrentParams infoSrc
  setTorrentSavePath atp "."
  th <- addTorrent ses atp
  ts' <- torrentStatus th Nothing
  name <- getName ts'

  TL.putStrLn $ format ("starting " % stext) name

  flip untilM_ (torrentStatus th Nothing >>= getIsSeeding) $ do
    ts   <- torrentStatus th Nothing
    dr::Double <- fromIntegral <$> getDownloadRate ts
    ur::Double <- fromIntegral <$> getUploadRate ts
    np   <- getNumPeers ts
    st   <- getState ts
    pg   <- getProgress ts

    let out = format ((fixed 2) % "% complete (down: "
                      % (fixed 1)  % "kB/s up: "
                      % (fixed 1) % "kB/s peers: "
                      % int % ") " % string)
              (pg * 100) (dr / 1000) (ur / 1000) np (camelToSpaces $ show st)
    TL.putStrLn out

    alerts <- popAlerts ses >>= toList
    forM_ alerts $ \a -> do
      (BitFlags acs) <- alertCategory a
      when (ErrorNotification `elem` acs) $
        print a

    hFlush stdout
    threadDelay 1000000

  putStrLn "complete"
opts :: ParserInfo Config
opts = info (helper <*> optParser)
       ( fullDesc
         <> progDesc "Simple BitTorrent client"
         <> header "simple-client - simple bittorrent client")

optParser :: Parser Config
optParser = Config
            <$> (T.pack <$> (argument str
                             (metavar "TORRENT-INFO-FILE"
                              <> help "torrent file name")))

camelToSpaces :: String -> String
camelToSpaces "" = ""
camelToSpaces (c:rst) = toLower c:go rst
  where
    go [] = []
    go (x:rest)
      | isUpper x = ' ':toLower x:go rest
      | otherwise = x:go rest
