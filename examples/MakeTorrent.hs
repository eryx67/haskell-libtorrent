{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
-- |
module Main where

import           Control.Monad (when, forM_, forM, zipWithM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.Default
import           Data.List (isPrefixOf)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Formatting
import           Options.Applicative
import           System.Directory (makeAbsolute)
import           System.EasyFile
import           System.IO (hFlush, stdout)

import           Libtorrent hiding (files)

data Config = Config {
  _file         :: !Text
  , _trackerUrl :: !Text
  }
            deriving Show

instance Default Config where
  def = Config {
    _file         = ""
    , _trackerUrl = ""
    }

main :: IO ()
main = do
  Config{..} <- execParser opts

  absFile <- makeAbsolute (T.unpack _file)
  absFiles <- walkDir absFile
  fileSizes <- forM absFiles getFileSize
  let parent = takeDirectory $ dropTrailingPathSeparator absFile
      parentLen = length $ splitDirectories parent
      files = map (\f -> joinPath . drop parentLen $ splitDirectories f) absFiles

  when (length files == 0) $
    error "No files added"

  forM_ (zip files fileSizes) $ \(fn, fs) ->
    putStrLn . TL.unpack $
    format (((left 10 ' ') %. int) % "kB " % string)
    (fs `div` 1024) fn

  fs <- newFileStorage
  zipWithM_ (\fn sz -> addFile fs fn sz (BitFlags []) Nothing Nothing)
    (map T.pack files) (map fromIntegral fileSizes)

  ct <- createTorrent fs Nothing (Just $ 4 * 1024 * 1024) Nothing
  createTorrentAddTracker ct _trackerUrl Nothing
  createTorrentSetCreator ct $ T.append "libtorrent " version

  createTorrentSetPieceHashes ct (T.pack parent) (\_ -> putChar '.' >> hFlush stdout)
  putStrLn "\ndone"
  res <- bencodedData <$> createTorrentGenerate ct

  liftIO $ BS.writeFile "out.torrent" res
  where
    opts = info (helper <*> optParser)
      ( fullDesc
        <> progDesc "BitTorrent file maker"
        <> header "make-torrent - create torrent file from file or directory")
    walkDir fp = do
      isDir <- doesDirectoryExist fp
      if not isDir
        then return [fp]
        else do
        children <- filter (not . isPrefixOf ".") <$> getDirectoryContents fp
        map (fp </>) . concat <$> forM children walkDir

optParser :: Parser Config
optParser = Config
            <$> (T.pack <$> (argument str
                              (metavar "DIRECTORY-OR-FILE"
                                <> help "Directory or file to create torrent from")))
            <*> (T.pack <$> (argument str
                              (metavar "TRACKER-URL"
                                <> help "Tracker url to publish this torrent to")))
