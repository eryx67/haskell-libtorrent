{-# LANGUAGE OverloadedStrings #-}
-- | 
module LibtorrentSpec (spec) where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Exception (finally)
import           Control.Monad (forever)
import           Data.List ((\\), union)
import           Foreign.Ptr ( freeHaskellFunPtr )
import           Test.Hspec

import           Libtorrent

spec :: Spec
spec = do
  describe "example 1" $ do
    it "create session" $ do
      ti <- newTorrentInfo "test-data/debian-8.5.0-amd64-CD-1.iso.torrent"
      atp <- newAddTorrentParams $ TorrentInfoSrc ti
      setSavePath atp "test-data"
      (BitFlags atpFlags) <- getFlags atp
      setFlags atp  . BitFlags . union [AutoManaged, DuplicateIsError] $ atpFlags \\ [Paused]

      ses <- newSession
      setAlertMask ses $ BitFlags [minBound..]
      cbPtr <- A.async (setAlertDispatch ses print) >>= A.wait
      (`finally` freeHaskellFunPtr cbPtr)  $ do
        startDht ses
        th <- addTorrent ses atp
        forever $ checkAlerts ses
        return th >>= (`shouldSatisfy` const True)
  where
    checkAlerts ses = do
      -- as <- popAlerts ses
      -- asl <- toList as
      -- putStrLn $ show asl
      print '.'
      threadDelay 1000000
