{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Libtorrent.TorrentInfoSpec where

import Test.Hspec

import Libtorrent.Exceptions
import Libtorrent.TorrentInfo

spec :: Spec
spec = do
  describe "Torrent info" $ do
    it "read from nonexistent file" $
      newTorrentInfo "aaa" `shouldThrow` (\case
                                             TorrentInfoError _ -> True
                                             _ -> False)
    it "read from existent file" $
      newTorrentInfo "test-data/debian-8.5.0-amd64-CD-1.iso.torrent" >>=
      (`shouldSatisfy` const True)


