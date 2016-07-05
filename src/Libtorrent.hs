{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
-- | <http://www.libtorrent.org libtorrent> bindings for Haskell
module Libtorrent ( module Import
                  , version
                  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.C.String (peekCAString)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)

import           Libtorrent.Alert as Import
import           Libtorrent.Bencode as Import
import           Libtorrent.Bitfield as Import
import           Libtorrent.CreateTorrent as Import
import           Libtorrent.ErrorCode as Import
import           Libtorrent.Exceptions as Import
import           Libtorrent.Extensions as Import
import           Libtorrent.FileStorage as Import
import           Libtorrent.PeerInfo as Import
import           Libtorrent.PeerRequest as Import
import           Libtorrent.Rss as Import
import           Libtorrent.Session as Import
import           Libtorrent.Session.AddTorrentParams as Import
import           Libtorrent.Session.DhtSettings as Import
import           Libtorrent.Session.PeSettings as Import
import           Libtorrent.Session.ProxySettings as Import
import           Libtorrent.Session.SessionSettings as Import
import           Libtorrent.Session.SessionStatus as Import
import           Libtorrent.Sha1Hash as Import
import           Libtorrent.String as Import
import           Libtorrent.TorrentHandle as Import
import           Libtorrent.TorrentHandle.BlockInfo as Import
import           Libtorrent.TorrentHandle.PartialPieceInfo as Import
import           Libtorrent.TorrentHandle.TorrentStatus as Import
import           Libtorrent.TorrentInfo as Import
import           Libtorrent.TorrentInfo.AnnounceEntry as Import
import           Libtorrent.Types as Import
import           Libtorrent.Types.ArrayLike as Import
import           Libtorrent.Vectors ()

import           Libtorrent.Inline (libtorrentCtx)

C.context libtorrentCtx

C.include "<libtorrent/version.hpp>"
C.using "namespace libtorrent"

version :: Text
version =
  T.pack . unsafePerformIO $ [CU.exp| const char * { version() }|] >>= peekCAString
