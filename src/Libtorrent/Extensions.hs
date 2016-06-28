{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | <http://www.libtorrent.org/reference-Plugins.html Plugins> plugins for "Libtorrent"

module Libtorrent.Extensions (utMetadataPlugin
                             , utPexPlugin
                             , smartBanPlugin
                             , ltTrackersPlugin
                             ) where

import           Foreign.Ptr ( Ptr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Inline
import           Libtorrent.Internal


C.context libtorrentCtx

C.include "<libtorrent/session.hpp>"
C.include "<libtorrent/extensions.hpp>"
C.include "<libtorrent/extensions/ut_metadata.hpp>"
C.include "<libtorrent/extensions/ut_pex.hpp>"
C.include "<libtorrent/extensions/smart_ban.hpp>"
C.include "<libtorrent/extensions/logger.hpp>"
C.include "<libtorrent/extensions/lt_trackers.hpp>"

C.include "extensions.hpp"


C.using "namespace std"
C.using "namespace libtorrent"


utMetadataPlugin :: IO (Ptr C'TorrentPlugin)
utMetadataPlugin =
  [CU.exp| TorrentPlugin * { &libtorrent::create_ut_metadata_plugin } |]

utPexPlugin :: IO (Ptr C'TorrentPlugin)
utPexPlugin =
  [CU.exp| TorrentPlugin * { &libtorrent::create_ut_pex_plugin } |]

smartBanPlugin :: IO (Ptr C'TorrentPlugin)
smartBanPlugin =
  [CU.exp| TorrentPlugin * { &libtorrent::create_smart_ban_plugin } |]

ltTrackersPlugin :: IO (Ptr C'TorrentPlugin)
ltTrackersPlugin =
  [CU.exp| TorrentPlugin * { &libtorrent::create_lt_trackers_plugin } |]

