{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | <http://www.libtorrent.org/reference-Plugins.html Plugins> plugins for "Libtorrent"

module Network.Libtorrent.Extensions (utMetadataPlugin
                             , utPexPlugin
                             , smartBanPlugin
                             , ltTrackersPlugin
                             ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.Ptr ( Ptr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Network.Libtorrent.Inline


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


utMetadataPlugin :: MonadIO m => m (Ptr C'TorrentPlugin)
utMetadataPlugin =
  liftIO [CU.exp| TorrentPlugin * { &libtorrent::create_ut_metadata_plugin } |]

utPexPlugin :: MonadIO m => m (Ptr C'TorrentPlugin)
utPexPlugin =
  liftIO [CU.exp| TorrentPlugin * { &libtorrent::create_ut_pex_plugin } |]

smartBanPlugin :: MonadIO m => m (Ptr C'TorrentPlugin)
smartBanPlugin =
  liftIO [CU.exp| TorrentPlugin * { &libtorrent::create_smart_ban_plugin } |]

ltTrackersPlugin :: MonadIO m => m (Ptr C'TorrentPlugin)
ltTrackersPlugin =
  liftIO [CU.exp| TorrentPlugin * { &libtorrent::create_lt_trackers_plugin } |]

