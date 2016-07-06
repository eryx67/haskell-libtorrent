{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Network.Libtorrent.Rss.FeedHandle (FeedHandle(..)
                                 ) where

import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/rss.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

newtype FeedHandle = FeedHandle { unFeedHandle :: ForeignPtr (CType FeedHandle)}

instance Show FeedHandle where
  show _ = "FeedHandle"

instance Inlinable FeedHandle where
  type (CType FeedHandle) = C'FeedHandle

instance FromPtr FeedHandle where
  fromPtr = objFromPtr FeedHandle $ \ptr ->
    [CU.exp| void { delete $(feed_handle * ptr) } |]

instance WithPtr FeedHandle where
  withPtr (FeedHandle fptr) = withForeignPtr fptr
