{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Libtorrent.Rss.FeedItem (FeedItem(..)
                      ) where


import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/rss.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

newtype FeedItem = FeedItem { unFeedItem :: ForeignPtr (CType FeedItem)}

instance Show FeedItem where
  show _ = "FeedItem"

instance Inlinable FeedItem where
  type (CType FeedItem) = C'FeedItem

instance FromPtr FeedItem where
  fromPtr = objFromPtr FeedItem $ \ptr ->
    [CU.exp| void { delete $(feed_item * ptr) } |]

instance WithPtr FeedItem where
  withPtr (FeedItem fptr) = withForeignPtr fptr
