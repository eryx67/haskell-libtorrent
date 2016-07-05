{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | <http://www.libtorrent.org/reference-Core.html#block-info block_info> structure for "Libtorrent"
module Libtorrent.TorrentHandle.BlockInfo( BlockState(..)
                                         , BlockInfo(..)
                                         , getBytesProgress
                                         , getBlockInfoBlockSize
                                         , getBlockInfoNumPeers
                                         , getBlockInfoState
                                         ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/torrent_handle.hpp>"
C.include "<boost/asio.hpp>"

C.include "torrent_handle.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

data BlockState =
  BlockNone
  | BlockRequested
  | BlockWriting
  | BlockFinished
  deriving (Show, Enum, Bounded, Eq)

newtype BlockInfo = BlockInfo { unBlockInfo :: ForeignPtr (CType BlockInfo)}

instance Show BlockInfo where
  show _ = "BlockInfo"

instance Inlinable BlockInfo where
  type (CType BlockInfo) = C'BlockInfo

instance FromPtr BlockInfo where
  fromPtr = objFromPtr BlockInfo $ \ptr ->
    [CU.exp| void { delete $(block_info * ptr); } |]

instance WithPtr BlockInfo where
  withPtr (BlockInfo fptr) = withForeignPtr fptr

-- FIXME: get linking error, don't know why
-- getPeer :: MonadIO m =>  BlockInfo -> m (Text, C.CShort)
-- getPeer ho =
--   liftIO . withPtr ho $ \hoPtr -> do
--   addr <- fromPtr [CU.block| string * {
--                       tcp::endpoint ep = $(block_info * hoPtr)->peer();
--                       return new std::string(ep.address().to_string());
--                     }
--                   |]
--   port <- [CU.block| short {
--                       tcp::endpoint ep = $(block_info * hoPtr)->peer();
--                       return ep.port();
--                     }
--                   |]
--   ( , port) <$> stdStringToText addr

getBytesProgress :: MonadIO m =>  BlockInfo -> m CInt
getBytesProgress ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(block_info * hoPtr)->bytes_progress } |]

getBlockInfoBlockSize :: MonadIO m =>  BlockInfo -> m CInt
getBlockInfoBlockSize ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(block_info * hoPtr)->block_size } |]

getBlockInfoNumPeers :: MonadIO m =>  BlockInfo -> m CInt
getBlockInfoNumPeers ho =
  liftIO . withPtr ho $ \hoPtr ->
  [CU.exp| int { $(block_info * hoPtr)->num_peers } |]

getBlockInfoState :: MonadIO m =>  BlockInfo -> m BlockState
getBlockInfoState ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(block_info * hoPtr)->state } |]
