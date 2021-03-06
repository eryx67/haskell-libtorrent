{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | <http://www.libtorrent.org/reference-Core.html#peer_request peer_request> structure for "Libtorrent"

module Network.Libtorrent.PeerRequest (PeerRequest
                              , unPeerRequest
                              , getPiece
                              , getStart
                              , getLength
                              ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/alert.hpp>"
C.include "<libtorrent/alert_types.hpp>"
C.include "<libtorrent/peer_request.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

newtype PeerRequest = PeerRequest { unPeerRequest :: ForeignPtr (CType PeerRequest)}

instance Show PeerRequest where
  show _ = "PeerRequest"

instance Inlinable PeerRequest where
  type (CType PeerRequest) = C'PeerRequest

instance FromPtr PeerRequest where
  fromPtr = objFromPtr PeerRequest $ \ptr ->
    [CU.exp| void { delete $(peer_request * ptr); } |]

instance WithPtr PeerRequest where
  withPtr (PeerRequest fptr) = withForeignPtr fptr

getPiece :: MonadIO m =>  PeerRequest -> m CInt
getPiece ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(peer_request * hoPtr)->piece } |]

getStart :: MonadIO m =>  PeerRequest -> m CInt
getStart ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(peer_request * hoPtr)->start } |]

getLength :: MonadIO m =>  PeerRequest -> m CInt
getLength ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(peer_request * hoPtr)->length } |]
