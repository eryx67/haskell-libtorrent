{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#proxy_settings proxy_settings> for "Libtorrent"

module Libtorrent.Session.ProxySettings (ProxySettings(..)
                                     , ProxyType(..)
                                     , newProxySettings
                                     , getProxyHostname
                                     , setProxyHostname
                                     , getProxyUsername
                                     , setProxyUsername
                                     , getProxyPassword
                                     , setProxyPassword
                                     , getProxyType
                                     , setProxyType
                                     , getProxyPort
                                     , setProxyPort
                                     , getProxyHostnames
                                     , setProxyHostnames
                                     , getProxyPeerConnections
                                     , setProxyPeerConnections
                                     ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text.Foreign as TF
import           Data.Word (Word16)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types
import           Libtorrent.String

C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data ProxyType =
  ProxyNone
  | ProxySocks4
  | ProxySocks5
  | ProxySocks5Pw
  | ProxyHttp
  | ProxyHttpPw
  | I2pProxy
  deriving (Show, Enum, Bounded, Eq)

newtype ProxySettings = ProxySettings { unProxySettings :: ForeignPtr (CType ProxySettings)}

instance Show ProxySettings where
  show _ = "ProxySettings"

instance Inlinable ProxySettings where
  type (CType ProxySettings) = C'ProxySettings

instance FromPtr ProxySettings where
  fromPtr = objFromPtr ProxySettings $ \ptr ->
    [CU.exp| void { delete $(proxy_settings * ptr); } |]

instance WithPtr ProxySettings where
  withPtr (ProxySettings fptr) = withForeignPtr fptr

newProxySettings :: MonadIO m =>  m ProxySettings
newProxySettings =
  liftIO $ fromPtr [CU.exp| proxy_settings * { new proxy_settings() }|]

getProxyHostname :: MonadIO m => ProxySettings -> m Text
getProxyHostname ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(proxy_settings * hoPtr)->hostname) } |]
  stdStringToText res

setProxyHostname :: MonadIO m => ProxySettings -> Text -> m ()
setProxyHostname ho val =
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(proxy_settings * hoPtr)->hostname = std::string($(const char * cstr), $(size_t clen))} |]

getProxyUsername :: MonadIO m => ProxySettings -> m Text
getProxyUsername ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(proxy_settings * hoPtr)->username) } |]
  stdStringToText res

setProxyUsername :: MonadIO m => ProxySettings -> Text -> m ()
setProxyUsername ho val = do
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    withPtr ho $ \hoPtr ->
      [CU.exp| void { $(proxy_settings * hoPtr)->username = std::string($(const char * cstr), $(size_t clen))} |]

getProxyPassword :: MonadIO m => ProxySettings -> m Text
getProxyPassword ho =
  liftIO . withPtr ho $ \hoPtr -> do
  res <- fromPtr [CU.exp| string * { new std::string($(proxy_settings * hoPtr)->password) } |]
  stdStringToText res

setProxyPassword :: MonadIO m => ProxySettings -> Text -> m ()
setProxyPassword ho val = do
  liftIO . TF.withCStringLen val $ \(cstr, len) -> do
    let clen = fromIntegral len
    liftIO . withPtr ho $ \hoPtr ->
      [CU.exp| void { $(proxy_settings * hoPtr)->password = std::string($(const char * cstr), $(size_t clen))} |]

getProxyType :: MonadIO m => ProxySettings -> m ProxyType
getProxyType ho =
  liftIO . withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| uint8_t { $(proxy_settings * hoPtr)->type } |]

setProxyType :: MonadIO m => ProxySettings -> ProxyType -> m ()
setProxyType ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(proxy_settings * hoPtr)->type = $(uint8_t val')} |]

getProxyPort :: MonadIO m =>  ProxySettings -> m Word16
getProxyPort ho =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| uint16_t { $(proxy_settings * hoPtr)->port } |]

setProxyPort :: MonadIO m =>  ProxySettings -> Word16 -> m ()
setProxyPort ho val =
  liftIO . withPtr ho $ \hoPtr ->
                 [CU.exp| void { $(proxy_settings * hoPtr)->port = $(uint16_t val)} |]

getProxyHostnames :: MonadIO m =>  ProxySettings -> m Bool
getProxyHostnames ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(proxy_settings * hoPtr)->proxy_hostnames } |]

setProxyHostnames :: MonadIO m =>  ProxySettings -> Bool -> m ()
setProxyHostnames ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(proxy_settings * hoPtr)->proxy_hostnames = $(bool val')} |]

getProxyPeerConnections :: MonadIO m => ProxySettings -> m Bool
getProxyPeerConnections ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(proxy_settings * hoPtr)->proxy_peer_connections } |]

setProxyPeerConnections :: MonadIO m => ProxySettings -> Bool -> m ()
setProxyPeerConnections ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(proxy_settings * hoPtr)->proxy_peer_connections = $(bool val')} |]
