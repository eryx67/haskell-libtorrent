{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | <http://www.libtorrent.org/reference-Core.html#pe_settings pe_settings> for "Libtorrent"

module Network.Libtorrent.Session.PeSettings (PeSettings
                                             , PeEncPolicy(..)
                                             , PeEncLevel(..)
                                             , unPeSettings
                                             , newPeSettings
                                             , getOutEncPolicy
                                             , setOutEncPolicy
                                             , getInEncPolicy
                                             , setInEncPolicy
                                             , getAllowedEncLevel
                                             , setAllowedEncLevel
                                             , getPreferRc4
                                             , setPreferRc4
                                             ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Foreign.ForeignPtr          (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils       (fromBool, toBool)
import           GHC.Generics                (Generic)
import qualified Language.C.Inline           as C
import qualified Language.C.Inline.Cpp       as C
import qualified Language.C.Inline.Unsafe    as CU

import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data PeEncPolicy =
  EncPolicyForced
  | EncPolicyEnabled
  | EncPolicyDisabled
  deriving (Show, Enum, Bounded, Eq, Generic)

data PeEncLevel =
  EncLevelNone
  | EncLevelPlaintext
  | EncLevelRc4
  | EncLevelBoth
  deriving (Show, Enum, Bounded, Eq, Generic)

newtype PeSettings = PeSettings { unPeSettings :: ForeignPtr (CType PeSettings)}

instance Show PeSettings where
  show _ = "PeSettings"

instance Inlinable PeSettings where
  type (CType PeSettings) = C'PeSettings

instance FromPtr PeSettings where
  fromPtr = objFromPtr PeSettings $ \ptr ->
    [CU.exp| void { delete $(pe_settings * ptr); } |]

instance WithPtr PeSettings where
  withPtr (PeSettings fptr) = withForeignPtr fptr

newPeSettings :: MonadIO m =>  m PeSettings
newPeSettings =
  liftIO $ fromPtr [CU.exp| pe_settings * { new pe_settings() }|]

getOutEncPolicy :: MonadIO m =>  PeSettings -> m PeEncPolicy
getOutEncPolicy ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->out_enc_policy } |]

setOutEncPolicy :: MonadIO m =>  PeSettings -> PeEncPolicy -> m ()
setOutEncPolicy ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->out_enc_policy = $(uint8_t val')} |]

getInEncPolicy :: MonadIO m =>  PeSettings -> m PeEncPolicy
getInEncPolicy ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->in_enc_policy } |]

setInEncPolicy :: MonadIO m =>  PeSettings -> PeEncPolicy -> m ()
setInEncPolicy ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->in_enc_policy = $(uint8_t val')} |]

getAllowedEncLevel :: MonadIO m =>  PeSettings -> m PeEncLevel
getAllowedEncLevel ho =
  liftIO . withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->allowed_enc_level } |]

setAllowedEncLevel :: MonadIO m =>  PeSettings -> PeEncLevel -> m ()
setAllowedEncLevel ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->allowed_enc_level = $(uint8_t val')} |]

getPreferRc4 :: MonadIO m =>  PeSettings -> m Bool
getPreferRc4 ho =
  liftIO . withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(pe_settings * hoPtr)->prefer_rc4 } |]

setPreferRc4 :: MonadIO m =>  PeSettings -> Bool -> m ()
setPreferRc4 ho val =
  liftIO . withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(pe_settings * hoPtr)->prefer_rc4 = $(bool val')} |]
