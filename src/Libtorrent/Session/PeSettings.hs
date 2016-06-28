{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
-- | <http://www.libtorrent.org/reference-Core.html#pe_settings pe_settings> for "Libtorrent"

module Libtorrent.Session.PeSettings (PeSettings
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

import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils (toBool, fromBool)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/session_settings.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

data PeEncPolicy =
  EncPolicyForced
  | EncPolicyEnabled
  | EncPolicyDisabled
  deriving (Show, Enum, Bounded)

data PeEncLevel =
  EncLevelNone
  | EncLevelPlaintext
  | EncLevelRc4
  | EncLevelBoth
  deriving (Show, Enum, Bounded)

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

newPeSettings :: IO PeSettings
newPeSettings =
  fromPtr [CU.exp| pe_settings * { new pe_settings() }|]

getOutEncPolicy :: PeSettings -> IO PeEncPolicy
getOutEncPolicy ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->out_enc_policy } |]

setOutEncPolicy :: PeSettings -> PeEncPolicy -> IO ()
setOutEncPolicy ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->out_enc_policy = $(uint8_t val')} |]

getInEncPolicy :: PeSettings -> IO PeEncPolicy
getInEncPolicy ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->in_enc_policy } |]

setInEncPolicy :: PeSettings -> PeEncPolicy -> IO ()
setInEncPolicy ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->in_enc_policy = $(uint8_t val')} |]

getAllowedEncLevel :: PeSettings -> IO PeEncLevel
getAllowedEncLevel ho =
  withPtr ho $ \hoPtr ->
  (toEnum . fromIntegral) <$> [CU.exp| uint8_t { $(pe_settings * hoPtr)->allowed_enc_level } |]

setAllowedEncLevel :: PeSettings -> PeEncLevel -> IO ()
setAllowedEncLevel ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromIntegral $ fromEnum val
  [CU.exp| void { $(pe_settings * hoPtr)->allowed_enc_level = $(uint8_t val')} |]

getPreferRc4 :: PeSettings -> IO Bool
getPreferRc4 ho =
  withPtr ho $ \hoPtr ->
  toBool <$> [CU.exp| bool { $(pe_settings * hoPtr)->prefer_rc4 } |]

setPreferRc4 :: PeSettings -> Bool -> IO ()
setPreferRc4 ho val =
  withPtr ho $ \hoPtr -> do
  let val' = fromBool val
  [CU.exp| void { $(pe_settings * hoPtr)->prefer_rc4 = $(bool val')} |]
