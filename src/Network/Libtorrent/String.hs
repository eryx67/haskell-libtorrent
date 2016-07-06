{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
-- | 

module Network.Libtorrent.String (StdString
                         , unStdString
                         , stdStringToText
                         , textToStdString
                         , stdStringToByteString
                         , byteStringToStdString
                         ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<string>"
C.include "<vector>"

C.using "namespace std"

newtype StdString = StdString { unStdString :: ForeignPtr (CType StdString) }

instance Show StdString where
  show = T.unpack . unsafePerformIO . stdStringToText

instance Inlinable StdString where
  type CType StdString = C'String

instance FromPtr StdString where
  fromPtr = objFromPtr StdString $ \ptr ->
    [CU.exp| void { delete $(string * ptr); } |]

instance WithPtr StdString where
  withPtr (StdString ptr) =
    withForeignPtr ptr

stdStringToText :: StdString -> IO Text
stdStringToText s =
  withPtr s $ \ptr -> do
    cstr <- [CU.exp| const char * {$(string * ptr)->c_str()} |]
    len <- [CU.exp| int {$(string * ptr)->length()} |]
    TF.peekCStringLen (cstr, fromIntegral len)

textToStdString :: Text -> IO StdString
textToStdString s =
  TF.withCStringLen s $ \(cstr, len) -> do
    let clen = fromIntegral len
    fromPtr [CU.exp| string * { new std::string($(const char * cstr), $(size_t clen))} |]

stdStringToByteString :: StdString -> IO ByteString
stdStringToByteString s =
  withPtr s $ \ptr -> do
    cstr <- [CU.exp| const char * {$(string * ptr)->c_str()} |]
    len <- [CU.exp| int {$(string * ptr)->length()} |]
    BS.packCStringLen (cstr, fromIntegral len)

byteStringToStdString :: ByteString -> IO StdString
byteStringToStdString s =
  fromPtr [CU.exp| string * { new std::string($bs-ptr:s, $bs-len:s) } |]
