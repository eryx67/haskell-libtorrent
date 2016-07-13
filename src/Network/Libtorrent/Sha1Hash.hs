{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module Network.Libtorrent.Sha1Hash (Sha1Hash
                           , unSha1Hash
                           , sha1HashSize
                           , newSha1Hash
                           , sha1HashToByteString
                           ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Foreign.ForeignPtr          (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array       (allocaArray)
import qualified Language.C.Inline           as C
import qualified Language.C.Inline.Cpp       as C
import qualified Language.C.Inline.Unsafe    as CU
import           System.IO.Unsafe            (unsafePerformIO)


import           Network.Libtorrent.Inline
import           Network.Libtorrent.Internal
import           Network.Libtorrent.String
import           Network.Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/sha1_hash.hpp>"

C.using "namespace std"
C.using "namespace libtorrent"

newtype Sha1Hash = Sha1Hash { unSha1Hash :: ForeignPtr (CType Sha1Hash) }

instance Show Sha1Hash where
  show sh = show . unsafePerformIO $
    withPtr sh $ \shPtr -> do
      s <- fromPtr [CU.exp| string * { new std::string($(sha1_hash * shPtr)->to_string()) } |]
      stdStringToByteString s

instance Inlinable Sha1Hash where
  type CType Sha1Hash = C'Sha1Hash

instance FromPtr Sha1Hash where
  fromPtr = objFromPtr Sha1Hash $ \ptr ->
    [CU.exp| void { delete $(sha1_hash * ptr); } |]

instance WithPtr Sha1Hash where
  withPtr (Sha1Hash ptr) =
    withForeignPtr ptr

sha1HashSize :: Int
sha1HashSize = 20

-- | Create 'Sha1Hash' from 'ByteString'. Bytestring must be 20 byte length.
newSha1Hash :: MonadIO m =>  ByteString -> m (Maybe Sha1Hash)
newSha1Hash bs
  | BS.length bs /= sha1HashSize =
    return Nothing
  | otherwise =
    liftIO $ Just <$> fromPtr [CU.exp| sha1_hash * { new sha1_hash($bs-ptr:bs) }|]

sha1HashToByteString :: MonadIO m => Sha1Hash -> m ByteString
sha1HashToByteString sh =
  liftIO . withPtr sh $ \shPtr -> do
  let shSize = fromIntegral sha1HashSize
  allocaArray sha1HashSize $ \cstr -> do
    [CU.block| void {
        for (int i = 0; i < $(int shSize); i++)
        $(char * cstr)[i] = (*$(sha1_hash * shPtr))[i];
       }
    |]
    BS.packCStringLen (cstr, sha1HashSize)
