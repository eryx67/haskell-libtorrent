{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | <http://www.libtorrent.org/reference-Utility.html#bitfield bitfield> structure for "Libtorrent"

module Libtorrent.Bitfield where

import           Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as BA
import qualified Data.Array.BitArray.ByteString as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)


import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types


C.context libtorrentCtx

C.include "<libtorrent/bitfield.hpp>"

C.using "namespace libtorrent"

newtype Bitfield = Bitfield { unBitfield :: ForeignPtr (CType Bitfield) }

instance Show Bitfield where
  show bf = show . BA.elems $ unsafePerformIO (bitfieldToBitArray bf)

instance Inlinable Bitfield where
  type CType Bitfield = C'Bitfield

instance FromPtr Bitfield where
  fromPtr = objFromPtr Bitfield $ \ptr ->
    [CU.exp| void { delete $(bitfield * ptr); } |]

instance WithPtr Bitfield where
  withPtr (Bitfield ptr) =
    withForeignPtr ptr

bitfieldToBitArray :: Bitfield -> IO (BitArray Int)
bitfieldToBitArray bf =
  withPtr bf $ \ptr -> do
    cstr <- [CU.exp| const char * {$(bitfield * ptr)->bytes()} |]
    blen <- [CU.exp| int {$(bitfield * ptr)->size()} |]
    let clen = fromIntegral $ (blen + 8) `quot` 8
    BA.fromByteString (0, fromIntegral $ blen - 1) <$>
      BS.packCStringLen (cstr, clen)

bitArrayToBitfield :: (BitArray Int) -> IO Bitfield
bitArrayToBitfield ba = do
  let bs = BA.toByteString ba
      (l, u) = BA.bounds ba
  BSU.unsafeUseAsCString bs $ \cstr -> do
    let blen = fromIntegral $ u - l
    fromPtr [CU.exp| bitfield * { new bitfield($(const char * cstr), $(int blen))} |]

