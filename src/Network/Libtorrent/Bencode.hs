{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | 

module Network.Libtorrent.Bencode (Bencoded
                          , entryToBencoded
                          , bencodedData
                          ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr ( Ptr )
import           Foreign.Storable (peek)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


import           Network.Libtorrent.Inline


C.context libtorrentCtx

C.include "<libtorrent/bencode.hpp>"

C.verbatim "typedef std::vector<char> VectorChar;"

C.using "namespace libtorrent"
C.using "namespace std"

newtype Bencoded = Bencoded ByteString

entryToBencoded :: MonadIO m => Ptr C'BencodeEntry -> m Bencoded
entryToBencoded ePtr =
  liftIO $ bracket
  [CU.exp| VectorChar * { new std::vector<char>() } |]
  (\bufPtr -> [CU.exp| void { delete $(VectorChar * bufPtr)} |]) $
  \bufPtr ->
    alloca $ \clenPtr ->
    alloca $ \cstrPtr -> do
      [C.block| void {
          bencode(std::back_inserter(*$(VectorChar * bufPtr)), *$(entry * ePtr));
          *$(size_t * clenPtr) = $(VectorChar * bufPtr)->size();
          *$(char ** cstrPtr) = $(VectorChar * bufPtr)->data();
         }
      |]
      clen <- peek clenPtr
      cstr <- peek cstrPtr
      Bencoded <$> BS.packCStringLen (cstr, fromIntegral clen)

bencodedData :: Bencoded -> ByteString
bencodedData (Bencoded v) = v
