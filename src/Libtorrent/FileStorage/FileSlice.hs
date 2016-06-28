{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}
module Libtorrent.FileStorage.FileSlice (FileSlice(..)
                                        , getFileIndex
                                        , getFileSliceOffset
                                        , getFileSliceSize
                                        ) where

import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.Types

C.context libtorrentCtx

C.include "<libtorrent/file_storage.hpp>"

C.using "namespace libtorrent"
C.using "namespace std"

newtype FileSlice = FileSlice { unFileSlice :: ForeignPtr (CType FileSlice)}

instance Show FileSlice where
  show _ = "FileSlice"

instance Inlinable FileSlice where
  type (CType FileSlice) = C'FileSlice

instance FromPtr FileSlice where
  fromPtr = objFromPtr FileSlice $ \ptr ->
    [CU.exp| void { delete $(file_slice * ptr); } |]

instance WithPtr FileSlice where
  withPtr (FileSlice fptr) = withForeignPtr fptr


getFileIndex :: FileSlice -> IO CInt
getFileIndex ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| int { $(file_slice * hoPtr)->file_index } |]

getFileSliceOffset :: FileSlice -> IO C.CSize
getFileSliceOffset ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| size_t { $(file_slice * hoPtr)->offset } |]

getFileSliceSize :: FileSlice -> IO C.CSize
getFileSliceSize ho =
  withPtr ho $ \hoPtr ->
                 [CU.exp| size_t { $(file_slice * hoPtr)->size } |]
