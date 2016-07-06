-- | 

module Network.Libtorrent.Internal where

import Control.Exception ( mask_ )
import Foreign.Concurrent ( newForeignPtr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_ )
import Foreign.Ptr ( Ptr )

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr toHask finalizer fromC = mask_ $ do
    objPtr <- fromC
    toHask <$> newForeignPtr objPtr (finalizer objPtr)

objFromPtr_ :: (ForeignPtr c -> hask) -> IO (Ptr c) -> IO hask
objFromPtr_ toHask fromC = mask_ $ do
    objPtr <- fromC
    toHask <$> newForeignPtr_ objPtr
