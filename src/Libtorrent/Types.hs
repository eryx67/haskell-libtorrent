{-# LANGUAGE TypeFamilies #-}
-- | 

module Libtorrent.Types where

import Data.Bits (testBit, setBit)
import Data.List (foldl')
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr )


class Inlinable a where
  type CType a :: *

class FromPtr a where
  fromPtr :: IO (Ptr (CType a)) -> IO a

class ToPtr a where
  toPtr :: a -> IO (Ptr (CType a))

class WithPtr a where
  withPtr :: a -> (Ptr (CType a) -> IO b) -> IO b

-- | Represent list of 'Enum' as bits in 'Int'
newtype BitFlags a = BitFlags { unBitFlags :: [a] }

instance (Enum a, Bounded a) => Enum (BitFlags a) where
  toEnum i =
    BitFlags $ filter (testBit i . fromEnum) [minBound..]

  fromEnum (BitFlags as) =
    foldl' (\i a -> setBit i (fromEnum a)) 0 as

instance Show a => Show (BitFlags a) where
  show (BitFlags as) = "BitFlags " ++ show as

-- | Type to represent std::vector
newtype StdVector e = StdVector { unStdVector :: ForeignPtr (CType (StdVector e))}

-- | Type to represent std::dequeue
newtype StdDeque e = StdDeque { unStdDeque :: ForeignPtr (CType (StdDeque e))}

