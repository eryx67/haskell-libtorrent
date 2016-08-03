{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |

module Network.Libtorrent.Types where

import           Data.Bits          (setBit, testBit)
import           Data.List          (foldl')
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Ptr        (Ptr)
import           GHC.Exts           (IsList (..))
import           GHC.Generics       (Generic)

class Inlinable a where
  type CType a :: *

class FromPtr a where
  fromPtr :: IO (Ptr (CType a)) -> IO a

class ToPtr a where
  toPtr :: a -> IO (Ptr (CType a))

class WithPtr a where
  withPtr :: a -> (Ptr (CType a) -> IO b) -> IO b

-- | Represent list of 'Enum' as bits in 'Int'
newtype BitFlags a = BitFlags { unBitFlags :: Set a }
  deriving (Foldable, Generic)


deriving instance Eq a => Eq (BitFlags a)
deriving instance Ord a => Monoid (BitFlags a)
deriving instance (Read a, Ord a) => Read (BitFlags a)

instance Show a => Show (BitFlags a) where
  show (BitFlags as) = "BitFlags " ++ (show $ Set.toList as)

instance Ord a => IsList (BitFlags a) where
  type Item (BitFlags a) = a
  fromList = BitFlags .Set.fromList
  toList (BitFlags as) = Set.toList as

instance (Enum a, Ord a, Bounded a) => Enum (BitFlags a) where
  toEnum i =
    BitFlags . Set.fromList $ filter (testBit i . fromEnum) [minBound..]

  fromEnum (BitFlags as) =
    foldl' (\i a -> setBit i (fromEnum a)) 0 as

-- | Type to represent std::vector
newtype StdVector e = StdVector { unStdVector :: ForeignPtr (CType (StdVector e))}

-- | Type to represent std::dequeue
newtype StdDeque e = StdDeque { unStdDeque :: ForeignPtr (CType (StdDeque e))}

