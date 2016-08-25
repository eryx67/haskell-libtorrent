{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- |

module Network.Libtorrent.Types where

import           Data.Aeson
import           Data.Bits              (setBit, testBit)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8  as BSC
import           Data.Function          ((&))
import           Data.List              (foldl')
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import           Foreign.ForeignPtr     (ForeignPtr)
import           Foreign.Ptr            (Ptr)
import           GHC.Exts               (IsList (..), fromList, toList)
import           GHC.Generics           (Generic)

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

instance (Ord a, ToJSON a) => ToJSON (BitFlags a) where
  toJSON (BitFlags as) = toJSON $ toList as

instance (Ord a, FromJSON a) => FromJSON (BitFlags a) where
   parseJSON v =
     BitFlags . fromList <$> parseJSON v

-- | Type to represent std::vector
newtype StdVector e = StdVector { unStdVector :: ForeignPtr (CType (StdVector e))}

-- | Type to represent std::dequeue
newtype StdDeque e = StdDeque { unStdDeque :: ForeignPtr (CType (StdDeque e))}

-- | 20-bytes torrent infohash
newtype InfoHash = InfoHash { unInfoHash :: ByteString }
                   deriving (Eq, Ord)

instance Show InfoHash where
  show (InfoHash ih) = "InfoHash " ++ (BSC.unpack $ BS16.encode ih)

instance  ToJSON InfoHash where
  toJSON = toJSON . infoHashToText

instance FromJSON InfoHash where
   parseJSON = withText "InfoHash" $ \txt ->
     maybe mempty pure . newInfoHash $ TE.encodeUtf8 txt

-- | Create 'InfoHash' from byte string or hex-encoded byte string
newInfoHash :: ByteString -> Maybe InfoHash
newInfoHash bs
  | BS.length bs == 20 = Just $ InfoHash bs
  | BS.length bs == 40 = BS16.decode bs & \case
      (ih, "") -> Just $ InfoHash ih
      _ -> Nothing
  | otherwise =
    Nothing
-- | Unpack 'InfoHash' to 'ByteString'
infoHashToByteString :: InfoHash -> ByteString
infoHashToByteString = unInfoHash

-- | Unpack 'InfoHash' to hex-encoded 'Text'
infoHashToText :: InfoHash -> Text
infoHashToText =
  TE.decodeUtf8 . BS16.encode . unInfoHash
