{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Network.Libtorrent.Types.ArrayLike where

import           Control.Monad   (forM_)
import           Foreign.C.Types (CSize)
import           Prelude         hiding (foldMap)

class ArrayLike a where
  type ElemType a :: *
  getElem :: a -> CSize -> IO (Maybe (ElemType a))

foldMap :: (ArrayLike a, Monoid m) => (ElemType a -> m) -> a -> IO m
foldMap f ar =
    go 0 mempty
    where
      go ix acc = do
        !el <- getElem ar ix
        case el of
          Nothing -> return acc
          Just v ->  go (succ ix) $! mappend (f v) acc

fold :: ArrayLike a => (ElemType a -> b -> b) -> b -> a -> IO b
fold f acc ar =
  go 0 acc
  where
    go ix acc' = do
      !el <- getElem ar ix
      case el of
        Nothing -> return acc'
        Just v ->  go (succ ix) $! f v acc'

toList :: ArrayLike a => a -> IO [ElemType a]
toList ar =
  foldMap pure ar

class ArrayLike a => VectorLike a where
  newVector :: IO a
  addElem :: a -> ElemType a -> IO ()

fromList :: VectorLike a => [ElemType a] -> IO a
fromList as = do
  v <- newVector
  forM_ as $ addElem v
  return v
