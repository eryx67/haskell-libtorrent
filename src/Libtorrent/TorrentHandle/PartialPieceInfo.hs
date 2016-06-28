{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | <http://www.libtorrent.org/reference-Core.html#partial-piece-info partial_piece_info> structure for "Libtorrent"
module Libtorrent.TorrentHandle.PartialPieceInfo( PieceState(..)
                                                , PartialPieceInfo(..)
                                                , getPieceIndex
                                                , getBlocksInPiece
                                                , getFinished
                                                , getWriting
                                                , getRequested
                                                , getBlocks
                                                , getPieceState
                                                ) where

import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           Libtorrent.Inline
import           Libtorrent.Internal
import           Libtorrent.TorrentHandle.BlockInfo
import           Libtorrent.Types
import           Libtorrent.TH (defineStdVector)

C.context libtorrentCtx

C.include "<libtorrent/torrent_handle.hpp>"

C.include "torrent_handle.hpp"

C.using "namespace libtorrent"
C.using "namespace std"

C.verbatim "typedef std::vector<libtorrent::block_info> VectorBlockInfo;"

$(defineStdVector "block_info" "VectorBlockInfo" ''C'BlockInfo ''C'VectorBlockInfo ''BlockInfo)

data PieceState =
  PieceNone
  | PieceSlow
  | PieceMedium
  | PieceFast
  deriving (Show, Enum, Bounded)

newtype PartialPieceInfo = PartialPieceInfo { unPartialPieceInfo :: ForeignPtr (CType PartialPieceInfo)}

instance Show PartialPieceInfo where
  show _ = "PartialPieceInfo"

instance Inlinable PartialPieceInfo where
  type (CType PartialPieceInfo) = C'PartialPieceInfo

instance FromPtr PartialPieceInfo where
  fromPtr = objFromPtr PartialPieceInfo $ \ptr ->
    [CU.exp| void { delete $(partial_piece_info * ptr); } |]

instance WithPtr PartialPieceInfo where
  withPtr (PartialPieceInfo fptr) = withForeignPtr fptr

getPieceIndex :: PartialPieceInfo -> IO CInt
getPieceIndex ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(partial_piece_info * hoPtr)->piece_index } |]

getBlocksInPiece :: PartialPieceInfo -> IO CInt
getBlocksInPiece ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(partial_piece_info * hoPtr)->blocks_in_piece } |]

getFinished :: PartialPieceInfo -> IO CInt
getFinished ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(partial_piece_info * hoPtr)->finished } |]

getWriting :: PartialPieceInfo -> IO CInt
getWriting ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(partial_piece_info * hoPtr)->writing } |]

getRequested :: PartialPieceInfo -> IO CInt
getRequested ho =
  withPtr ho $ \hoPtr ->
  [CU.exp| int { $(partial_piece_info * hoPtr)->requested } |]

getBlocks :: PartialPieceInfo -> IO (StdVector BlockInfo)
getBlocks ho =
  withPtr ho $ \hoPtr ->
  fromPtr [CU.block| VectorBlockInfo * {
              block_info *bs = $(partial_piece_info * hoPtr)->blocks;
              int bn = $(partial_piece_info * hoPtr)->blocks_in_piece;
              return new std::vector<block_info>(bs, bs + bn);
             }
          |]

getPieceState :: PartialPieceInfo -> IO PieceState
getPieceState ho =
  withPtr ho $ \hoPtr ->
  toEnum . fromIntegral <$> [CU.exp| int { $(partial_piece_info * hoPtr)->piece_state } |]
