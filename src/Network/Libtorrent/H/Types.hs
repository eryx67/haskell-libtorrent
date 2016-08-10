-- |

module Network.Libtorrent.H.Types where

class MergeableOpts o where
  mergeOpts :: o -> o -> o

