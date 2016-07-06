{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 

module Network.Libtorrent.Vectors where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import           Network.Libtorrent.Inline
import           Network.Libtorrent.String (StdString)
import           Network.Libtorrent.TH (defineStdVector)


C.context libtorrentCtx

C.include "<string>"
C.include "<vector>"

C.verbatim "typedef std::vector<std::string> VectorString;"


C.using "namespace std"

$(defineStdVector "string" "VectorString" ''C'String ''C'VectorString ''StdString)

