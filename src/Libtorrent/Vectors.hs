{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 

module Libtorrent.Vectors where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import           Libtorrent.Inline
import           Libtorrent.String (StdString)
import           Libtorrent.TH (defineStdVector)


C.context libtorrentCtx

C.include "<string>"
C.include "<vector>"

C.verbatim "typedef std::vector<std::string> VectorString;"


C.using "namespace std"

$(defineStdVector "string" "VectorString" ''C'String ''C'VectorString ''StdString)

