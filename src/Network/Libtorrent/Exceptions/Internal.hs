{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Error handling helpers

module Network.Libtorrent.Exceptions.Internal where

import           Data.Monoid ((<>))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           Language.Haskell.TH.Quote ( QuasiQuoter, quoteExp )


import           Network.Libtorrent.Inline


C.context libtorrentCtx

C.include "<libtorrent/error_code.hpp>"

C.using "namespace libtorrent"

except :: QuasiQuoter
except = C.block {quoteExp = \s -> quoteExp C.block $ wrapLibtorrentExcept s}

exceptU :: QuasiQuoter
exceptU = CU.block {quoteExp = \s -> quoteExp CU.block $ wrapLibtorrentExcept s}

wrapLibtorrentExcept :: String -> String
wrapLibtorrentExcept s =
    "error_code * {\n\
    \  try\n\
    \  {\n   " <> s <> "\n\
    \    return NULL;\n\
    \  }\n\
    \  catch (const libtorrent_exception & le)\n\
    \  {\n\
    \    return new error_code(le.error());\n\
    \  }\n\
    \}"
