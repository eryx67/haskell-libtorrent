-- | 
module Libtorrent.SessionSpec (spec) where

import Test.Hspec
import Libtorrent.Session

spec :: Spec
spec = do
  describe "Session" $ do
    it "create new session" $ do
      newSession >>= (`shouldSatisfy` const True)
