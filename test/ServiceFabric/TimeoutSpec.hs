{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module ServiceFabric.TimeoutSpec where

import ServiceFabric.Timeout
import Test.Hspec
import Test.QuickCheck
import Control.Concurrent

timeoutOptions :: TimeoutOptions
timeoutOptions = defaultTimeoutOptions { timeoutLimitMs = 10 }

spec :: Spec
spec = do
  describe "addTimeout" $ do
    it "Services taking too long should fail with a timeout" $ do
      property $ \request ->
        let service req         = (if req > 0 then return () else threadDelay (100 * 1000)) >> return (req + 100) 
            timeoutService      = addTimeout timeoutOptions service
            successCase         = (timeoutService request) `shouldReturn` (request + 100)
            failureCase         = (timeoutService request) `shouldThrow` (== TimeoutException(timeoutDescription timeoutOptions))
        in if request > (0 :: Int) then successCase else failureCase 