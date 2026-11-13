{-# LANGUAGE ScopedTypeVariables #-}

module Glue.TimeoutSpec (spec) where

import           Control.Concurrent.Async (async, wait)
import           Glue.Clock
import           Glue.Clock.Virtual
import           Glue.Timeout
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "addTimeout" $ do
    it "Services taking too long should fail with a timeout" $
      property $ \(request :: Int) -> do
        vc <- newVirtualClock
        let clock          = virtualClock vc :: Clock IO
            options        = (defaultTimeoutOptions :: TimeoutOptions IO) { timeoutLimitMs = 10, timeoutClock = clock }
            service req    = (if req > 0 then pure () else clockSleep clock (50 * 1000)) >> pure (req + 100)
            timeoutService = addTimeout options service
        if request > 0
          then timeoutService request `shouldReturn` (request + 100)
          else do
            call <- async (timeoutService request)
            waitForSleepers vc 2
            advanceClock vc (10 * 1000)
            wait call `shouldThrow` (== TimeoutException (timeoutDescription options))

    it "Does not time out a call that finishes just inside the limit" $ do
      vc <- newVirtualClock
      let clock   = virtualClock vc :: Clock IO
          options = (defaultTimeoutOptions :: TimeoutOptions IO) { timeoutLimitMs = 10, timeoutClock = clock }
      call <- async (addTimeout options (\(x :: Int) -> clockSleep clock 9999 >> pure x) 7)
      waitForSleepers vc 2
      advanceClock vc 9999
      wait call `shouldReturn` 7

  describe "addTimeoutEither" $
    it "Returns the timeout failure as a value" $ do
      vc <- newVirtualClock
      let clock   = virtualClock vc :: Clock IO
          options = (defaultTimeoutOptions :: TimeoutOptions IO) { timeoutLimitMs = 10, timeoutClock = clock }
          service (_ :: Int) = clockSleep clock 50000 >> pure (Right (1 :: Int))
      call <- async (addTimeoutEither options "timed out" service 1)
      waitForSleepers vc 2
      advanceClock vc 10000
      wait call `shouldReturn` (Left ("timed out" :: String))
