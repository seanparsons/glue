{-# LANGUAGE ScopedTypeVariables #-}

module Glue.ShutdownSpec (spec) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Glue.Shutdown
import           Test.Hspec

spec :: Spec
spec = do
  describe "gracefulShutdown" $ do
    it "Allows in-flight requests to drain cleanly within the grace period and rejects new requests" $ do
      let service (req :: Int) = do
            threadDelay (30 * 1000)
            pure (req * 2)
      (wrapped, handle) <- gracefulShutdown (defaultShutdownOptions { shutdownGracePeriodMs = 500 }) service
      isShuttingDown handle `shouldReturn` False
      inFlight <- async (wrapped 21)
      threadDelay (5 * 1000)
      inFlightCount handle `shouldReturn` 1
      drainedCleanly <- initiateShutdown handle
      drainedCleanly `shouldBe` True
      isShuttingDown handle `shouldReturn` True
      wait inFlight `shouldReturn` 42
      wrapped 99 `shouldThrow` (== ServiceShuttingDownException "Service is shutting down.")

    it "Returns shutdownFallback for rejected calls when configured" $ do
      let opts = defaultShutdownOptions { shutdownFallback = Just (-1 :: Int), shutdownGracePeriodMs = 100 }
      (wrapped, handle) <- gracefulShutdown opts (\x -> pure (x * 2))
      wrapped 10 `shouldReturn` 20
      _ <- initiateShutdown handle
      wrapped 10 `shouldReturn` (-1)

    it "Cancels stuck in-flight requests when grace period expires" $ do
      let opts = defaultShutdownOptions { shutdownFallback = Just (-999 :: Int), shutdownGracePeriodMs = 20 }
          slowService (_ :: Int) = threadDelay (500 * 1000) >> pure 100
      (wrapped, handle) <- gracefulShutdown opts slowService
      inFlight <- async (wrapped 1)
      threadDelay (5 * 1000)
      drainedCleanly <- initiateShutdown handle
      drainedCleanly `shouldBe` False
      wait inFlight `shouldReturn` (-999)
