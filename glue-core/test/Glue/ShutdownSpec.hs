{-# LANGUAGE ScopedTypeVariables #-}

module Glue.ShutdownSpec (spec) where

import           Control.Concurrent       (yield)
import           Control.Concurrent.Async (async, wait)
import           Glue.Clock
import           Glue.Clock.Virtual
import           Glue.Shutdown
import           Test.Hspec
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, takeTMVar)

spec :: Spec
spec = do
  describe "gracefulShutdown" $ do
    it "Allows in-flight requests to drain cleanly within the grace period and rejects new requests" $ do
      vc      <- newVirtualClock
      release <- newEmptyTMVarIO
      let clock = virtualClock vc :: Clock IO
          service (req :: Int) = do
            atomically $ takeTMVar release
            pure (req * 2)
      (wrapped, handle) <- gracefulShutdown ((defaultShutdownOptions :: ShutdownOptions IO Int) { shutdownGracePeriodMs = 500, shutdownClock = clock }) service
      isShuttingDown handle `shouldReturn` False
      inFlight <- async (wrapped 21)
      let waitInFlight = inFlightCount handle >>= \n -> if n == 1 then pure () else yield >> waitInFlight
      waitInFlight
      shutdown <- async (initiateShutdown handle)
      waitForSleepers vc 1
      isShuttingDown handle `shouldReturn` True
      atomically $ putTMVar release ()
      wait shutdown `shouldReturn` True
      wait inFlight `shouldReturn` 42
      wrapped 99 `shouldThrow` (== ServiceShuttingDownException "Service is shutting down.")

    it "Returns shutdownFallback for rejected calls when configured" $ do
      let opts = defaultShutdownOptions { shutdownFallback = Just (-1 :: Int), shutdownGracePeriodMs = 100 }
      (wrapped, handle) <- gracefulShutdown opts (\x -> pure (x * 2))
      wrapped 10 `shouldReturn` 20
      _ <- initiateShutdown handle
      wrapped 10 `shouldReturn` (-1)

    it "Cancels stuck in-flight requests when grace period expires" $ do
      vc <- newVirtualClock
      let clock = virtualClock vc :: Clock IO
          opts  = (defaultShutdownOptions :: ShutdownOptions IO Int) { shutdownFallback = Just (-999), shutdownGracePeriodMs = 20, shutdownClock = clock }
          slowService (_ :: Int) = clockSleep clock (500 * 1000) >> pure (100 :: Int)
      (wrapped, handle) <- gracefulShutdown opts slowService
      inFlight <- async (wrapped 1)
      waitForSleepers vc 1
      shutdown <- async (initiateShutdown handle)
      waitForSleepers vc 2
      advanceClock vc (20 * 1000)
      wait shutdown `shouldReturn` False
      wait inFlight `shouldReturn` (-999)

  describe "gracefulShutdownEither" $
    it "Returns the shutting-down failure as a value" $ do
      (wrapped, handle) <- gracefulShutdownEither defaultShutdownOptions "shutting down" (\(x :: Int) -> pure (Right (x * 2)))
      wrapped 4 `shouldReturn` (Right 8 :: Either String Int)
      _ <- initiateShutdown handle
      wrapped 4 `shouldReturn` Left "shutting down"
