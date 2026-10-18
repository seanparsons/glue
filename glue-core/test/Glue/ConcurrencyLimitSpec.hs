{-# LANGUAGE ScopedTypeVariables #-}

module Glue.ConcurrencyLimitSpec (spec) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, mapConcurrently, wait)
import           Data.IORef
import           Glue.ConcurrencyLimit
import           Test.Hspec
import           UnliftIO.Exception       (Exception, throwIO, try)

data ConcurrencyTestException = ConcurrencyTestException
  deriving (Eq, Show)

instance Exception ConcurrencyTestException

spec :: Spec
spec = do
  describe "limitConcurrency" $ do
    it "Never exceeds maxInFlight concurrent executions" $ do
      currentInFlight <- newIORef (0 :: Int)
      peakInFlight    <- newIORef (0 :: Int)
      let maxAllowed = 3
          service (req :: Int) = do
            cur <- atomicModifyIORef' currentInFlight (\c -> (c + 1, c + 1))
            atomicModifyIORef' peakInFlight (\p -> (max p cur, ()))
            threadDelay (20 * 1000)
            atomicModifyIORef' currentInFlight (\c -> (c - 1, ()))
            pure (req * 2)
      limited <- limitConcurrency (defaultConcurrencyLimitOptions { maxInFlight = maxAllowed }) service
      results <- mapConcurrently limited [1 .. 12]
      results `shouldBe` fmap (* 2) [1 .. 12]
      peak <- readIORef peakInFlight
      peak `shouldSatisfy` (<= maxAllowed)
      peak `shouldSatisfy` (> 1)

    it "Rejects requests when maxInFlight and maxQueued are full" $ do
      let opts = ConcurrencyLimitOptions
            { maxInFlight                 = 1
            , maxQueued                   = Just 1
            , concurrencyLimitDescription = "Busy"
            }
          service (req :: Int) = do
            threadDelay (60 * 1000)
            pure req
      limited <- limitConcurrency opts service
      a1 <- async (try (limited 1) :: IO (Either ConcurrencyLimitExceededException Int))
      threadDelay (10 * 1000)
      a2 <- async (try (limited 2) :: IO (Either ConcurrencyLimitExceededException Int))
      threadDelay (10 * 1000)
      -- 3rd request should be rejected immediately since 1 is in-flight and 1 is queued
      r3 <- try (limited 3) :: IO (Either ConcurrencyLimitExceededException Int)
      r1 <- wait a1
      r2 <- wait a2
      r3 `shouldBe` Left (ConcurrencyLimitExceededException "Busy")
      r1 `shouldBe` Right 1
      r2 `shouldBe` Right 2

    it "Releases slot when underlying service throws an exception" $ do
      let opts = defaultConcurrencyLimitOptions { maxInFlight = 1, maxQueued = Just 0 }
          failingOrOk (req :: Int) =
            if req == 0 then throwIO ConcurrencyTestException else pure (req + 10)
      limited <- limitConcurrency opts failingOrOk
      limited 0 `shouldThrow` (== ConcurrencyTestException)
      limited 5 `shouldReturn` 15
