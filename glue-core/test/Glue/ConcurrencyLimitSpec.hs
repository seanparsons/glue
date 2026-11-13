{-# LANGUAGE ScopedTypeVariables #-}

module Glue.ConcurrencyLimitSpec (spec) where

import           Control.Concurrent       (threadDelay, yield)
import           Control.Concurrent.Async (async, cancel, mapConcurrently, poll,
                                           wait)
import           Data.IORef
import           Glue.ConcurrencyLimit
import           Test.Hspec
import           UnliftIO.Exception       (Exception, throwIO, try)
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)

data ConcurrencyTestException = ConcurrencyTestException
  deriving (Eq, Show)

instance Exception ConcurrencyTestException

waitUntil :: IO Bool -> IO ()
waitUntil condition = condition >>= \done -> if done then pure () else yield >> waitUntil condition

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
      gate    <- newEmptyTMVarIO
      entered <- newIORef (0 :: Int)
      let service (req :: Int) = do
            atomicModifyIORef' entered (\n -> (n + 1, ()))
            atomically $ readTMVar gate
            pure req
      limited <- limitConcurrency opts service
      a1 <- async (try (limited 1) :: IO (Either ConcurrencyLimitExceededException Int))
      waitUntil ((== 1) <$> readIORef entered)
      -- With 1 in flight, one of these queues and the other is rejected at once.
      a2 <- async (try (limited 2) :: IO (Either ConcurrencyLimitExceededException Int))
      a3 <- async (try (limited 3) :: IO (Either ConcurrencyLimitExceededException Int))
      let rejected = fmap (maybe False (either (const False) (either (const True) (const False)))) . poll
      waitUntil ((||) <$> rejected a2 <*> rejected a3)
      atomically $ putTMVar gate ()
      wait a1 `shouldReturn` Right 1
      results <- traverse wait [a2, a3]
      length [() | Left (ConcurrencyLimitExceededException "Busy") <- results] `shouldBe` 1
      length [r | Right r <- results, r `elem` [2, 3]] `shouldBe` 1

    it "Releases slot when underlying service throws an exception" $ do
      let opts = defaultConcurrencyLimitOptions { maxInFlight = 1, maxQueued = Just 0 }
          failingOrOk (req :: Int) =
            if req == 0 then throwIO ConcurrencyTestException else pure (req + 10)
      limited <- limitConcurrency opts failingOrOk
      limited 0 `shouldThrow` (== ConcurrencyTestException)
      limited 5 `shouldReturn` 15

  describe "limitConcurrencyEither" $
    it "Returns the saturation failure as a value" $ do
      gate    <- newEmptyTMVarIO
      started <- newEmptyTMVarIO
      let opts = defaultConcurrencyLimitOptions { maxInFlight = 1, maxQueued = Just 0 }
      limited <- limitConcurrencyEither opts "busy" $ \(req :: Int) -> do
        atomically $ putTMVar started ()
        atomically $ readTMVar gate
        pure (Right req)
      first <- async (limited 1)
      atomically $ readTMVar started
      limited 2 `shouldReturn` (Left "busy" :: Either String Int)
      atomically $ putTMVar gate ()
      wait first `shouldReturn` Right 1

  describe "limitConcurrencyBy" $ do
    it "Refuses a second call on a key with a limit of one" $ do
      store <- newInMemoryAdmissionStore (const 1)
      gate  <- newEmptyTMVarIO
      let limited = limitConcurrencyByWithStore store (`div` 10) (\(req :: Int) -> atomically (readTMVar gate) >> pure req)
      first <- async (limited 1)
      waitUntil ((== 1) <$> slotsInUse store 0)
      limited 2 `shouldReturn` Left Saturated
      atomically $ putTMVar gate ()
      wait first `shouldReturn` Right 1
      limited 3 `shouldReturn` Right 3

    it "Keeps keys isolated" $ do
      store <- newInMemoryAdmissionStore (const 1)
      gate  <- newEmptyTMVarIO
      let limited = limitConcurrencyByWithStore store (`div` 10) (\(req :: Int) -> atomically (readTMVar gate) >> pure req)
      first <- async (limited 1)
      waitUntil ((== 1) <$> slotsInUse store 0)
      other <- async (limited 11)
      waitUntil ((== 1) <$> slotsInUse store 1)
      atomically $ putTMVar gate ()
      wait first `shouldReturn` Right 1
      wait other `shouldReturn` Right 11

    it "Uses a separate limit per key" $ do
      store <- newInMemoryAdmissionStore (\k -> if k == "wide" then 2 else 1)
      gate  <- newEmptyTMVarIO
      let limited = limitConcurrencyByWithStore store fst (\(_, n :: Int) -> atomically (readTMVar gate) >> pure n)
      a <- async (limited ("wide" :: String, 1))
      b <- async (limited ("wide", 2))
      waitUntil ((== 2) <$> slotsInUse store "wide")
      limited ("wide", 3) `shouldReturn` Left Saturated
      atomically $ putTMVar gate ()
      wait a `shouldReturn` Right 1
      wait b `shouldReturn` Right 2

    it "Releases the slot on exception and on cancellation, leaving no keys behind" $ do
      store <- newInMemoryAdmissionStore (const 1)
      let failing (_ :: Int) = throwIO ConcurrencyTestException :: IO Int
      limitConcurrencyByWithStore store id failing 1 `shouldThrow` (== ConcurrencyTestException)
      slotsInUse store 1 `shouldReturn` 0
      gate <- newEmptyTMVarIO
      blocked <- async (limitConcurrencyByWithStore store id (\(req :: Int) -> atomically (readTMVar gate) >> pure req) 2)
      waitUntil ((== 1) <$> slotsInUse store 2)
      cancel blocked
      slotsInUse store 2 `shouldReturn` 0
      trackedKeys store `shouldReturn` 0

    it "Never exceeds the limit for a key under concurrent load" $ do
      current <- newIORef (0 :: Int)
      peak    <- newIORef (0 :: Int)
      let service (req :: Int) = do
            cur <- atomicModifyIORef' current (\c -> (c + 1, c + 1))
            atomicModifyIORef' peak (\p -> (max p cur, ()))
            yield
            atomicModifyIORef' current (\c -> (c - 1, ()))
            pure req
      limited <- limitConcurrencyBy (const ()) (const 3) service
      results <- mapConcurrently limited [1 .. 200]
      length [r | Right r <- results] `shouldSatisfy` (> 0)
      readIORef peak >>= (`shouldSatisfy` (<= 3))

  describe "withSlot" $
    it "Holds the slot until the work finishes, beyond any service call" $ do
      store <- newInMemoryAdmissionStore (const 1)
      gate  <- newEmptyTMVarIO
      streaming <- async (withSlot store "stream" (atomically (readTMVar gate) >> pure "consumed"))
      waitUntil ((== 1) <$> slotsInUse store ("stream" :: String))
      withSlot store "stream" (pure "second") `shouldReturn` Left Saturated
      atomically $ putTMVar gate ()
      wait streaming `shouldReturn` Right "consumed"
      withSlot store "stream" (pure "second") `shouldReturn` Right "second"

  describe "limitConcurrencyByEither" $
    it "Returns the given failure when the key is full" $ do
      limited <- limitConcurrencyByEither (const ()) (const 0) "full" (\(req :: Int) -> pure (Right req))
      limited 1 `shouldReturn` (Left "full" :: Either String Int)
