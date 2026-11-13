{-# LANGUAGE ScopedTypeVariables #-}

module Glue.DogpileProtectionSpec (spec) where

import           Control.Concurrent       (yield)
import           Control.Concurrent.Async (async, wait)
import           Data.IORef
import           Glue.Clock
import           Glue.Clock.Virtual
import           Glue.DogpileProtection
import           Test.Hspec
import           UnliftIO.Exception       (Exception, throwIO)
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)

data DogpileProtectionTestException = DogpileProtectionTestException
  deriving (Eq, Show)

instance Exception DogpileProtectionTestException

requests :: [Int]
requests = replicate 10 1

waitForCount :: IORef Int -> Int -> IO ()
waitForCount ref n = readIORef ref >>= \c -> if c >= n then pure () else yield >> waitForCount ref n

spec :: Spec
spec = do
  describe "dogpileProtect" $ do
    it "With multiple calls to a slow service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      gate    <- newEmptyTMVarIO
      let service request = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            atomically $ readTMVar gate
            pure (request * 2)
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      waitForCount counter 1
      atomically $ putTMVar gate ()
      traverse wait asyncResults `shouldReturn` fmap (* 2) requests
      readIORef counter `shouldReturn` 1

    it "With multiple calls to a slow failing service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      gate    <- newEmptyTMVarIO
      let service _ = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            atomically $ readTMVar gate
            throwIO DogpileProtectionTestException :: IO Int
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      waitForCount counter 1
      atomically $ putTMVar gate ()
      traverse wait asyncResults `shouldThrow` (== DogpileProtectionTestException)
      readIORef counter `shouldReturn` 1

  describe "dogpileProtectWith" $
    it "Retains a completed result for exactly the cache time" $ do
      vc      <- newVirtualClock
      counter <- newIORef (0 :: Int)
      let clock   = virtualClock vc :: Clock IO
          options = (defaultDogpileOptions :: DogpileOptions IO) { dogpileCacheTimeMs = 100, dogpileClock = clock }
          service (request :: Int) = atomicModifyIORef' counter (\n -> (n + 1, request * 2))
      protectedService <- dogpileProtectWith options service
      protectedService 1 `shouldReturn` 2
      waitForSleepers vc 1
      protectedService 1 `shouldReturn` 2
      readIORef counter `shouldReturn` 1
      advanceClock vc (99 * 1000)
      protectedService 1 `shouldReturn` 2
      readIORef counter `shouldReturn` 1
      advanceClock vc 1000
      protectedService 1 `shouldReturn` 2
      readIORef counter `shouldReturn` 2
