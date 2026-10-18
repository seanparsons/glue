{-# LANGUAGE ScopedTypeVariables #-}

module Glue.DogpileProtectionSpec (spec) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Data.IORef
import           Glue.DogpileProtection
import           Test.Hspec
import           UnliftIO.Exception       (Exception, throwIO)

data DogpileProtectionTestException = DogpileProtectionTestException
  deriving (Eq, Show)

instance Exception DogpileProtectionTestException

requests :: [Int]
requests = replicate 10 1

delayTime :: Int
delayTime = 50 * 1000

spec :: Spec
spec = do
  describe "dogpileProtect" $ do
    it "With multiple calls to a slow service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      let service request = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            threadDelay delayTime
            pure (request * 2)
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      traverse wait asyncResults `shouldReturn` fmap (* 2) requests
      readIORef counter `shouldReturn` 1

    it "With multiple calls to a slow failing service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      let service _ = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            threadDelay delayTime
            throwIO DogpileProtectionTestException :: IO Int
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      traverse wait asyncResults `shouldThrow` (== DogpileProtectionTestException)
      readIORef counter `shouldReturn` 1
