{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.DogpileProtectionSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Base   hiding (throw, throwIO)
import           Control.Exception.Lifted
import           Data.IORef
import           Data.Traversable
import           Data.Typeable
import           Glue.DogpileProtection
import           Prelude                  hiding (sequence)
import           Test.Hspec

data DogpileProtectionTestException = DogpileProtectionTestException deriving (Eq, Show, Typeable)
instance Exception DogpileProtectionTestException

requests :: [Int]
requests = take 10 $ repeat 1

delayTime :: Int
delayTime = 1000 * 1000

spec :: Spec
spec = do
  describe "dogpileProtect" $ do
    it "With multiple calls to a slow service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      let service request = atomicModifyIORef' counter (\n -> (n + 1, ())) >> threadDelay delayTime >> return (request * 2)
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      let results = traverse wait asyncResults
      results `shouldReturn` (fmap (*2) requests)
      (readIORef counter) `shouldReturn` 1
    it "With multiple calls to a slow failing service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      let service _ = atomicModifyIORef' counter (\n -> (n + 1, ())) >> threadDelay delayTime >> throwIO DogpileProtectionTestException :: IO Int
      protectedService <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      let results = traverse wait asyncResults
      results `shouldThrow` (== DogpileProtectionTestException)
      (readIORef counter) `shouldReturn` 1


