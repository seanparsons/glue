{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.DogpileProtectionSpec where

import Data.Traversable
import Data.Typeable
import Glue.DogpileProtection
import Test.Hspec
import Data.IORef
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Base hiding (throw, throwIO)
import Control.Exception.Lifted
import Prelude hiding (sequence)

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
      (_, protectedService) <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      let results = traverse wait asyncResults
      results `shouldReturn` (fmap (*2) requests)
      (readIORef counter) `shouldReturn` 1
    it "With multiple calls to a slow failing service only one actually gets through" $ do
      counter <- newIORef (0 :: Int)
      let service _ = atomicModifyIORef' counter (\n -> (n + 1, ())) >> threadDelay delayTime >> throwIO DogpileProtectionTestException :: IO Int
      (_, protectedService) <- dogpileProtect service
      asyncResults <- traverse (async . protectedService) requests
      let results = traverse wait asyncResults
      results `shouldThrow` (== DogpileProtectionTestException)
      (readIORef counter) `shouldReturn` 1        


