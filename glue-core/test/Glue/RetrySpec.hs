{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Glue.RetrySpec where

import           Control.Concurrent
import           Control.Exception.Base   hiding (throw, throwIO, throwTo)
import           Control.Exception.Lifted hiding (throwTo)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Typeable
import           Glue.Retry
import           Test.Hspec
import           Test.QuickCheck

newtype SmallInt = SmallInt Int deriving (Eq, Show)

instance Arbitrary SmallInt where
  arbitrary = sized $ \s -> do
                              n <- choose (0, s `min` 10)
                              return $ SmallInt n

data RetryTestException = RetryTestException deriving (Eq, Show, Typeable)
instance Exception RetryTestException

spec :: Spec
spec = do
  describe "retryingService" $ do
    it "Attempts a service call multiple times" $ do
      property $ \(request, (SmallInt failures), (SmallInt retries)) ->
        do
          ref <- liftIO $ newIORef 0
          let service req   = do
                                counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                                if counter > failures then return req else throwIO RetryTestException
          let options       = defaultRetryOptions { maximumRetries = retries }
          let retryService  = retryingService options service
          let successCase   = (retryService request) `shouldReturn` (request :: Int)
          let failureCase   = (retryService request) `shouldThrow` (== RetryTestException)
          if retries >= failures then successCase else failureCase
    it "Asynchronous exceptions are rethrown" $ do
      property $ \(request, retries) ->
        do
          ref <- liftIO $ newIORef (0 :: Int)
          let service req   = do
                                atomicModifyIORef' ref (\c -> (c + 1, ()))
                                threadId <- myThreadId
                                _ <- forkIO (threadDelay 10000 >> throwTo threadId UserInterrupt)
                                threadDelay 1000000
                                return (req * 2 :: Int)
          let options       = defaultRetryOptions { maximumRetries = retries }
          let retryService  = retryingService options service
          (retryService request) `shouldThrow` (== UserInterrupt)
          (readIORef ref) `shouldReturn` 1
