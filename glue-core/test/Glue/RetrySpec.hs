module Glue.RetrySpec (spec) where

import           Control.Concurrent     (forkIO, myThreadId, threadDelay)
import           Control.Exception.Base (AsyncException (UserInterrupt),
                                         throwTo)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Glue.Retry
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception     (Exception, throwIO)

newtype SmallInt = SmallInt Int
  deriving (Eq, Show)

instance Arbitrary SmallInt where
  arbitrary = sized $ \s -> do
    n <- choose (0, s `min` 10)
    pure $ SmallInt n

data RetryTestException = RetryTestException
  deriving (Eq, Show)

instance Exception RetryTestException

spec :: Spec
spec = do
  describe "retryingService" $ do
    it "Attempts a service call multiple times" $
      property $ \(request, SmallInt failures, SmallInt retries) -> do
        ref <- liftIO $ newIORef 0
        let service req = do
              counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
              if counter > failures then pure req else throwIO RetryTestException
            options      = defaultRetryOptions { maximumRetries = retries }
            retryService = retryingService options service
            successCase  = retryService request `shouldReturn` (request :: Int)
            failureCase  = retryService request `shouldThrow` (== RetryTestException)
        if retries >= failures then successCase else failureCase

    it "Asynchronous exceptions are rethrown" $
      property $ \(request, retries) -> do
        ref <- liftIO $ newIORef (0 :: Int)
        let service req = do
              atomicModifyIORef' ref (\c -> (c + 1, ()))
              threadId <- myThreadId
              _ <- forkIO (threadDelay 2000 >> throwTo threadId UserInterrupt)
              threadDelay 1000000
              pure (req * 2 :: Int)
            options      = defaultRetryOptions { maximumRetries = retries }
            retryService = retryingService options service
        retryService request `shouldThrow` (== UserInterrupt)
        readIORef ref `shouldReturn` 1
