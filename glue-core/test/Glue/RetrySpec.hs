{-# LANGUAGE ScopedTypeVariables #-}

module Glue.RetrySpec (spec) where

import           Control.Concurrent       (throwTo)
import           Control.Concurrent.Async (async, asyncThreadId, waitCatch)
import           Control.Exception.Base   (AsyncException (UserInterrupt))
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Glue.Clock
import           Glue.Retry
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception     (Exception, fromException, throwIO, try)
import           UnliftIO.STM           (atomically, newEmptyTMVarIO, putTMVar,
                                         readTMVar)

newtype SmallInt = SmallInt Int
  deriving (Eq, Show)

instance Arbitrary SmallInt where
  arbitrary = sized $ \s -> do
    n <- choose (0, s `min` 10)
    pure $ SmallInt n

data RetryTestException = RetryTestException
  deriving (Eq, Show)

instance Exception RetryTestException

newtype NotRetryableException = NotRetryableException Int
  deriving (Eq, Show)

instance Exception NotRetryableException

-- | A clock that records each sleep (in milliseconds) instead of sleeping.
recordingClock :: IORef [Int] -> Clock IO
recordingClock ref =
  Clock
    { clockNow     = pure 0
    , clockSleep   = \us -> atomicModifyIORef' ref (\s -> (s ++ [us `div` 1000], ()))
    , clockTimeout = \_ action -> Just <$> action
    }

-- | A service failing the given number of times before succeeding, counting calls.
failingTimes :: IORef Int -> Int -> Int -> IO Int
failingTimes ref failures req = do
  counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
  if counter > failures then pure req else throwIO RetryTestException

withSleeps :: (IORef [Int] -> IO ()) -> IO [Int]
withSleeps body = do
  sleeps <- newIORef []
  body sleeps
  readIORef sleeps

alwaysFailing :: IO (Int -> IO Int)
alwaysFailing = do
  ref <- newIORef 0
  pure (failingTimes ref maxBound)

spec :: Spec
spec = do
  describe "retryingService" $ do
    it "Attempts a service call multiple times" $
      property $ \(request, SmallInt failures, SmallInt retries) -> do
        ref <- liftIO $ newIORef 0
        let options      = defaultRetryOptions { maximumRetries = retries }
            retryService = retryingService options (failingTimes ref failures)
            successCase  = retryService request `shouldReturn` (request :: Int)
            failureCase  = retryService request `shouldThrow` (== RetryTestException)
        if retries >= failures then successCase else failureCase

    it "Asynchronous exceptions are rethrown" $
      property $ \(request, retries) -> do
        ref     <- liftIO $ newIORef (0 :: Int)
        started <- newEmptyTMVarIO
        let service req = do
              atomicModifyIORef' ref (\c -> (c + 1, ()))
              atomically $ putTMVar started ()
              never <- newEmptyTMVarIO
              atomically $ readTMVar never :: IO ()
              pure (req * 2 :: Int)
            options      = defaultRetryOptions { maximumRetries = retries }
            retryService = retryingService options service
        call <- async (retryService request)
        atomically $ readTMVar started
        throwTo (asyncThreadId call) UserInterrupt
        result <- waitCatch call
        either fromException (const Nothing) result `shouldBe` Just UserInterrupt
        readIORef ref `shouldReturn` 1

    it "Does not retry a failure rejected by retryOn" $ do
      ref <- newIORef (0 :: Int)
      let service req = atomicModifyIORef' ref (\c -> (c + 1, ())) >> throwIO (NotRetryableException req) :: IO Int
          retryable e = maybe True (const False) (fromException e :: Maybe NotRetryableException)
          retryService = retryingService defaultRetryOptions { retryOn = retryable } service
      retryService 4 `shouldThrow` (== NotRetryableException 4)
      readIORef ref `shouldReturn` 1

    it "Does not retry a request rejected by retryAllowed" $ do
      ref <- newIORef 0
      let retryService = retryingService defaultRetryOptions { retryAllowed = (> 0) } (failingTimes ref 1)
      retryService 0 `shouldThrow` (== RetryTestException)
      readIORef ref `shouldReturn` 1

    it "Waits according to the backoff, capped by the maximum delay" $ do
      service <- alwaysFailing
      sleeps <- withSleeps $ \ref -> do
        let options = defaultRetryOptions
              { maximumRetries  = 5
              , retryBackoff    = ExponentialBackoff 100 2
              , retryMaxDelayMs = Just 300
              , retryClock      = recordingClock ref
              }
        retryingService options service 1 `shouldThrow` (== RetryTestException)
      sleeps `shouldBe` [100, 200, 300, 300, 300]

    it "Computes constant, linear and exponential backoffs" $ do
      fmap (backoffDelayMs (ConstantBackoff 50)) [0 .. 3] `shouldBe` [50, 50, 50, 50]
      fmap (backoffDelayMs (LinearBackoff 50 25)) [0 .. 3] `shouldBe` [50, 75, 100, 125]
      fmap (backoffDelayMs (ExponentialBackoff 50 3)) [0 .. 3] `shouldBe` [50, 150, 450, 1350]

    it "Keeps jittered waits within their bounds" $
      property $ \(randoms :: [NonNegative Int], equal :: Bool) -> do
        source <- newIORef (cycle (0 : fmap getNonNegative randoms))
        service <- alwaysFailing
        let random n = atomicModifyIORef' source (\rs -> (drop 1 rs, head' rs `mod` (n + 1)))
            head' = foldr const 0
        sleeps <- withSleeps $ \ref -> do
          let options = defaultRetryOptions
                { maximumRetries  = 6
                , retryBackoff    = ExponentialBackoff 10 2
                , retryMaxDelayMs = Just 200
                , retryJitter     = if equal then EqualJitter else FullJitter
                , retryRandom     = random
                , retryClock      = recordingClock ref
                }
          retryingService options service 1 `shouldThrow` (== RetryTestException)
        let caps = [10, 20, 40, 80, 160, 200]
            lower cap = if equal then cap `div` 2 else 0
        sleeps `shouldSatisfy` (\s -> and (zipWith (\d cap -> d >= lower cap && d <= cap) s caps) && length s == 6)

    it "Skips a retry whose wait would pass the deadline, returning the last failure" $ do
      ref     <- newIORef (0 :: Int)
      retries <- newIORef []
      let service req = do
            n <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            throwIO (NotRetryableException (req + n)) :: IO Int
      sleeps <- withSleeps $ \sleepRef -> do
        let options = defaultRetryOptions
              { maximumRetries   = 5
              , retryBackoff     = LinearBackoff 100 100
              , retryRemainingMs = pure (Just 150)
              , onRetry          = \attempt _ delay -> atomicModifyIORef' retries (\r -> (r ++ [(attempt, delay)], ()))
              , retryClock       = recordingClock sleepRef
              }
        retryingService options service 10 `shouldThrow` (== NotRetryableException 12)
      sleeps `shouldBe` [100]
      readIORef retries `shouldReturn` [(1, 100)]

    it "Does not retry once the deadline has passed" $ do
      ref <- newIORef 0
      let options = defaultRetryOptions { retryRemainingMs = pure (Just 0) }
      retryingService options (failingTimes ref 1) 1 `shouldThrow` (== RetryTestException)
      readIORef ref `shouldReturn` 1

  describe "retryingServiceEither" $ do
    it "Retries Left failures accepted by retryOn and returns the last one" $ do
      ref <- newIORef (0 :: Int)
      let service (_ :: Int) = atomicModifyIORef' ref (\c -> (c + 1, Left (c + 1) :: Either Int Int))
          options = defaultRetryOptions { maximumRetries = 5, retryOn = (< 3) }
      retryingServiceEither options service 1 `shouldReturn` Left 3
      readIORef ref `shouldReturn` 3

    it "Behaves like the exception shape for the same outcomes" $
      property $ \(outcomes :: [Bool], SmallInt retries) -> do
        exceptionRef <- newIORef outcomes
        valueRef     <- newIORef outcomes
        let next ref = atomicModifyIORef' ref (\o -> case o of { (x : xs) -> (xs, x); [] -> ([], True) })
            exceptional (req :: Int) = next exceptionRef >>= \ok -> if ok then pure req else throwIO RetryTestException
            valued (req :: Int) = next valueRef >>= \ok -> pure (if ok then Right req else Left RetryTestException)
            options :: RetryOptions IO Int f
            options = defaultRetryOptions { maximumRetries = retries }
        viaException <- try (retryingService options exceptional 7)
        viaValue     <- retryingServiceEither options valued 7
        viaException `shouldBe` viaValue
        remaining <- readIORef valueRef
        readIORef exceptionRef `shouldReturn` remaining
