{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting concurrent request constraints (bulkhead pattern),
-- limiting the number of in-flight and optionally queued requests.
module Glue.ConcurrencyLimit
  ( ConcurrencyLimitOptions (..)
  , ConcurrencyLimitExceededException (..)
  , defaultConcurrencyLimitOptions
  , limitConcurrency
  ) where

import           Glue.Types
import           UnliftIO           (Exception, MonadIO, MonadUnliftIO)
import           UnliftIO.Exception (bracketOnError, finally, throwIO)
import           UnliftIO.STM       (atomically, modifyTVar', newTVarIO,
                                     readTVar, retrySTM, writeTVar)

-- | Options for configuring concurrent request limits.
data ConcurrencyLimitOptions = ConcurrencyLimitOptions
  { maxInFlight                 :: Int
  -- ^ Maximum number of concurrent requests allowed to execute the underlying service simultaneously.
  , maxQueued                   :: Maybe Int
  -- ^ Optional maximum number of requests allowed to wait in the queue (`Nothing` for unbounded queue, `Just 0` for no queueing).
  , concurrencyLimitDescription :: String
  -- ^ Description attached to t'ConcurrencyLimitExceededException' when a request is rejected.
  }
  deriving (Eq, Show)

-- | Default concurrency limit options allowing 10 in-flight requests and an unbounded queue.
defaultConcurrencyLimitOptions :: ConcurrencyLimitOptions
defaultConcurrencyLimitOptions =
  ConcurrencyLimitOptions
    { maxInFlight                 = 10
    , maxQueued                   = Nothing
    , concurrencyLimitDescription = "Concurrency limit exceeded."
    }

-- | Exception thrown when both in-flight and queue limits have been reached.
newtype ConcurrencyLimitExceededException = ConcurrencyLimitExceededException String
  deriving (Eq, Show)

instance Exception ConcurrencyLimitExceededException

-- | Wraps a service with a concurrency limit restricting in-flight and queued requests.
limitConcurrency
  :: (MonadUnliftIO m, MonadIO n)
  => ConcurrencyLimitOptions
  -- ^ Options configuring the concurrency and queue bounds.
  -> BasicService m a b
  -- ^ Service to protect.
  -> n (BasicService m a b)
limitConcurrency options service = do
  let maxActive = max 1 (maxInFlight options)
  inFlightVar <- newTVarIO (0 :: Int)
  queuedVar   <- newTVarIO (0 :: Int)
  let releaseInFlight = atomically $ modifyTVar' inFlightVar (subtract 1)
      releaseQueued   = atomically $ modifyTVar' queuedVar (subtract 1)
      waitForSlot     = atomically $ do
        inFlight <- readTVar inFlightVar
        if inFlight < maxActive
          then do
            modifyTVar' queuedVar (subtract 1)
            writeTVar inFlightVar (inFlight + 1)
          else retrySTM
      limitedService request = do
        admission <- atomically $ do
          inFlight <- readTVar inFlightVar
          queued   <- readTVar queuedVar
          if inFlight < maxActive && queued == 0
            then do
              writeTVar inFlightVar (inFlight + 1)
              pure (Right True)
            else case maxQueued options of
              Just maxQ | queued >= maxQ -> pure (Left ())
              _ -> do
                writeTVar queuedVar (queued + 1)
                pure (Right False)
        case admission of
          Left () ->
            throwIO $ ConcurrencyLimitExceededException (concurrencyLimitDescription options)
          Right True ->
            finally (service request) releaseInFlight
          Right False -> do
            bracketOnError (pure ()) (\_ -> releaseQueued) (\_ -> waitForSlot)
            finally (service request) releaseInFlight
  pure limitedService
