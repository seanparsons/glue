{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting graceful service shutdown, rejecting new requests with either
-- a fallback value or t'ServiceShuttingDownException', waiting for in-flight requests
-- to drain within a configurable grace period, and cancelling any remaining calls if
-- the grace period expires.
module Glue.Shutdown
  ( ShutdownOptions (..)
  , ServiceShuttingDownException (..)
  , ShutdownHandle (..)
  , defaultShutdownOptions
  , gracefulShutdown
  , gracefulShutdownEither
  ) where

import           Data.Maybe         (fromMaybe)
import           Glue.Clock
import           Glue.Failure
import           Glue.Types
import           UnliftIO           (Exception, MonadIO, MonadUnliftIO)
import           UnliftIO.Async     (race)
import           UnliftIO.Exception (finally, throwIO)
import           UnliftIO.STM       (atomically, modifyTVar', newTVarIO,
                                     readTVar, retrySTM, writeTVar)

-- | Options for configuring graceful service shutdown.
data ShutdownOptions m b = ShutdownOptions
  { shutdownFallback      :: Maybe b
  -- ^ Optional default value returned for rejected requests after shutdown is initiated.
  -- If 'Nothing', t'ServiceShuttingDownException' is thrown instead.
  , shutdownGracePeriodMs :: Int
  -- ^ Maximum time in milliseconds to wait for existing in-flight requests to complete before cancelling them.
  , shutdownDescription   :: String
  -- ^ Description attached to t'ServiceShuttingDownException'.
  , shutdownClock         :: Clock m
  -- ^ Clock used to measure the grace period.
  }

-- | Default shutdown options: throws t'ServiceShuttingDownException' on rejected calls and waits up to 5 seconds for in-flight calls.
defaultShutdownOptions :: MonadUnliftIO m => ShutdownOptions m b
defaultShutdownOptions =
  ShutdownOptions
    { shutdownFallback      = Nothing
    , shutdownGracePeriodMs = 5000
    , shutdownDescription   = "Service is shutting down."
    , shutdownClock         = systemClock
    }

-- | Exception thrown when a service is shutting down and no 'shutdownFallback' is configured.
newtype ServiceShuttingDownException = ServiceShuttingDownException String
  deriving (Eq, Show)

instance Exception ServiceShuttingDownException

-- | Handle used to control and inspect a service wrapped with 'gracefulShutdown'.
data ShutdownHandle m = ShutdownHandle
  { initiateShutdown :: m Bool
  -- ^ Initiates shutdown: immediately rejects new requests, waits up to 'shutdownGracePeriodMs'
  -- for in-flight requests to finish, and cancels any remaining calls if the grace period expires.
  -- Returns 'True' if all in-flight calls drained cleanly within the grace period, or 'False' if forced cancellation occurred.
  , isShuttingDown   :: m Bool
  -- ^ Returns 'True' if shutdown has been initiated.
  , inFlightCount    :: m Int
  -- ^ Returns the current number of in-flight requests executing the service.
  }

-- | Wraps a service with graceful shutdown support.
gracefulShutdown
  :: (MonadUnliftIO m, MonadIO n)
  => ShutdownOptions m b
  -- ^ Options configuring fallback behaviour and grace period.
  -> BasicService m a b
  -- ^ Service to wrap.
  -> n (BasicService m a b, ShutdownHandle m)
gracefulShutdown options =
  shutdownVia (maybe (throwIO $ ServiceShuttingDownException (shutdownDescription options)) pure (shutdownFallback options)) options

-- | Variant of 'gracefulShutdown' for services returning failures as values: rejected calls
-- return 'shutdownFallback' if set, otherwise the given failure, instead of throwing.
gracefulShutdownEither
  :: (MonadUnliftIO m, MonadIO n)
  => ShutdownOptions m (Either e b)
  -- ^ Options configuring fallback behaviour and grace period ('shutdownDescription' is unused).
  -> e
  -- ^ Failure returned for rejected calls when no 'shutdownFallback' is set.
  -> EitherService m a e b
  -- ^ Service to wrap.
  -> n (EitherService m a e b, ShutdownHandle m)
gracefulShutdownEither options onShuttingDown =
  shutdownVia (pure (fromMaybe (Left onShuttingDown) (shutdownFallback options))) options

shutdownVia
  :: forall m n a b.
     (MonadUnliftIO m, MonadIO n)
  => m b
  -> ShutdownOptions m b
  -> BasicService m a b
  -> n (BasicService m a b, ShutdownHandle m)
shutdownVia rejectRequest options service = do
  shuttingDownVar <- newTVarIO False
  forceCancelVar  <- newTVarIO False
  inFlightVar     <- newTVarIO (0 :: Int)

  let waitForForceCancel :: m ()
      waitForForceCancel = atomically $ do
        cancelled <- readTVar forceCancelVar
        if cancelled then pure () else retrySTM

      waitDrained :: m ()
      waitDrained = atomically $ do
        active <- readTVar inFlightVar
        if active <= 0 then pure () else retrySTM

      wrappedService :: BasicService m a b
      wrappedService request = do
        accepted <- atomically $ do
          shuttingDown <- readTVar shuttingDownVar
          if shuttingDown
            then pure False
            else do
              modifyTVar' inFlightVar (+ 1)
              pure True
        if not accepted
          then rejectRequest
          else do
            res <-
              race waitForForceCancel (service request)
                `finally` atomically (modifyTVar' inFlightVar (subtract 1))
            case res of
              Right val -> pure val
              Left ()   -> rejectRequest

      handle =
        ShutdownHandle
          { initiateShutdown = do
              atomically $ writeTVar shuttingDownVar True
              let graceUs = max 0 (shutdownGracePeriodMs options) * 1000
              drained <- clockTimeout (shutdownClock options) graceUs waitDrained
              case drained of
                Just () -> pure True
                Nothing -> do
                  atomically $ writeTVar forceCancelVar True
                  waitDrained
                  pure False
          , isShuttingDown = atomically $ readTVar shuttingDownVar
          , inFlightCount  = atomically $ readTVar inFlightVar
          }

  pure (wrappedService, handle)
