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
  ) where

import           Glue.Types
import           UnliftIO           (Exception, MonadIO, MonadUnliftIO)
import           UnliftIO.Async     (race)
import           UnliftIO.Exception (finally, throwIO)
import           UnliftIO.STM       (atomically, modifyTVar', newTVarIO,
                                     readTVar, retrySTM, writeTVar)
import           UnliftIO.Timeout   (timeout)

-- | Options for configuring graceful service shutdown.
data ShutdownOptions b = ShutdownOptions
  { shutdownFallback      :: Maybe b
  -- ^ Optional default value returned for rejected requests after shutdown is initiated.
  -- If 'Nothing', t'ServiceShuttingDownException' is thrown instead.
  , shutdownGracePeriodMs :: Int
  -- ^ Maximum time in milliseconds to wait for existing in-flight requests to complete before cancelling them.
  , shutdownDescription   :: String
  -- ^ Description attached to t'ServiceShuttingDownException'.
  }
  deriving (Eq, Show)

-- | Default shutdown options: throws t'ServiceShuttingDownException' on rejected calls and waits up to 5 seconds for in-flight calls.
defaultShutdownOptions :: ShutdownOptions b
defaultShutdownOptions =
  ShutdownOptions
    { shutdownFallback      = Nothing
    , shutdownGracePeriodMs = 5000
    , shutdownDescription   = "Service is shutting down."
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
  :: forall m n a b.
     (MonadUnliftIO m, MonadIO n)
  => ShutdownOptions b
  -- ^ Options configuring fallback behaviour and grace period.
  -> BasicService m a b
  -- ^ Service to wrap.
  -> n (BasicService m a b, ShutdownHandle m)
gracefulShutdown options service = do
  shuttingDownVar <- newTVarIO False
  forceCancelVar  <- newTVarIO False
  inFlightVar     <- newTVarIO (0 :: Int)

  let rejectRequest :: m b
      rejectRequest =
        case shutdownFallback options of
          Just fallback -> pure fallback
          Nothing       -> throwIO $ ServiceShuttingDownException (shutdownDescription options)

      waitForForceCancel :: m ()
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
              drained <- timeout graceUs waitDrained
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
