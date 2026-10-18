{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting the dogpile protection of a service,
-- see <https://en.wikipedia.org/wiki/Cache_stampede>.
module Glue.DogpileProtection
  ( DogpileOptions (..)
  , defaultDogpileOptions
  , dogpileProtectWith
  , dogpileProtect
  ) where

import           Control.Monad       (void)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as M
import           Glue.Types
import           UnliftIO            (MonadIO, MonadUnliftIO, SomeException,
                                      liftIO)
import           UnliftIO.Concurrent (forkIO, threadDelay)
import           UnliftIO.Exception  (onException, throwIO, toException, tryAny)
import           UnliftIO.IORef      (IORef, atomicModifyIORef', newIORef)
import           UnliftIO.MVar       (MVar, newEmptyMVar, putMVar, readMVar,
                                      tryPutMVar)

-- | Options for configuring dogpile protection.
newtype DogpileOptions = DogpileOptions
  { dogpileCacheTimeMs :: Int
  -- ^ How long (in milliseconds) to retain a completed result for subsequent callers.
  }
  deriving (Eq, Show)

-- | Default options for dogpile protection, caching completed values for 5 seconds (5000ms).
defaultDogpileOptions :: DogpileOptions
defaultDogpileOptions =
  DogpileOptions
    { dogpileCacheTimeMs = 5000
    }

data DogpileResult b
  = CachedValue (Either SomeException b)
  | RequestInProgress (MVar (Either SomeException b))

type ResultMap a b = M.HashMap a (DogpileResult b)
type ResultRef a b = IORef (ResultMap a b)

getDogpileResult :: MonadIO m => DogpileResult b -> m b
getDogpileResult (RequestInProgress mvar) = readMVar mvar >>= either throwIO pure
getDogpileResult (CachedValue result)     = either throwIO pure result

-- | Dogpile protection of a service with configurable options.
dogpileProtectWith
  :: forall m n a b.
     (MonadUnliftIO m, MonadIO n, Hashable a)
  => DogpileOptions
  -- ^ Options configuring dogpile protection.
  -> BasicService m a b
  -- ^ The service to protect.
  -> n (BasicService m a b)
dogpileProtectWith options service = do
  mapRef <- newIORef M.empty :: n (ResultRef a b)
  let ttlMs = dogpileCacheTimeMs options
      protectedService request = do
        newVar <- newEmptyMVar
        actionOrExisting <- atomicModifyIORef' mapRef $ \resultMap ->
          case M.lookup request resultMap of
            Just existing -> (resultMap, Right existing)
            Nothing       -> (M.insert request (RequestInProgress newVar) resultMap, Left newVar)
        case actionOrExisting of
          Right existing -> getDogpileResult existing
          Left mvar -> do
            let cleanupOnAbort = do
                  atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))
                  void $ tryPutMVar mvar (Left (toException (userError "Dogpile leader aborted")))
            result <- tryAny (service request) `onException` cleanupOnAbort
            putMVar mvar result
            if ttlMs > 0
              then do
                atomicModifyIORef' mapRef (\m -> (M.insert request (CachedValue result) m, ()))
                void $ liftIO $ forkIO $ do
                  threadDelay (ttlMs * 1000)
                  atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))
              else
                atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))
            either throwIO pure result
  pure protectedService

-- | Dogpile protection of a service using 'defaultDogpileOptions',
-- preventing multiple concurrent calls for the same value from being submitted.
dogpileProtect
  :: (MonadUnliftIO m, MonadIO n, Hashable a)
  => BasicService m a b
  -- ^ The service to protect.
  -> n (BasicService m a b)
dogpileProtect = dogpileProtectWith defaultDogpileOptions
