{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting concurrent request constraints (bulkhead pattern),
-- limiting the number of in-flight and optionally queued requests.
--
-- 'limitConcurrency' caps one wrapped service. 'limitConcurrencyBy' keeps one limit per key (a
-- backend or target, say) and refuses a full key at once with 'Saturated' as a value, so a full
-- guard can feed a fallback without exception plumbing and is never mistaken for a backend failure.
-- A slot covers the wrapped call only; 'withSlot' holds one for work that outlives the call, such
-- as a streamed response consumed later.
module Glue.ConcurrencyLimit
  ( ConcurrencyLimitOptions (..)
  , ConcurrencyLimitExceededException (..)
  , defaultConcurrencyLimitOptions
  , limitConcurrency
  , limitConcurrencyEither
    -- * Keyed limits
  , Saturated (..)
  , AdmissionLease (..)
  , AdmissionStore (..)
  , newInMemoryAdmissionStore
  , withSlot
  , limitConcurrencyBy
  , limitConcurrencyByWithStore
  , limitConcurrencyByEither
  ) where

import           Data.Foldable       (traverse_)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as M
import           Glue.Failure
import           Glue.Types
import           UnliftIO            (Exception, MonadIO, MonadUnliftIO)
import           UnliftIO.Exception  (bracket, bracketOnError, finally,
                                      throwIO)
import           UnliftIO.STM        (atomically, modifyTVar', newTVarIO,
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

limitVia
  :: (MonadUnliftIO m, MonadIO n)
  => m r
  -> ConcurrencyLimitOptions
  -> BasicService m a r
  -> n (BasicService m a r)
limitVia refuse options service = do
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
          Left () -> refuse
          Right True ->
            finally (service request) releaseInFlight
          Right False -> do
            bracketOnError (pure ()) (\_ -> releaseQueued) (\_ -> waitForSlot)
            finally (service request) releaseInFlight
  pure limitedService

-- | Wraps a service with a concurrency limit restricting in-flight and queued requests.
limitConcurrency
  :: (MonadUnliftIO m, MonadIO n)
  => ConcurrencyLimitOptions
  -- ^ Options configuring the concurrency and queue bounds.
  -> BasicService m a b
  -- ^ Service to protect.
  -> n (BasicService m a b)
limitConcurrency options =
  limitVia (throwIO $ ConcurrencyLimitExceededException (concurrencyLimitDescription options)) options

-- | Variant of 'limitConcurrency' for services returning failures as values, returning the given
-- failure when both limits are reached instead of throwing.
limitConcurrencyEither
  :: (MonadUnliftIO m, MonadIO n)
  => ConcurrencyLimitOptions
  -- ^ Options configuring the concurrency and queue bounds ('concurrencyLimitDescription' is unused).
  -> e
  -- ^ Failure returned when the limits are reached.
  -> EitherService m a e b
  -- ^ Service to protect.
  -> n (EitherService m a e b)
limitConcurrencyEither options onSaturated = limitVia (pure (Left onSaturated)) options

-- | Refusal by a keyed limit whose slots are all in use.
data Saturated = Saturated
  deriving (Eq, Show)

-- | A claimed slot, released exactly once by 'withSlot'.
newtype AdmissionLease m = AdmissionLease
  { releaseLease :: m ()
  }

-- | Where keyed slots are counted. The in-memory store is the default; an implementation backed by
-- shared storage (claim, renew and release of expiring leases, say) gives limits shared across
-- instances, renewing its own leases for as long as they are held.
data AdmissionStore m k = AdmissionStore
  { claimSlot   :: k -> m (Maybe (AdmissionLease m))
  -- ^ Claims a slot for the key, or 'Nothing' if its limit is reached. Must be atomic.
  , slotsInUse  :: k -> m Int
  -- ^ Slots currently held for the key.
  , trackedKeys :: m Int
  -- ^ Number of keys with slots held. Keys with nothing in flight are not kept.
  }

-- | An in-memory store with the given limit per key (a limit of zero or less refuses every call).
newInMemoryAdmissionStore :: (Hashable k, MonadIO m, MonadIO n) => (k -> Int) -> n (AdmissionStore m k)
newInMemoryAdmissionStore limitFor = do
  countsVar <- newTVarIO M.empty
  let release key = atomically $ modifyTVar' countsVar (M.update (\n -> if n <= 1 then Nothing else Just (n - 1)) key)
      claim key = atomically $ do
        counts <- readTVar countsVar
        let current = M.findWithDefault 0 key counts
        if current < limitFor key
          then do
            writeTVar countsVar (M.insert key (current + 1) counts)
            pure (Just (AdmissionLease (release key)))
          else pure Nothing
  pure
    AdmissionStore
      { claimSlot   = claim
      , slotsInUse  = \key -> atomically $ M.findWithDefault 0 key <$> readTVar countsVar
      , trackedKeys = atomically $ M.size <$> readTVar countsVar
      }

-- | Runs the work holding a slot for the key, or returns 'Saturated' at once if none is free.
-- The slot is released on every exit path, including asynchronous cancellation.
withSlot :: MonadUnliftIO m => AdmissionStore m k -> k -> m r -> m (Either Saturated r)
withSlot store key work =
  bracket (claimSlot store key) (traverse_ releaseLease) $
    maybe (pure (Left Saturated)) (const (Right <$> work))

-- | Wraps a service with one concurrency limit per key, refusing a call whose key is full with
-- 'Saturated' instead of queueing it.
limitConcurrencyBy
  :: (MonadUnliftIO m, MonadIO n, Hashable k)
  => (a -> k)
  -- ^ The key a request is limited under.
  -> (k -> Int)
  -- ^ The limit for each key.
  -> BasicService m a b
  -- ^ Service to protect.
  -> n (BasicService m a (Either Saturated b))
limitConcurrencyBy keyOf limitFor service = do
  store <- newInMemoryAdmissionStore limitFor
  pure (limitConcurrencyByWithStore store keyOf service)

-- | Variant of 'limitConcurrencyBy' counting slots in the given store.
limitConcurrencyByWithStore
  :: MonadUnliftIO m
  => AdmissionStore m k
  -- ^ Store counting the slots.
  -> (a -> k)
  -- ^ The key a request is limited under.
  -> BasicService m a b
  -- ^ Service to protect.
  -> BasicService m a (Either Saturated b)
limitConcurrencyByWithStore store keyOf service request = withSlot store (keyOf request) (service request)

-- | Variant of 'limitConcurrencyBy' for services returning failures as values, returning the given
-- failure when the key is full.
limitConcurrencyByEither
  :: (MonadUnliftIO m, MonadIO n, Hashable k)
  => (a -> k)
  -- ^ The key a request is limited under.
  -> (k -> Int)
  -- ^ The limit for each key.
  -> e
  -- ^ Failure returned when the key is full.
  -> EitherService m a e b
  -- ^ Service to protect.
  -> n (EitherService m a e b)
limitConcurrencyByEither keyOf limitFor onSaturated service = do
  limited <- limitConcurrencyBy keyOf limitFor service
  pure $ fmap (either (const (Left onSaturated)) id) . limited
