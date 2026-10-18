{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing circuit breaker functionality, which is the ability to open a circuit
-- once a number of failures have occurred, thereby preventing later calls from attempting
-- to make unsuccessful calls.
-- Often this is useful if the underlying service were to repeatedly time out, so as to
-- reduce the number of calls inflight holding up upstream callers.
module Glue.CircuitBreaker
  ( CircuitBreakerOptions (..)
  , CircuitBreakerState
  , CircuitBreakerException (..)
  , combineCircuitBreakerStates
  , isCircuitBreakerOpen
  , isCircuitBreakerClosed
  , defaultCircuitBreakerOptions
  , circuitBreaker
  ) where

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Glue.Types
import           UnliftIO              (Exception, MonadIO, MonadUnliftIO,
                                        liftIO)
import           UnliftIO.Exception    (bracketOnError, throwIO)
import           UnliftIO.IORef        (IORef, atomicModifyIORef', newIORef,
                                        readIORef, writeIORef)

-- | Options for determining behaviour of circuit breaking services.
data CircuitBreakerOptions = CircuitBreakerOptions
  { maxBreakerFailures :: Int
  -- ^ How many times the underlying service must fail in the given window before the circuit opens.
  , resetTimeoutSecs   :: Int
  -- ^ The window of time in which the underlying service must fail for the circuit to open.
  , breakerDescription :: String
  -- ^ Description that is attached to the failure so as to identify the particular circuit.
  }
  deriving (Eq, Show)

-- | Defaulted options for the circuit breaker with 3 failures over 60 seconds.
defaultCircuitBreakerOptions :: CircuitBreakerOptions
defaultCircuitBreakerOptions =
  CircuitBreakerOptions
    { maxBreakerFailures = 3
    , resetTimeoutSecs   = 60
    , breakerDescription = "Circuit breaker open."
    }

-- | Status indicating if the circuit is open.
data BreakerStatus
  = BreakerClosed Int
  | BreakerOpen Int
  deriving (Eq, Show)

-- | Representation of the state the circuit breaker is currently in.
newtype CircuitBreakerState = CircuitBreakerState [IORef BreakerStatus]

-- | Exception thrown when the circuit is open.
newtype CircuitBreakerException = CircuitBreakerException String
  deriving (Eq, Show)

instance Exception CircuitBreakerException

-- | Combines multiple states together.
combineCircuitBreakerStates :: Foldable t => t CircuitBreakerState -> CircuitBreakerState
combineCircuitBreakerStates = CircuitBreakerState . foldMap (\(CircuitBreakerState refs) -> refs)

-- | Determines if a specific status is open.
isStatusOpen :: BreakerStatus -> Bool
isStatusOpen (BreakerOpen _)   = True
isStatusOpen (BreakerClosed _) = False

-- | Determines if a specific status is closed.
isStatusClosed :: BreakerStatus -> Bool
isStatusClosed (BreakerOpen _)   = False
isStatusClosed (BreakerClosed _) = True

-- | Determines if a circuit breaker is open.
isCircuitBreakerOpen :: MonadIO m => CircuitBreakerState -> m Bool
isCircuitBreakerOpen (CircuitBreakerState states) =
  or <$> traverse (fmap isStatusOpen . readIORef) states

-- | Determines if a circuit breaker is closed.
isCircuitBreakerClosed :: MonadIO m => CircuitBreakerState -> m Bool
isCircuitBreakerClosed (CircuitBreakerState states) =
  and <$> traverse (fmap isStatusClosed . readIORef) states

-- | Circuit breaking services can be constructed with this function.
circuitBreaker
  :: (MonadUnliftIO m, MonadIO n)
  => CircuitBreakerOptions
  -- ^ Options for specifying the circuit breaker behaviour.
  -> BasicService m a b
  -- ^ Service to protect with the circuit breaker.
  -> n (CircuitBreakerState, BasicService m a b)
circuitBreaker options service = do
  let retrieveTime = liftIO $ round <$> getPOSIXTime
      failureMax = maxBreakerFailures options
      callIfClosed request ref = bracketOnError (pure ()) (\_ -> incErrors ref) (\_ -> service request)
      canaryCall request ref = do
        result <- callIfClosed request ref
        writeIORef ref $ BreakerClosed 0
        pure result
      incErrors ref = do
        currentTime <- retrieveTime
        atomicModifyIORef' ref $ \status -> case status of
          BreakerClosed errorCount ->
            ( if errorCount >= failureMax
                then BreakerOpen (currentTime + resetTimeoutSecs options)
                else BreakerClosed (errorCount + 1)
            , ()
            )
          other -> (other, ())
      failingCall = throwIO $ CircuitBreakerException $ breakerDescription options
      callIfOpen request ref = do
        currentTime <- retrieveTime
        canaryRequest <- atomicModifyIORef' ref $ \status -> case status of
          BreakerClosed _ -> (status, False)
          BreakerOpen time ->
            if currentTime > time
              then (BreakerOpen (currentTime + resetTimeoutSecs options), True)
              else (status, False)
        if canaryRequest then canaryCall request ref else failingCall
      breakerService ref request = do
        status <- readIORef ref
        case status of
          BreakerClosed _ -> callIfClosed request ref
          BreakerOpen _   -> callIfOpen request ref
  ref <- newIORef $ BreakerClosed 0
  pure (CircuitBreakerState [ref], breakerService ref)
