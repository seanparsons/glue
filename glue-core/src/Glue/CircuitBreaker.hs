{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing circuit breaker functionality, which is the ability to open a circuit once a number of failures have occurred, thereby preventing later calls from attempting to make unsuccessful calls.
-- | Often this is useful if the underlying service were to repeatedly time out, so as to reduce the number of calls inflight holding up upstream callers.
module Glue.CircuitBreaker(
    CircuitBreakerOptions
  , CircuitBreakerState
  , CircuitBreakerException(..)
  , combineCircuitBreakerStates
  , isCircuitBreakerOpen
  , isCircuitBreakerClosed
  , defaultCircuitBreakerOptions
  , circuitBreaker
  , maxBreakerFailures
  , resetTimeoutSecs
  , breakerDescription
) where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.IORef.Lifted
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Glue.Types

-- | Options for determining behaviour of circuit breaking services.
data CircuitBreakerOptions = CircuitBreakerOptions {
    maxBreakerFailures :: Int        -- ^ How many times the underlying service must fail in the given window before the circuit opens.
  , resetTimeoutSecs   :: Int        -- ^ The window of time in which the underlying service must fail for the circuit to open.
  , breakerDescription :: String     -- ^ Description that is attached to the failure so as to identify the particular circuit.
}

-- | Defaulted options for the circuit breaker with 3 failures over 60 seconds.
defaultCircuitBreakerOptions :: CircuitBreakerOptions
defaultCircuitBreakerOptions = CircuitBreakerOptions { maxBreakerFailures = 3, resetTimeoutSecs = 60, breakerDescription = "Circuit breaker open." }

-- | Status indicating if the circuit is open.
data BreakerStatus = BreakerClosed Int | BreakerOpen Int deriving (Eq, Show)

-- | Representation of the state the circuit breaker is currently in.
data CircuitBreakerState = CircuitBreakerState [IORef BreakerStatus]

-- | Exception thrown when the circuit is open.
data CircuitBreakerException = CircuitBreakerException String deriving (Eq, Show, Typeable)
instance Exception CircuitBreakerException

-- | Combines multiple states together.
combineCircuitBreakerStates :: (Foldable t) => t CircuitBreakerState -> CircuitBreakerState
combineCircuitBreakerStates states = CircuitBreakerState $ foldMap (\(CircuitBreakerState refs) -> refs) states

-- | Determines if a specific status is open.
isStatusOpen :: BreakerStatus -> Bool
isStatusOpen (BreakerOpen _)   = True
isStatusOpen (BreakerClosed _) = False

-- | Determines if a specific status is closed.
isStatusClosed :: BreakerStatus -> Bool
isStatusClosed (BreakerOpen _)   = False
isStatusClosed (BreakerClosed _) = True

-- | Determines if a circuit breaker is open.
isCircuitBreakerOpen :: (MonadBaseControl IO m) => CircuitBreakerState -> m Bool
isCircuitBreakerOpen (CircuitBreakerState states) = fmap or $ traverse (\ref -> fmap isStatusOpen $ readIORef ref) states

-- | Determines if a circuit breaker is closed.
isCircuitBreakerClosed :: (MonadBaseControl IO m) => CircuitBreakerState -> m Bool
isCircuitBreakerClosed (CircuitBreakerState states) = fmap and $ traverse (\ref -> fmap isStatusClosed $ readIORef ref) states

-- TODO: Check that values within m aren't lost on a successful call.
-- | Circuit breaking services can be constructed with this function.
circuitBreaker :: (MonadBaseControl IO m, MonadBaseControl IO n)
               => CircuitBreakerOptions       -- ^ Options for specifying the circuit breaker behaviour.
               -> BasicService m a b          -- ^ Service to protect with the circuit breaker.
               -> n (CircuitBreakerState, BasicService m a b)
circuitBreaker options service =
  let retrieveTime                = liftBase $ round `fmap` getPOSIXTime
      failureMax                  = maxBreakerFailures options
      callIfClosed request ref    = bracketOnError (return ()) (\_ -> incErrors ref) (\_ -> service request)
      canaryCall request ref      = do
                                      result <- callIfClosed request ref
                                      writeIORef ref $ BreakerClosed 0
                                      return result
      incErrors ref               = do
                                      currentTime <- retrieveTime
                                      atomicModifyIORef' ref $ \status -> case status of
                                        (BreakerClosed errorCount) -> (if errorCount >= failureMax then BreakerOpen (currentTime + (resetTimeoutSecs options)) else BreakerClosed (errorCount + 1), ())
                                        other                             -> (other, ())
      failingCall                 = throw $ CircuitBreakerException $ breakerDescription options
      callIfOpen request ref      = do
                                      currentTime <- retrieveTime
                                      canaryRequest <- atomicModifyIORef' ref $ \status -> case status of
                                                              (BreakerClosed _)  -> (status, False)
                                                              (BreakerOpen time) -> if currentTime > time then ((BreakerOpen (currentTime + (resetTimeoutSecs options))), True) else (status, False)
                                      if canaryRequest then canaryCall request ref else failingCall
      breakerService ref request  = do
                                      status <- readIORef ref
                                      case status of
                                        (BreakerClosed _)  -> callIfClosed request ref
                                        (BreakerOpen _)    -> callIfOpen request ref

  in do
        ref <- newIORef $ BreakerClosed 0
        return (CircuitBreakerState [ref], breakerService ref)
