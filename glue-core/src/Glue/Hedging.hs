{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting request hedging (duplication), dispatching multiple concurrent
-- requests (optionally staggered) and completing with the first successful result.
module Glue.Hedging
  ( HedgingOptions (..)
  , defaultHedgingOptions
  , hedgingService
  ) where

import           Control.Monad       (when)
import           Data.Maybe          (fromMaybe)
import           Glue.Types
import           UnliftIO            (Async, MonadUnliftIO, SomeException,
                                      withAsync)
import           UnliftIO.Concurrent (threadDelay)
import           UnliftIO.Exception  (throwIO, tryAny)
import           UnliftIO.STM        (atomically, newTVarIO, readTVar, retrySTM,
                                      writeTVar)

-- | Options for configuring hedged (duplicated) service calls.
data HedgingOptions = HedgingOptions
  { hedgingCount   :: Int
  -- ^ Total number of concurrent attempts to make (including the primary call).
  , hedgingDelayMs :: Int
  -- ^ Stagger delay in milliseconds before each subsequent hedged call (`0` to fire all simultaneously).
  }
  deriving (Eq, Show)

-- | Default hedging options that fire 2 concurrent requests with no stagger delay.
defaultHedgingOptions :: HedgingOptions
defaultHedgingOptions =
  HedgingOptions
    { hedgingCount   = 2
    , hedgingDelayMs = 0
    }

withAsyncs :: MonadUnliftIO m => [m a] -> ([Async a] -> m b) -> m b
withAsyncs []     k = k []
withAsyncs (m:ms) k = withAsync m $ \a -> withAsyncs ms (\as -> k (a : as))

-- | Creates a service that duplicates requests up to 'hedgingCount' times and returns
-- the first successful response, automatically cancelling remaining in-flight attempts.
-- If all attempts fail, the first encountered exception is rethrown.
hedgingService
  :: forall m a b.
     MonadUnliftIO m
  => HedgingOptions
  -- ^ Options configuring the number of hedged calls and stagger delay.
  -> BasicService m a b
  -- ^ Underlying service to invoke.
  -> BasicService m a b
hedgingService options service request
  | hedgingCount options <= 1 = service request
  | otherwise = do
      let total     = hedgingCount options
          delayUs   = max 0 (hedgingDelayMs options) * 1000
      stateVar <- newTVarIO (Nothing :: Maybe b, total, Nothing :: Maybe SomeException)
      let runAttempt :: Int -> m ()
          runAttempt idx = do
            when (idx > 0 && delayUs > 0) $
              threadDelay (idx * delayUs)
            res <- tryAny (service request)
            atomically $ do
              (mWin, remaining, mErr) <- readTVar stateVar
              case res of
                Right val -> writeTVar stateVar (Just (fromMaybe val mWin), remaining - 1, mErr)
                Left err  -> writeTVar stateVar (mWin, remaining - 1, Just (fromMaybe err mErr))
      withAsyncs [runAttempt i | i <- [0 .. total - 1]] $ \_ -> do
        outcome <- atomically $ do
          (mWin, remaining, mErr) <- readTVar stateVar
          case mWin of
            Just val -> pure (Right val)
            Nothing
              | remaining <= 0 -> pure (Left mErr)
              | otherwise      -> retrySTM
        case outcome of
          Right val        -> pure val
          Left (Just err)  -> throwIO err
          Left Nothing     -> service request
