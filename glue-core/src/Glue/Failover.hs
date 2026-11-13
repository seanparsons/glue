{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting failover: either re-invoking one service with a transformed request each
-- time a failure occurs ('failover'), or trying an ordered list of distinct backends, a primary
-- then its backups, until one answers ('failoverAcross').
module Glue.Failover
  ( FailoverOptions (..)
  , defaultFailoverOptions
  , failover
  , FailoverAcrossOptions (..)
  , ReportedFailure (..)
  , defaultFailoverAcrossOptions
  , failoverAcross
  , failoverAcrossIndexed
  , failoverAcrossEither
  , failoverAcrossIndexedEither
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (fromMaybe)
import           Glue.Failure
import           Glue.Types
import           UnliftIO           (MonadUnliftIO, SomeException)
import           UnliftIO.Exception (catchAny)

-- | Options for determining behaviour of failover services.
data FailoverOptions a = FailoverOptions
  { maxFailovers             :: Int
  -- ^ The maximum number of times the service will failover and make another attempt.
  , transformFailoverRequest :: a -> a
  -- ^ Each time the service performs a failover, transform the request with this function.
  }

-- | Simple defaults that result in retrying 3 times immediately without transforming the request.
defaultFailoverOptions :: FailoverOptions a
defaultFailoverOptions =
  FailoverOptions
    { maxFailovers             = 3
    , transformFailoverRequest = id
    }

-- | Creates a service that can transform requests with each subsequent failure.
failover
  :: MonadUnliftIO m
  => FailoverOptions a
  -- ^ Instance of t'FailoverOptions' to configure the failover functionality.
  -> BasicService m a b
  -- ^ The service to perform failover around.
  -> BasicService m a b
failover options service =
  let invokeService failCount request =
        let afterFail _ = invokeService (failCount + 1) (transformFailoverRequest options request)
            invoke      = service request
        in  if failCount >= maxFailovers options
              then invoke
              else catchAny invoke afterFail
  in  invokeService 0

-- | Which failure is reported when every backend has failed.
data ReportedFailure
  = ReportFirstFailure
  -- ^ The primary's failure.
  | ReportLastFailure
  -- ^ The last backup's failure.
  deriving (Eq, Show)

-- | Options for failing over across distinct backends.
data FailoverAcrossOptions failure = FailoverAcrossOptions
  { advanceOn       :: failure -> Bool
  -- ^ Whether a failure moves on to the next backend. A failure that doesn't (an invalid request,
  -- say, which a backup would only repeat) stops the chain and is reported at once.
  , reportedFailure :: ReportedFailure
  -- ^ Which failure is reported when every backend has failed.
  }

-- | Defaults that advance on every failure and report the last one.
defaultFailoverAcrossOptions :: FailoverAcrossOptions failure
defaultFailoverAcrossOptions =
  FailoverAcrossOptions
    { advanceOn       = const True
    , reportedFailure = ReportLastFailure
    }

failoverAcrossVia
  :: Monad m
  => FailureMode m r failure
  -> FailoverAcrossOptions failure
  -> NonEmpty (BasicService m a r)
  -> BasicService m a (Int, r)
failoverAcrossVia mode options (primary :| backups) request = go 0 Nothing primary backups
  where
    report index failure = (,) index <$> reraise mode failure
    go index firstFailure service rest = do
      outcome <- observeCall mode (service request)
      case outcome of
        Right result -> pure (index, result)
        Left failure
          | not (advanceOn options failure) -> report index failure
          | otherwise -> case rest of
              next : rest' -> go (index + 1) (Just (fromMaybe (index, failure) firstFailure)) next rest'
              [] -> case (reportedFailure options, firstFailure) of
                (ReportFirstFailure, Just (firstIndex, first)) -> report firstIndex first
                _                                              -> report index failure

-- | Creates a service that tries each backend in order and returns the first success.
-- Asynchronous exceptions always propagate and never advance to the next backend.
failoverAcross
  :: MonadUnliftIO m
  => FailoverAcrossOptions SomeException
  -- ^ Options deciding which failures advance and which failure is reported.
  -> NonEmpty (BasicService m a b)
  -- ^ The backends, primary first.
  -> BasicService m a b
failoverAcross options services = fmap snd . failoverAcrossIndexed options services

-- | Variant of 'failoverAcross' that also reports the index of the backend that answered,
-- for tracing and billing.
failoverAcrossIndexed
  :: MonadUnliftIO m
  => FailoverAcrossOptions SomeException
  -- ^ Options deciding which failures advance and which failure is reported.
  -> NonEmpty (BasicService m a b)
  -- ^ The backends, primary first.
  -> BasicService m a (Int, b)
failoverAcrossIndexed = failoverAcrossVia exceptionFailures

-- | Variant of 'failoverAcross' for services returning failures as values: @Left@ results are
-- judged by 'advanceOn'. Exceptions are not caught and do not advance.
failoverAcrossEither
  :: Monad m
  => FailoverAcrossOptions e
  -- ^ Options deciding which failures advance and which failure is reported.
  -> NonEmpty (EitherService m a e b)
  -- ^ The backends, primary first.
  -> EitherService m a e b
failoverAcrossEither options services = fmap (fmap snd) . failoverAcrossIndexedEither options services

-- | Variant of 'failoverAcrossEither' that also reports the index of the backend that answered.
failoverAcrossIndexedEither
  :: Monad m
  => FailoverAcrossOptions e
  -- ^ Options deciding which failures advance and which failure is reported.
  -> NonEmpty (EitherService m a e b)
  -- ^ The backends, primary first.
  -> EitherService m a e (Int, b)
failoverAcrossIndexedEither options services =
  fmap (\(index, result) -> (,) index <$> result) . failoverAcrossVia valueFailures options services
