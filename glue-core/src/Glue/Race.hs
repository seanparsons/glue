{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module providing a low-level race of concurrent attempts, for callers that make their own
-- decisions about when to launch another attempt and what to do as each one ends (see
-- 'Glue.Hedging.hedgeAcross' for a ready-made policy built on it).
--
-- Attempts are numbered from 0 in launch order. 'awaitNext' reports each attempt's ending once,
-- earliest index first when several have ended. 'abandon' cancels an attempt and waits until it
-- has actually finished, reporting how it really ended: an attempt that completed just before the
-- cancel reports its result. Leaving 'withRace' abandons every attempt still running.
module Glue.Race
  ( Race
  , AttemptOutcome (..)
  , RaceEvent (..)
  , withRace
  , launch
  , awaitNext
  , abandon
  , abandonAll
  , finishedAttempts
  ) where

import           Control.Monad      (filterM)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (catMaybes, fromMaybe)
import           Glue.Clock
import           Glue.Failure
import           UnliftIO           (Async, AsyncCancelled (..), MonadUnliftIO,
                                     SomeException, fromException)
import           UnliftIO.Async     (asyncWithUnmask, cancel, pollSTM,
                                     waitCatch)
import           UnliftIO.Exception (bracket, mask_)
import           UnliftIO.STM       (TVar, atomically, modifyTVar', newTVarIO,
                                     orElse, readTVar, retrySTM, stateTVar)

-- | How an attempt ended.
data AttemptOutcome failure r
  = AttemptAnswered r
  -- ^ The attempt returned a result.
  | AttemptFailed failure
  -- ^ The attempt failed, as seen by the race's failure mode.
  | AttemptCancelled
  -- ^ The attempt was abandoned before it ended.
  | AttemptCrashed SomeException
  -- ^ The attempt threw outside its failure mode: an asynchronous exception, or any exception
  -- from a service whose failures are values.
  deriving (Show)

-- | What 'awaitNext' observed.
data RaceEvent failure r
  = AttemptEnded Int (AttemptOutcome failure r)
  -- ^ The attempt with this index ended.
  | RaceTimedOut
  -- ^ The limit passed with no attempt ending.
  | NothingRunning
  -- ^ Every launched attempt has already been reported or abandoned.
  deriving (Show)

-- | A race of attempts, each producing an @r@ whose failures are seen through a t'FailureMode'.
data Race m failure r = Race
  { raceMode     :: FailureMode m r failure
  , raceClock    :: Clock m
  , raceRunning  :: TVar (M.Map Int (Async (Either failure r)))
  -- ^ Attempts not yet reported or abandoned.
  , raceEnded    :: TVar (M.Map Int (AttemptOutcome failure r))
  , raceNextId   :: TVar Int
  }

toOutcome :: Either SomeException (Either failure r) -> AttemptOutcome failure r
toOutcome (Right (Right result)) = AttemptAnswered result
toOutcome (Right (Left failure)) = AttemptFailed failure
toOutcome (Left e) = case fromException e of
  Just AsyncCancelled -> AttemptCancelled
  Nothing             -> AttemptCrashed e

-- | Runs a race, abandoning any attempts still running when the body finishes or throws.
withRace :: MonadUnliftIO m => FailureMode m r failure -> Clock m -> (Race m failure r -> m x) -> m x
withRace mode clock = bracket create abandonAll
  where
    create = Race mode clock <$> newTVarIO M.empty <*> newTVarIO M.empty <*> newTVarIO 0

-- | Launches an attempt, returning its index.
launch :: MonadUnliftIO m => Race m failure r -> m r -> m Int
launch race action = mask_ $ do
  attempt <- asyncWithUnmask $ \unmask -> unmask (observeCall (raceMode race) action)
  atomically $ do
    index <- stateTVar (raceNextId race) (\i -> (i, i + 1))
    modifyTVar' (raceRunning race) (M.insert index attempt)
    pure index

-- | Waits for the next attempt to end, for at most the given number of milliseconds if given
-- (zero or less only checks). When several have ended, the lowest index is reported first.
awaitNext :: MonadUnliftIO m => Race m failure r -> Maybe Int -> m (RaceEvent failure r)
awaitNext race limitMs =
  case limitMs of
    Nothing -> atomically next
    Just ms
      | ms <= 0   -> atomically (next `orElse` pure RaceTimedOut)
      | otherwise -> fromMaybe RaceTimedOut <$> clockTimeout (raceClock race) (ms * 1000) (atomically next)
  where
    next = do
      running <- readTVar (raceRunning race)
      if M.null running
        then pure NothingRunning
        else do
          polled <- traverse (\(index, attempt) -> fmap ((,) index) <$> pollSTM attempt) (M.toAscList running)
          case catMaybes polled of
            [] -> retrySTM
            (index, result) : _ -> do
              let outcome = toOutcome result
              modifyTVar' (raceRunning race) (M.delete index)
              modifyTVar' (raceEnded race) (M.insert index outcome)
              pure (AttemptEnded index outcome)

-- | Cancels an attempt and waits until it has finished, returning how it really ended. An
-- attempt already reported returns its reported outcome; an index never launched reports
-- 'AttemptCancelled'.
abandon :: MonadUnliftIO m => Race m failure r -> Int -> m (AttemptOutcome failure r)
abandon race index = do
  (ended, running) <- atomically $ (,) <$> (M.lookup index <$> readTVar (raceEnded race))
                                       <*> (M.lookup index <$> readTVar (raceRunning race))
  case (ended, running) of
    (Just outcome, _)    -> pure outcome
    (Nothing, Nothing)   -> pure AttemptCancelled
    (Nothing, Just attempt) -> do
      cancel attempt
      outcome <- toOutcome <$> waitCatch attempt
      atomically $ do
        modifyTVar' (raceRunning race) (M.delete index)
        modifyTVar' (raceEnded race) (M.insert index outcome)
      pure outcome

-- | Abandons every attempt not yet reported, in index order, returning how each ended.
abandonAll :: MonadUnliftIO m => Race m failure r -> m [(Int, AttemptOutcome failure r)]
abandonAll race = do
  running <- atomically $ M.keys <$> readTVar (raceRunning race)
  traverse (\index -> (,) index <$> abandon race index) running

-- | Indices of attempts that have ended but not yet been reported by 'awaitNext'.
finishedAttempts :: MonadUnliftIO m => Race m failure r -> m [Int]
finishedAttempts race = atomically $ do
  running <- M.toAscList <$> readTVar (raceRunning race)
  fmap fst <$> filterM (fmap (maybe False (const True)) . pollSTM . snd) running
