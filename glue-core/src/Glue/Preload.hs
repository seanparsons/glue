{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing a form of caching where values for given keys are preloaded ahead of time.
-- Once warmed up, requests for preloaded keys will be instant, with the values refreshed in the background.
module Glue.Preload
  ( PreloadedOptions (..)
  , PreloadStoppedBeforeExecutedException (..)
  , defaultPreloadedOptions
  , preloadingService
  , preloadingCall
  ) where

import           Control.Monad       (void)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.Maybe          (fromJust)
import           Glue.Types
import           UnliftIO            (Exception, MonadIO, MonadUnliftIO,
                                      SomeException (..), liftIO)
import           UnliftIO.Async      (race_)
import           UnliftIO.Concurrent (forkIO, threadDelay)
import           UnliftIO.Exception  (throwIO)
import           UnliftIO.IORef      (IORef, atomicModifyIORef', newIORef,
                                      readIORef)
import           UnliftIO.MVar       (MVar, newEmptyMVar, readMVar, tryPutMVar)

-- | Options for determining behaviour of preloading services.
data PreloadedOptions m a b = PreloadedOptions
  { preloadedKeys           :: S.HashSet a
  -- ^ Keys to preload.
  , preloadingRefreshTimeMs :: Int
  -- ^ Amount of time between refreshes.
  , preloadingRun           :: MToIO m
  -- ^ Get an 'IO' of the response for the caching.
  }

-- | Defaulted options for preloading a 'S.HashSet' of keys with a 30 second refresh time.
defaultPreloadedOptions :: S.HashSet a -> MToIO m -> PreloadedOptions m a b
defaultPreloadedOptions toPreload pRun =
  PreloadedOptions
    { preloadedKeys           = toPreload
    , preloadingRefreshTimeMs = 30 * 1000
    , preloadingRun           = pRun
    }

data PreloadedState a b
  = PreloadedStarted
  | PreloadedWithResult (FailOrSuccess a b) Bool

-- | Exception thrown when preloading is stopped before the initial preload has executed.
data PreloadStoppedBeforeExecutedException = PreloadStoppedBeforeExecutedException
  deriving (Eq, Show)

instance Exception PreloadStoppedBeforeExecutedException

modifyStateWithResult :: Either SomeException (MultiGetResponse a b) -> PreloadedState a b -> (PreloadedState a b, Bool)
modifyStateWithResult result PreloadedStarted          = (PreloadedWithResult result True, True)
modifyStateWithResult result (PreloadedWithResult _ c) = (PreloadedWithResult result c, c)

applyResultToState :: MonadIO m => IORef (PreloadedState a b) -> MVar () -> FailOrSuccess a b -> m Bool
applyResultToState stateRef readyVar result = do
  continue <- atomicModifyIORef' stateRef $ modifyStateWithResult result
  void $ tryPutMVar readyVar ()
  pure continue

waitForResult :: MonadIO m => IORef (PreloadedState a b) -> MVar () -> m (MultiGetResponse a b)
waitForResult stateRef readyVar = do
  readMVar readyVar
  state <- readIORef stateRef
  case state of
    PreloadedStarted                      -> throwIO PreloadStoppedBeforeExecutedException
    PreloadedWithResult (Right success) _ -> pure success
    PreloadedWithResult (Left failure)  _ -> throwIO failure

markAsFinished :: PreloadedState a b -> (PreloadedState a b, ())
markAsFinished PreloadedStarted          = (PreloadedWithResult (Left (SomeException PreloadStoppedBeforeExecutedException)) False, ())
markAsFinished (PreloadedWithResult r _) = (PreloadedWithResult r False, ())

-- | Preloads the results of calls for given keys.
preloadingService
  :: forall m n a b.
     (MonadUnliftIO m, MonadIO n, Hashable a)
  => PreloadedOptions m a b
  -- ^ Instance of t'PreloadedOptions' to configure the preloading functionality.
  -> MultiGetService m a b
  -- ^ The service to perform preloading of.
  -> n (MultiGetService m a b, () -> n ())
preloadingService PreloadedOptions{..} service = do
  !stateIORef <- newIORef PreloadedStarted
  !readyVar   <- newEmptyMVar
  !stopVar    <- newEmptyMVar
  let stop _ = do
        atomicModifyIORef' stateIORef markAsFinished
        void $ tryPutMVar readyVar ()
        void $ tryPutMVar stopVar ()
  let runUpdate = do
        result <- makeCall service preloadedKeys
        applyResultToState stateIORef readyVar result
  let updatePreloaded = do
        continue <- liftIO $ preloadingRun runUpdate
        if continue
          then do
            stopped <- liftIO $ raceWait stopVar (preloadingRefreshTimeMs * 1000)
            if stopped then pure () else updatePreloaded
          else pure ()
  let plService request = do
        let fromPreloadKeys = S.intersection request preloadedKeys
        let fromServiceKeys = S.difference request preloadedKeys
        !fromPreload <-
          if S.null fromPreloadKeys
            then pure M.empty
            else M.filterWithKey (\k _ -> S.member k fromPreloadKeys) <$> waitForResult stateIORef readyVar
        !fromService <-
          if S.null fromServiceKeys
            then pure M.empty
            else service fromServiceKeys
        pure $ M.union fromService fromPreload
  void $ liftIO $ forkIO updatePreloaded
  pure (plService, stop)
  where
    raceWait :: MVar () -> Int -> IO Bool
    raceWait stopVar delayUs = do
      race_ (readMVar stopVar) (threadDelay delayUs)
      not <$> tryPutMVar stopVar ()

-- | Preloads a single parameter-less call.
preloadingCall
  :: forall m n b.
     (MonadUnliftIO m, MonadIO n)
  => MToIO m
  -- ^ Get an 'IO' of the call for the caching.
  -> Int
  -- ^ Amount of time between refreshes.
  -> m b
  -- ^ The call to perform preloading of.
  -> n (m b, () -> n ())
preloadingCall toIO timeBetweenRefreshes call = do
  let multiGetService = basicToMultiGet (\_ -> call) :: MultiGetService m () b
  let options = PreloadedOptions (S.singleton ()) timeBetweenRefreshes toIO
  (preloading, stop) <- preloadingService options multiGetService
  let preloadingBasicService = multiGetToBasic preloading
  let preloadingCallResult = fromJust <$> preloadingBasicService ()
  pure (preloadingCallResult, stop)
