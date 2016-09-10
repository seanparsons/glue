{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Module containing a form of caching where values for given keys are preloaded ahead of time.
-- | Once warmed up requests for preloaded keys will be instant, with the values refreshed in the background.
module Glue.Preload(
    PreloadedOptions
  , defaultPreloadedOptions
  , preloadingService
  , preloadingCall
  , preloadedKeys
  , preloadingRefreshTimeMs
  , preloadingRun
) where

import Data.Maybe
import Glue.Types
import Data.Hashable
import Data.Typeable
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Control.Concurrent.Lifted
import Control.Exception.Base hiding (throwIO)
import Control.Exception.Lifted
import Data.IORef.Lifted
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

-- | Options for determining behaviour of preloading services.
data PreloadedOptions m a b = PreloadedOptions {
  preloadedKeys             :: S.HashSet a,       -- ^ Keys to preload.
  preloadingRefreshTimeMs   :: Int,               -- ^ Amount of time between refreshes.
  preloadingRun             :: MToIO m            -- ^ Get an IO of the response for the caching.
}

-- | Defaulted options for preloading a HashSet of keys with a 30 second refresh time.
defaultPreloadedOptions :: S.HashSet a -> MToIO m -> PreloadedOptions m a b
defaultPreloadedOptions toPreload pRun = PreloadedOptions {
  preloadedKeys           = toPreload,
  preloadingRefreshTimeMs = 30 * 1000,
  preloadingRun           = pRun
}

data PreloadedState a b = PreloadedStarted
                        | PreloadedWithResult (FailOrSuccess a b) Bool

data PreloadStoppedBeforeExecutedException = PreloadStoppedBeforeExecutedException deriving (Eq, Show, Typeable)
instance Exception PreloadStoppedBeforeExecutedException

modifyStateWithResult :: forall a b . Either SomeException (MultiGetResponse a b) -> PreloadedState a b -> (PreloadedState a b, Bool)
modifyStateWithResult result PreloadedStarted           = (PreloadedWithResult result True, True)
modifyStateWithResult result (PreloadedWithResult _ c)  = (PreloadedWithResult result c, c)

applyResultToState :: forall m a b . MonadBaseControl IO m => IORef (PreloadedState a b) -> FailOrSuccess a b -> m Bool
applyResultToState stateRef result = atomicModifyIORef' stateRef $ modifyStateWithResult result

waitForResult :: forall m a b . (MonadBaseControl IO m) => IORef (PreloadedState a b) -> m (MultiGetResponse a b)
waitForResult stateRef = do
                            state <- readIORef stateRef
                            let tryAgainLater = threadDelay 1000 >> waitForResult stateRef
                            case state of
                                          PreloadedStarted                      -> tryAgainLater
                                          PreloadedWithResult (Right success) _ -> return success
                                          PreloadedWithResult (Left failure)  _ -> throwIO failure

markAsFinished :: PreloadedState a b -> (PreloadedState a b, ())
markAsFinished PreloadedStarted = (PreloadedWithResult (Left (SomeException PreloadStoppedBeforeExecutedException)) False, ())
markAsFinished (PreloadedWithResult r _) = (PreloadedWithResult r False, ())

-- | Preloads the results of calls for given keys.
preloadingService :: forall m n a b . (MonadIO m, MonadIO n, MonadBaseControl IO m, MonadBaseControl IO n, Eq a, Hashable a, Show a)
                  => PreloadedOptions m a b      -- ^ Instance of 'PreloadedOptions' to configure the preloading functionality.
                  -> MultiGetService m a b       -- ^ The service to perform preloading of.
                  -> n (MultiGetService m a b, () -> n ())
preloadingService PreloadedOptions{..} service = do
  !stateIORef <- newIORef PreloadedStarted
  let stop _ = atomicModifyIORef' stateIORef markAsFinished
  let runUpdate = do
                    result <- makeCall service preloadedKeys
                    applyResultToState stateIORef result
  let updatePreloaded = do
                          continue <- liftIO $ preloadingRun $ runUpdate
                          if continue then threadDelay (preloadingRefreshTimeMs * 1000) >> updatePreloaded else return ()
  let plService request = do
                            let fromPreloadKeys = S.intersection request preloadedKeys
                            let fromServiceKeys = S.difference request preloadedKeys
                            !fromPreload <- if S.null fromPreloadKeys then return M.empty else fmap (M.filterWithKey (\k -> \_ -> S.member k fromPreloadKeys)) $ waitForResult stateIORef
                            !fromService <- if S.null fromServiceKeys then return M.empty else service fromServiceKeys
                            return $ M.union fromService fromPreload
  fork updatePreloaded
  return (plService, stop)

-- | Preloads a single parameter-less call.
preloadingCall :: forall m n b . (MonadIO m, MonadIO n, MonadBaseControl IO m, MonadBaseControl IO n)
               => MToIO m                     -- ^ Get an IO of the call for the caching.
               -> Int                         -- ^ Amount of time between refreshes.
               -> m b                         -- ^ The call to perform preloading of.
               -> n (m b, () -> n ())
preloadingCall toIO timeBetweenRefreshes call = do
  let multiGetService = basicToMultiGet (\_ -> call) :: MultiGetService m () b
  let options = PreloadedOptions (S.singleton ()) timeBetweenRefreshes toIO
  (preloading, stop) <- preloadingService options multiGetService
  let preloadingBasicService = multiGetToBasic preloading
  let preloadingCallResult = fmap (fromJust) $ preloadingBasicService ()
  return (preloadingCallResult, stop)
