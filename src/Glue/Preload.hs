{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module containing a form of caching where values for given keys are preloaded ahead of time.
-- | Once warmed up requests for preloaded keys will be instant, with the values refreshed in the background.
module Glue.Preload(
    PreloadedOptions
  , defaultPreloadedOptions
  , preloadingService
) where

import Glue.Types
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Control.Concurrent.Lifted
import Control.Exception.Base hiding (throwIO)
import Control.Exception.Lifted
import Data.IORef.Lifted
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

-- | Options for determining behaviour of preloading services.
data PreloadedOptions a = PreloadedOptions {
  preloadedKeys             :: S.HashSet a,   -- ^ Keys to preload.
  preloadingRefreshTimeMs   :: Int            -- ^ Amount of time between refreshes.
}

-- | Defaulted options for preloading a HashSet of keys with a 30 second refresh time.
defaultPreloadedOptions :: S.HashSet a -> PreloadedOptions a
defaultPreloadedOptions toPreload = PreloadedOptions {
  preloadedKeys           = toPreload,
  preloadingRefreshTimeMs = 30 * 1000
}

data PreloadedState a b = PreloadedNotStarted
                        | PreloadedStarted
                        | PreloadedWithResult (Either SomeException (MultiGetResponse a b))
                        | PreloadedFinished

updatePreloadState :: PreloadedState a b -> (PreloadedState a b, Bool)
updatePreloadState PreloadedNotStarted  = (PreloadedStarted, True)
updatePreloadState state                = (state, False)

applyResultToState :: MonadBaseControl IO m => IORef (PreloadedState a b) -> Either SomeException (MultiGetResponse a b) -> m Bool
applyResultToState stateRef result = do
  state <- readIORef stateRef
  case state of
                PreloadedFinished   -> return False
                PreloadedNotStarted -> return True
                _                   -> (writeIORef stateRef $ PreloadedWithResult result) >> return True

waitForResult :: MonadBaseControl IO m => IORef (PreloadedState a b) -> m (MultiGetResponse a b)
waitForResult stateRef = do
                            state <- readIORef stateRef
                            let tryAgainLater = threadDelay 1000 >> waitForResult stateRef
                            case state of
                                          PreloadedNotStarted                     -> tryAgainLater
                                          PreloadedStarted                        -> tryAgainLater
                                          PreloadedWithResult (Right success)     -> return success
                                          PreloadedWithResult (Left failure)      -> throwIO failure
                                          PreloadedFinished                       -> throwIO $ AssertionFailed "Invalid State"

-- | Preloads the results of calls for given keys.
preloadingService :: forall m n a b . (MonadIO m, MonadBaseControl IO m, MonadBaseControl IO n, Eq a, Hashable a)
                  => PreloadedOptions a          -- ^ Instance of 'PreloadedOptions' to configure the preloading functionality.
                  -> MultiGetService m a b       -- ^ The service to perform preloading of.
                  -> n (MultiGetService m a b, () -> n ())
preloadingService PreloadedOptions{..} service = do
  !stateIORef <- newIORef PreloadedNotStarted
  let stop _ = writeIORef stateIORef PreloadedFinished
  let updatePreloaded = do
                          result <- makeCall service preloadedKeys
                          continue <- applyResultToState stateIORef result
                          if continue then (threadDelay (preloadingRefreshTimeMs * 1000) >> updatePreloaded) else return ()
  let plService request = do
                            shouldStart <- atomicModifyIORef' stateIORef updatePreloadState
                            if shouldStart then fork updatePreloaded >> return () else return ()
                            let fromPreloadKeys = S.intersection request preloadedKeys
                            let fromServiceKeys = S.difference request preloadedKeys
                            !fromPreload <- if S.null fromPreloadKeys then return M.empty else fmap (M.filterWithKey (\k -> \_ -> S.member k fromPreloadKeys)) $ waitForResult stateIORef
                            !fromService <- if S.null fromServiceKeys then return M.empty else service fromServiceKeys
                            return $ M.union fromService fromPreload
  return (plService, stop)
