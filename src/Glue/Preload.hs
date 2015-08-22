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
import qualified Control.Monad.Loops as L
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

-- | Options for determining behaviour of preloading services.
data PreloadedOptions a m n = PreloadedOptions {
  preloadedKeys             :: S.HashSet a,   -- ^ Keys to preload.
  preloadingRefreshTimeMs   :: Int,           -- ^ Amount of time between refreshes.
  executePreload            :: m () -> n ()   -- ^ How to run the preload action.
}

-- | Defaulted options for preloading a HashSet of keys with a 30 second refresh time.
defaultPreloadedOptions :: S.HashSet a -> (m () -> n ()) -> PreloadedOptions a m n
defaultPreloadedOptions toPreload executeP = PreloadedOptions {
  preloadedKeys           = toPreload,
  preloadingRefreshTimeMs = 30 * 1000,
  executePreload          = executeP
}

-- | Preloads the results of calls for given keys.
preloadingService :: (MonadIO m, MonadBaseControl IO m, MonadBaseControl IO n, Eq a, Hashable a)
                  => PreloadedOptions a m n          -- ^ Instance of 'PreloadedOptions' to configure the preloading functionality.
                  -> MultiGetService m a b           -- ^ The service to perform preloading of.
                  -> n (MultiGetService m a b, () -> n ())
preloadingService PreloadedOptions{..} service = do
  !preloadedVar <- newEmptyMVar
  !shouldContinueVar <- newMVar True
  let !updatePreloaded = do
                            result <- makeCall service preloadedKeys
                            _ <- tryTakeMVar preloadedVar
                            putMVar preloadedVar result
                            threadDelay (preloadingRefreshTimeMs * 1000)
  _ <- fork $ L.whileM_ (readMVar shouldContinueVar) $ executePreload $ updatePreloaded
  let plService request = do
                            let fromPreloadKeys = S.intersection request preloadedKeys
                            let fromServiceKeys = S.difference request preloadedKeys
                            !fromPreload <- if S.null fromPreloadKeys then return M.empty else fmap (M.filterWithKey (\k -> \_ -> S.member k fromPreloadKeys)) $ getResult preloadedVar 
                            !fromService <- if S.null fromServiceKeys then return M.empty else service fromServiceKeys
                            return $ M.union fromService fromPreload
  return (plService, \_ -> tryTakeMVar shouldContinueVar >> putMVar shouldContinueVar False)