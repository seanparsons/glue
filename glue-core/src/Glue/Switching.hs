{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing a switchable service, allowing the possibility of changing
-- a service "live".
module Glue.Switching
  ( switchingService
  ) where

import           Glue.Types
import           UnliftIO       (MonadIO)
import           UnliftIO.IORef (newIORef, readIORef, writeIORef)

-- | Provides a switchable service.
-- Works for both 'BasicService' and 'MultiGetService' (since 'MultiGetService' is a 'BasicService').
switchingService
  :: (MonadIO m, MonadIO n)
  => BasicService m a b
  -- ^ The service to initialise the switching service with.
  -> n (BasicService m a b, BasicService m a b -> n ())
switchingService service = do
  !serviceRef <- newIORef service
  let switching rs = do
        serviceToUse <- readIORef serviceRef
        serviceToUse rs
  let switchingUpdate newService = writeIORef serviceRef newService
  pure (switching, switchingUpdate)
