{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing a switchable service, allowing the possibility of changing
-- | a service "live".
module Glue.Switching(
    switchingService
) where

import           Control.Monad.Base
import           Data.IORef.Lifted
import           Glue.Types

-- | Provides a switchable service.
switchingService :: (MonadBase IO m, MonadBase IO n) => MultiGetService m a b  -- ^ The service to initialise the switching service with.
                 -> n (MultiGetService m a b, (MultiGetService m a b) -> n ())
switchingService service = do
  !serviceRef <- newIORef service
  let switching rs = do
                        serviceToUse <- readIORef serviceRef
                        serviceToUse rs
  let switchingUpdate newService = writeIORef serviceRef newService
  return (switching, switchingUpdate)
