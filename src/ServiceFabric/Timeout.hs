{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ServiceFabric.Timeout where

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import ServiceFabric.Types
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.CatchIO

addTimeout :: Int -> BasicService m a b -> BasicService m a b
addTimeout timeoutMs service = do
  mvar <- liftIO $ newEmptyMVar
  liftIO $ forkIO $ 