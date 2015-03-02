{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ServiceFabric.DogpileProtection where

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import ServiceFabric.Types
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.CatchIO

dogpileProtect :: (MonadCatchIO m, Eq a, Hashable a) => BasicService m a b -> m (IORef (HashMap a (MVar b)), BasicService m a b)
dogpileProtect service = do
  mapRef <- liftIO $ newIORef M.empty
  let protectedService request = do
                                    firstRequestMVar <- liftIO $ newEmptyMVar 
                                    resultAction <- liftIO $ atomicModifyIORef' mapRef (\refMap -> 
                                        let removeFromMap           = liftIO $ atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))
                                            invokeService           = do 
                                                                        result <- bracketOnError (return ()) (\_ -> removeFromMap) (\_ -> service request)
                                                                        liftIO $ putMVar firstRequestMVar result
                                                                        return result
                                            updateMap mvar          = (M.insert request mvar refMap, liftIO $ readMVar mvar)
                                            addToMap                = (M.insert request firstRequestMVar refMap, invokeService)
                                        in  maybe addToMap updateMap $ M.lookup request refMap)
                                    resultAction
  return (mapRef, protectedService)