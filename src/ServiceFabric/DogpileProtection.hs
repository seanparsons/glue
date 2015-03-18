{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServiceFabric.DogpileProtection(
  dogpileProtect
) where

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import ServiceFabric.Types
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.CatchIO

-- Loses the values held within m.
-- Should make this return just BasicService, hiding the HashMap.
-- Need sharding support.
dogpileProtect :: (MonadCatchIO m, Eq a, Hashable a) => BasicService m a b -> m (IORef (HashMap a (ResultVar b)), BasicService m a b)
dogpileProtect service = do
  mapRef <- liftIO $ newIORef M.empty
  let protectedService request = do
                                    firstRequestMVar <- liftIO $ newEmptyMVar 
                                    resultAction <- liftIO $ atomicModifyIORef' mapRef (\refMap -> 
                                        let removeFromMap           = liftIO $ atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))

                                            invokeService           = do 
                                                                        result <- bracketOnError (return ()) (\_ -> removeFromMap) (\_ -> service request)
                                                                        liftIO $ putMVar firstRequestMVar $ Right result
                                                                        return result
                                            updateMap mvar          = (M.insert request mvar refMap, liftIO $ getResult mvar)
                                            addToMap                = (M.insert request firstRequestMVar refMap, invokeService)
                                        in  maybe addToMap updateMap $ M.lookup request refMap)
                                    resultAction
  return (mapRef, protectedService)