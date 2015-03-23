{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.DogpileProtection(
  dogpileProtect
) where

import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.IORef.Lifted
import Glue.Types

-- Loses the values held within m.
-- Should make this return just BasicService, hiding the HashMap.
-- Need sharding support.
dogpileProtect :: (MonadBaseControl IO m, Eq a, Hashable a) => BasicService m a b -> m (IORef (M.HashMap a (ResultVar b)), BasicService m a b)
dogpileProtect service = do
  mapRef <- newIORef M.empty
  let protectedService request = do
                                    firstRequestMVar <- newEmptyMVar 
                                    resultAction <- atomicModifyIORef' mapRef (\refMap -> 
                                        let removeFromMap           = atomicModifyIORef' mapRef (\m -> (M.delete request m, ()))
                                            invokeService           = do 
                                                                        result <- bracketOnError (return ()) (\_ -> removeFromMap) (\_ -> service request)
                                                                        putMVar firstRequestMVar $ Right result
                                                                        return result
                                            updateMap mvar          = (M.insert request mvar refMap, getResult mvar)
                                            addToMap                = (M.insert request firstRequestMVar refMap, invokeService)
                                        in  maybe addToMap updateMap $ M.lookup request refMap)
                                    resultAction
  return (mapRef, protectedService)