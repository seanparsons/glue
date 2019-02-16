{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting the dogpile protection of a service, see <http://en.wikipedia.org/wiki/Cache_stampede http://en.wikipedia.org/wiki/Cache_stampede>.
module Glue.DogpileProtection(
  dogpileProtect
) where

import           Control.Concurrent
import           Control.Exception
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.IORef
import           Glue.Types

data DogpileResult b = CachedValue (Either SomeException b)
                     | RequestInProgress (IO b)

type ResultMap a b = M.HashMap a (DogpileResult b)

type ResultRef a b = IORef (ResultMap a b)

getDogpileResult :: DogpileResult b -> IO b
getDogpileResult (RequestInProgress requestWait) = requestWait
getDogpileResult (CachedValue result)            = either throw return result

waitForMVar :: MVar (Either SomeException b) -> IO b
waitForMVar mvar = do
  result <- readMVar mvar
  either throw return result

getProtectedResult :: (Eq a, Hashable a)
                   => a -> BasicService IO a b -> MVar (Either SomeException b) -> ResultRef a b -> ResultMap a b -> (ResultMap a b, IO b)
getProtectedResult request service mvar mapRef resultMap =
  let mvarWait = waitForMVar mvar
      invokeService = do
        result <- try $ service request
        putMVar mvar result
        atomicModifyIORef' mapRef (\mapToUpdate -> (M.insert request (CachedValue result) mapToUpdate, ()))
        forkIO $ do
          threadDelay (5 * 1000 * 1000)
          atomicModifyIORef' mapRef (\mapToUpdate -> (M.delete request mapToUpdate, ()))
      inCache = M.lookup request resultMap
  in  maybe (M.insert request (RequestInProgress mvarWait) resultMap, invokeService >> mvarWait) (\r -> (resultMap, getDogpileResult r)) inCache

-- | Dogpile protection of a service, to prevent multiple calls for the same value being submitted.
-- | Loses the values held within m.
dogpileProtect :: (Eq a, Hashable a)
               => BasicService IO a b   -- ^ The service to protect.
               -> IO (BasicService IO a b)
dogpileProtect service = do
  mapRef <- newIORef M.empty
  let protectedService request = do
        mvar <- newEmptyMVar
        resultAction <- atomicModifyIORef' mapRef $ getProtectedResult request service mvar mapRef
        resultAction
  return protectedService
