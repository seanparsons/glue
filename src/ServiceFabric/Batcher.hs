{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServiceFabric.Batcher(
) where

import ServiceFabric.Types
import Control.Monad.IO.Class
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Hashable

data BatchingOptions = BatchingOptions {
  batchWindowMs   :: Int
} deriving (Eq, Show)

batchingService :: (Eq a, Hashable a, MonadCatchIO m) => MultiGetService m a b -> (BasicService m a (Maybe b), MultiGetService m a b)
batchingService service = 
  let makeCall requests                                     = catch (fmap right $ service requests) (\(e :: SomeException) -> return $ left e)
      applyToPending (Left e) (SingleRequest a var)         = putMVar var $ Left e
      applyToPending (Right results) (SingleRequest a var)  = putMVar var $ Right $ M.lookup a results
      applyToPending (Left e) (MultiRequest as var)         = putMVar var $ Left e
      applyToPending (Right results) (MultiRequest as var)  = putMVar var $ Right $ M.filter (flip $ S.member as) results
      processCalls (RequestBatch pendings requests)         = do
                                                                result <- makeCall requests
                                                                traverse_ (applyToPending result) pendings





  do
  ref <- liftIO $ newIORef []

data RequestBatch = RequestBatch [PendingRequest] (S.HashSet a)

data PendingRequest a b = 
  SingleRequest a (ResultVar (Maybe b)) |
  MultiRequest (S.HashSet a) (ResultVar (M.HashMap a b))



{-
If there's no waiting requests, start the background thread.


-}