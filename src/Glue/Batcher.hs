{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for creating a service that batches calls across requests.
module Glue.Batcher(
    BatchingOptions
  , batchingService
  , defaultBatchingOptions
  , batchWindowMs
) where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Foldable
import Data.Hashable
import Data.IORef.Lifted
import Glue.Types

-- | Options for configuring the batching.
data BatchingOptions = BatchingOptions {
    batchWindowMs :: Int           -- ^ Window in milliseconds over which to batch.
} deriving (Eq, Show)

-- | Default instance of 'BatchingOptions' with a batching window of 10ms.
defaultBatchingOptions :: BatchingOptions
defaultBatchingOptions = BatchingOptions {
   batchWindowMs = 10
}

-- | Cumulative request batch of all the outstanding calls.
data RequestBatch a b = RequestBatch [PendingRequest a b] (MultiGetRequest a)

-- | Individual request to satisfy, for either a single key or a multi request.
data PendingRequest a b =
  SingleRequest a (ResultVar (Maybe b)) |
  MultiRequest (MultiGetRequest a) (ResultVar (MultiGetResponse a b))

applyToPending :: (Eq a, Hashable a, MonadBaseControl IO m) => Either SomeException (M.HashMap a b) -> PendingRequest a b -> m ()
applyToPending (Left e) (SingleRequest _ var)         = putMVar var $ Left e
applyToPending (Right results) (SingleRequest a var)  = putMVar var $ Right $ M.lookup a results
applyToPending (Left e) (MultiRequest _ var)          = putMVar var $ Left e
applyToPending (Right results) (MultiRequest as var)  = putMVar var $ Right $ M.filterWithKey (\k -> \_ -> S.member k as) results

emptyBatch :: (Eq a, Hashable a) => RequestBatch a b
emptyBatch = RequestBatch [] S.empty

processCalls :: (Eq a, Hashable a, MonadBaseControl IO m) => MultiGetService m a b -> RequestBatch a b -> m ()
processCalls service (RequestBatch pendings requests) = do
                                                          result <- makeCall service requests
                                                          traverse_ (applyToPending result) pendings

startBatch :: (Eq a, Hashable a, MonadBaseControl IO m) => BatchingOptions -> MultiGetService m a b -> IORef (RequestBatch a b) -> m ()
startBatch options service ref = fmap (\_ -> ()) $ fork $ do
                                                            threadDelay (1000 * batchWindowMs options)
                                                            pendings <- atomicModifyIORef' ref (\p -> (emptyBatch, p))
                                                            processCalls service pendings

addPending :: (Eq a, Hashable a, MonadBaseControl IO m) => BatchingOptions -> MultiGetService m a b -> IORef (RequestBatch a b) -> PendingRequest a b -> m ()
addPending options service ref p@(SingleRequest request _) = join $ atomicModifyIORef' ref (\(RequestBatch ps rs) -> (RequestBatch (p : ps) (S.insert request rs), if null ps then startBatch options service ref else return ()))
addPending options service ref p@(MultiRequest requests _) = join $ atomicModifyIORef' ref (\(RequestBatch ps rs) -> (RequestBatch (p : ps) (S.union rs requests), if null ps then startBatch options service ref else return ()))

singleService :: (Eq a, Hashable a, MonadBaseControl IO m) => BatchingOptions -> MultiGetService m a b -> IORef (RequestBatch a b) -> BasicService m a (Maybe b)
singleService options service ref request = do
                                              mvar <- newEmptyMVar
                                              let pending = SingleRequest request mvar
                                              addPending options service ref pending
                                              getResult mvar
multiService :: (Eq a, Hashable a, MonadBaseControl IO m) => BatchingOptions -> MultiGetService m a b -> IORef (RequestBatch a b) -> MultiGetService m a b
multiService options service ref requests = do
                                              mvar <- newEmptyMVar
                                              let pending = MultiRequest requests mvar
                                              addPending options service ref pending
                                              getResult mvar

-- | Function for constructing a batching service.
batchingService :: (Eq a, Hashable a, MonadBaseControl IO m, Applicative m, MonadBaseControl IO n)
                => BatchingOptions            -- ^ Options to configure the batched service.
                -> MultiGetService m a b      -- ^ Service around which to batch requests.
                -> n (BasicService m a (Maybe b), MultiGetService m a b)
batchingService options service = do
  ref <- newIORef emptyBatch
  return (singleService options service ref, multiService options service ref)
