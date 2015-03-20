{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServiceFabric.Batcher(
    BatchingOptions
  , batchingService
) where

import Control.Applicative
import Control.Exception.Base(SomeException)
import Control.Concurrent.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Foldable
import Data.Hashable
import Data.IORef(IORef)
import Data.IORef.Lifted
import ServiceFabric.Types

data BatchingOptions = BatchingOptions {
  batchWindowMs :: Int
} deriving (Eq, Show)

batchingService :: (Eq a, Hashable a, MonadBaseControl IO m, Applicative m) => BatchingOptions -> MultiGetService m a b -> (BasicService m a (Maybe b), MultiGetService m a b)
batchingService options service = 
  let emptyBatch :: RequestBatch a b
      emptyBatch                                            = RequestBatch [] S.empty
      makeCall :: S.HashSet a -> m (Either SomeException (M.HashMap a b))
      makeCall requests                                     = catch (fmap Right $ service requests) (\(e :: SomeException) -> return $ Left e)
      applyToPending :: Either SomeException (M.HashMap a b) -> PendingRequest a b -> m ()
      applyToPending (Left e) (SingleRequest a var)         = putMVar var $ Left e
      applyToPending (Right results) (SingleRequest a var)  = putMVar var $ Right $ M.lookup a results
      applyToPending (Left e) (MultiRequest as var)         = putMVar var $ Left e
      applyToPending (Right results) (MultiRequest as var)  = putMVar var $ Right $ M.filter ((flip S.member) as) results
      processCalls :: RequestBatch a b -> m ()
      processCalls (RequestBatch pendings requests)         = do
                                                                result <- makeCall requests
                                                                traverse_ (applyToPending result) pendings
      startBatch :: IORef (RequestBatch a b) -> m ()
      startBatch ref                                        = fmap (\_ -> ()) $ fork $ do
                                                                                          threadDelay (1000 * batchWindowMs options)
                                                                                          pendings <- atomicModifyIORef' ref (\p -> (emptyBatch, p))
                                                                                          processCalls pendings
      startBatchIfEmpty :: IORef (RequestBatch a b) -> m ()
      startBatchIfEmpty ref                                 = readIORef ref >>= (\(RequestBatch p _) -> if null p then return () else fmap (\_ -> ()) $ startBatch ref)
      addPending :: IORef (RequestBatch a b) -> PendingRequest a b -> m ()
      addPending ref p@(SingleRequest request _)            = atomicModifyIORef' ref (\(RequestBatch ps rs) -> (RequestBatch (p : ps) (S.insert rs request), ()))
      addPending ref p@(MultiRequest requests _)            = atomicModifyIORef' ref (\(RequestBatch ps rs) -> (RequestBatch (p : ps) (S.union rs requests), ()))
      singleService :: IORef (RequestBatch a b) -> BasicService m a (Maybe b)
      singleService ref request                             = do
                                                                startBatchIfEmpty ref
                                                                mvar <- newEmptyMVar
                                                                let pending = SingleRequest request mvar
                                                                addPending ref pending
                                                                liftIO $ getResult mvar
      multiService :: IORef (RequestBatch a b) -> MultiGetService m a b
      multiService ref requests                             = do
                                                                startBatchIfEmpty ref
                                                                mvar <- newEmptyMVar
                                                                let pending = MultiRequest requests mvar
                                                                addPending ref pending
                                                                liftIO $ getResult mvar
  in do
        ref <- newIORef emptyBatch
        (singleService ref, multiService ref)

data RequestBatch a b = RequestBatch [PendingRequest a b] (S.HashSet a)

data PendingRequest a b = 
  SingleRequest a (ResultVar (Maybe b)) |
  MultiRequest (S.HashSet a) (ResultVar (M.HashMap a b))



{-
If there's no waiting requests, start the background thread.


-}