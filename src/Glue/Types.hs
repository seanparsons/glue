{-# LANGUAGE FlexibleContexts #-}

module Glue.Types(
    BasicService
  , MultiGetService
  , MultiGetRequest
  , MultiGetResponse
  , ResultVar
  , multiGetToBasic
  , basicToMultiGet
  , getResult
) where

import Control.Applicative
import Data.Hashable
import Control.Concurrent
import qualified Control.Concurrent.MVar.Lifted as MV
import Control.Exception.Base hiding(throw, throwIO)
import Control.Exception.Lifted hiding(throw)
import Control.Monad.Trans.Control
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type BasicService m a b = a -> m b
type MultiGetRequest a = S.HashSet a
type MultiGetResponse a b = M.HashMap a b
type MultiGetService m a b = BasicService m (MultiGetRequest a) (MultiGetResponse a b)
type ResultVar a = MVar (Either SomeException a)

multiGetToBasic :: (Hashable a, Eq a, Monad m) => MultiGetService m a b -> BasicService m a b
multiGetToBasic service = (\r -> do
  mapResult <- service (S.singleton r)
  let possibleResult = M.lookup r mapResult
  maybe (fail "Invalid result.") return possibleResult)

basicToMultiGet :: (Hashable a, Eq a, Applicative m) => BasicService m a b -> MultiGetService m a b
basicToMultiGet service = 
  let callService resultMap request    = liftA2 (flip $ M.insert request) resultMap (service request)
  in  S.foldl' callService (pure M.empty)

-- Utils?
getResult :: (MonadBaseControl IO m) => ResultVar a -> m a
getResult var = do
  result <- MV.readMVar var
  either throwIO return result