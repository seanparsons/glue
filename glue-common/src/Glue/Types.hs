{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing the root types and some support functionality.
module Glue.Types(
    BasicService
  , MultiGetService
  , MultiGetRequest
  , MultiGetResponse
  , ResultVar
  , MToIO
  , FailOrSuccess
  , multiGetToBasic
  , basicToMultiGet
  , getResult
  , makeCall
) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.MVar.Lifted as MV
import           Control.Exception.Base         hiding (catch, throw, throwIO)
import           Control.Exception.Lifted       hiding (throw)
import           Control.Monad.Trans.Control
import           Data.Hashable
import qualified Data.HashMap.Strict            as M
import qualified Data.HashSet                   as S

-- | Type alias for the most basic form of a service supported.
type BasicService m a b = a -> m b
-- | Type alias for the request portion of a `MultiGetService`.
type MultiGetRequest a = S.HashSet a
-- | Type alias for the response portion of a `MultiGetService`.
type MultiGetResponse a b = M.HashMap a b
-- | Type alias for a service that looks up multiple values and returns multiple results.
type MultiGetService m a b = BasicService m (MultiGetRequest a) (MultiGetResponse a b)
-- | Type alias for the common container of a result used in asynchronous calls.
type ResultVar a = MVar (Either SomeException a)
-- | Run the m into an IO instance for a response.
type MToIO m = forall a. m a -> IO a
-- | Type alias for either a failure or a successful response.
type FailOrSuccess a b = Either SomeException (MultiGetResponse a b)

-- | Convert a 'MultiGetService' into a 'BasicService' that looks up a single key, returning a `Maybe` which determines if the value was present.
multiGetToBasic :: (Hashable a, Eq a, Monad m) => MultiGetService m a b -> BasicService m a (Maybe b)
multiGetToBasic service = (\r -> do
  mapResult <- service (S.singleton r)
  return $ M.lookup r mapResult)

-- | Convert a 'BasicService' into a 'MultiGetService'
basicToMultiGet :: (Hashable a, Eq a, Applicative m) => BasicService m a b -> MultiGetService m a b
basicToMultiGet service =
  let callService resultMap request = liftA2 (flip $ M.insert request) resultMap (service request)
  in  S.foldl' callService (pure M.empty)

-- | Obtain a result from a 'ResultVar' in the 'Monad' of your choice.
getResult :: (MonadBaseControl IO m) => ResultVar a -> m a
getResult var = do
  result <- MV.readMVar var
  either throwIO return result

-- | Makes a multi-get call and handles the error bundling it up inside an 'Either'.
makeCall :: (Eq a, Hashable a, MonadBaseControl IO m) => MultiGetService m a b -> S.HashSet a -> m (FailOrSuccess a b)
makeCall service requests = catch (fmap Right $ service requests) (\(e :: SomeException) -> return $ Left e)
