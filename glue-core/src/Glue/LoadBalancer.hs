{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module providing round-robin load balancing across multiple services.
module Glue.LoadBalancer
  ( NoServicesAvailableException (..)
  , roundRobinLoadBalancer
  , roundRobinLoadBalancerNE
  ) where

import           Data.Foldable      (toList)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Glue.Types
import           UnliftIO           (Exception, MonadIO)
import           UnliftIO.Exception (throwIO)
import           UnliftIO.IORef     (atomicModifyIORef', newIORef)

-- | Exception thrown when attempting to call a load balancer configured with an empty set of services.
data NoServicesAvailableException = NoServicesAvailableException
  deriving (Eq, Show)

instance Exception NoServicesAvailableException

-- | Creates a service that distributes requests across a 'NonEmpty' list of backend services in round-robin order.
roundRobinLoadBalancerNE
  :: (MonadIO m, MonadIO n)
  => NonEmpty (BasicService m a b)
  -- ^ Non-empty collection of backend services to balance across.
  -> n (BasicService m a b)
roundRobinLoadBalancerNE services = do
  let svcList = NE.toList services
      count   = length svcList
  indexRef <- newIORef (0 :: Int)
  let balanced request = do
        idx <- atomicModifyIORef' indexRef (\i -> ((i + 1) `mod` count, i))
        let target = svcList !! idx
        target request
  pure balanced

-- | Creates a service that distributes requests across a foldable collection of backend services in round-robin order.
-- If the collection is empty, invoking the returned service throws t'NoServicesAvailableException'.
roundRobinLoadBalancer
  :: (MonadIO m, MonadIO n, Foldable t)
  => t (BasicService m a b)
  -- ^ Collection of backend services to balance across.
  -> n (BasicService m a b)
roundRobinLoadBalancer services =
  case toList services of
    []     -> pure $ const $ throwIO NoServicesAvailableException
    (x:xs) -> roundRobinLoadBalancerNE (x :| xs)
