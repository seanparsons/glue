{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Glue.RetrySpec where

import Data.IORef
import Data.Typeable
import Glue.Retry
import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base hiding (throw)
import Control.Monad.CatchIO
import Control.Monad.IO.Class

newtype SmallInt = SmallInt Int deriving (Eq, Show)

instance Arbitrary SmallInt where
  arbitrary = sized $ \s -> do
                                 n <- choose (0, s `min` 10)
                                 return $ SmallInt n

data RetryTestException = RetryTestException deriving (Eq, Show, Typeable)
instance Exception RetryTestException

spec :: Spec
spec = do
  describe "retryingService" $ do
    it "Attempts a service call multiple times" $ do
      property $ \(request, (SmallInt failures), (SmallInt retries)) -> 
        do
          ref <- liftIO $ newIORef 0
          let service req   = do
                                counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                                if counter > failures then return req else throw RetryTestException
          let options       = defaultRetryOptions { maximumRetries = retries }
          let retryService  = retryingService options service
          let successCase   = (retryService request) `shouldReturn` (request :: Int)
          let failureCase   = (retryService request) `shouldThrow` (== RetryTestException)
          if retries >= failures then successCase else failureCase