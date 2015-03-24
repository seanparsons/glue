{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Glue.FailoverSpec where

import Data.Typeable
import Glue.Failover
import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base hiding (throw, throwIO)
import Control.Exception.Lifted

data FailoverTestException = FailoverTestException deriving (Eq, Show, Typeable)
instance Exception FailoverTestException

spec :: Spec
spec = do
  describe "failover" $ do
    it "Failover handles potentially multiple errors" $ do
      property $ \(request, failureMax) ->
        let positiveFailureMax  = (abs failureMax) `mod` 10
            service req         = if req >= 10 then return (req + 100) else throwIO FailoverTestException 
            options             = defaultFailoverOptions { transformFailoverRequest = (+1), maxFailovers = positiveFailureMax }
            failOverService     = failover options service
            successCase         = (failOverService request) `shouldReturn` ((if request <= 10 then 10 else request) + 100)
            failureCase         = (failOverService request) `shouldThrow` (== FailoverTestException)
        in if request + positiveFailureMax >= 10 then successCase else failureCase 