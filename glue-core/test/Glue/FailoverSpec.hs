{-# LANGUAGE FlexibleContexts #-}

module Glue.FailoverSpec (spec) where

import           Glue.Failover
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception (Exception, throwIO)

data FailoverTestException = FailoverTestException
  deriving (Eq, Show)

instance Exception FailoverTestException

spec :: Spec
spec = do
  describe "failover" $ do
    it "Failover handles potentially multiple errors" $
      property $ \(request, failureMax) ->
        let positiveFailureMax = abs failureMax `mod` 10
            service req        = if req >= 10 then pure (req + 100) else throwIO FailoverTestException
            options            = defaultFailoverOptions { transformFailoverRequest = (+ 1), maxFailovers = positiveFailureMax }
            failOverService    = failover options service
            successCase        = failOverService request `shouldReturn` ((if request <= 10 then 10 else request) + 100)
            failureCase        = failOverService request `shouldThrow` (== FailoverTestException)
        in  if request + positiveFailureMax >= 10 then successCase else failureCase
