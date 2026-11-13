{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.FailoverSpec (spec) where

import qualified Control.Exception  as E
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty (..))
import           Glue.Failover
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception (Exception, fromException, throwIO, try)

data FailoverTestException = FailoverTestException
  deriving (Eq, Show)

instance Exception FailoverTestException

-- | A backend failure, carrying the backend index and whether it should advance.
data BackendException = BackendException Int Bool
  deriving (Eq, Show)

instance Exception BackendException

advances :: E.SomeException -> Bool
advances e = maybe True (\(BackendException _ advance) -> advance) (fromException e)

-- | Backends that record their calls, each succeeding or failing (advancing or not) as scripted.
backends :: IORef [Int] -> NonEmpty (Maybe Bool) -> NonEmpty (Int -> IO Int)
backends calls = fmap backend . indexed
  where
    indexed (x :| xs) = (0, x) :| zip [1 ..] xs
    backend (index, script) req = do
      atomicModifyIORef' calls (\c -> (c ++ [index], ()))
      case script of
        Nothing      -> pure (req + index)
        Just advance -> throwIO (BackendException index advance)

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

  describe "failoverAcross" $ do
    it "Tries backends in order and returns the first success with its index" $ do
      calls <- newIORef []
      let services = backends calls (Just True :| [Just True, Nothing, Nothing])
      failoverAcrossIndexed defaultFailoverAcrossOptions services 100 `shouldReturn` (2, 102)
      readIORef calls `shouldReturn` [0, 1, 2]

    it "Stops at a failure that does not advance" $ do
      calls <- newIORef []
      let services = backends calls (Just True :| [Just False, Nothing])
          options  = defaultFailoverAcrossOptions { advanceOn = advances }
      failoverAcross options services 1 `shouldThrow` (== BackendException 1 False)
      readIORef calls `shouldReturn` [0, 1]

    it "Reports the last or first failure when every backend fails" $ do
      calls <- newIORef []
      let services = backends calls (Just True :| [Just True, Just True])
      failoverAcross defaultFailoverAcrossOptions services 1 `shouldThrow` (== BackendException 2 True)
      failoverAcross defaultFailoverAcrossOptions { reportedFailure = ReportFirstFailure } services 1
        `shouldThrow` (== BackendException 0 True)

    it "Propagates an asynchronous exception without advancing" $ do
      calls <- newIORef ([] :: [Int])
      let interrupted _ = atomicModifyIORef' calls (\c -> (c ++ [0], ())) >> E.throwIO E.UserInterrupt :: IO Int
          services = interrupted :| [\req -> atomicModifyIORef' calls (\c -> (c ++ [1], req))]
      failoverAcross defaultFailoverAcrossOptions services 1 `shouldThrow` (== E.UserInterrupt)
      readIORef calls `shouldReturn` [0]

    it "Behaves like the Either shape for the same outcomes" $
      property $ \(script :: [Maybe Bool], first :: Bool) -> do
        calls <- newIORef []
        let scripted = Just True :| script
            options :: FailoverAcrossOptions e
            options = defaultFailoverAcrossOptions { reportedFailure = if first then ReportFirstFailure else ReportLastFailure }
            valued = fmap (\(index, s) req -> pure (maybe (Right (req + index)) (\advance -> Left (index, advance)) s :: Either (Int, Bool) Int))
                          ((0, Just True) :| zip [1 ..] script)
        viaException <- try (failoverAcrossIndexed options { advanceOn = advances } (backends calls scripted) 10)
        viaValue     <- failoverAcrossIndexedEither options { advanceOn = snd } valued 10
        either (\(BackendException i a) -> Left (i, a)) Right viaException `shouldBe` viaValue
