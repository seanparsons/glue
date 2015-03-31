{-# LANGUAGE OverloadedStrings #-}

import Glue
import Control.Concurrent.Async
import Control.Concurrent.Lifted
import Data.IORef.Lifted
import qualified Data.List as L
import Data.Text
import Data.Traversable
import System.Metrics
import qualified System.Metrics.Distribution as D
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Text.Printf

printStats :: Store -> Text -> IO ()
printStats store name = do
  samples <- sampleAll store
  let possibleValue = M.lookup name samples
  case possibleValue of
                        (Just (Distribution stats)) -> printf "%s - sum : %f - count : %d" (unpack name) (D.sum stats) (D.count stats) >> putStrLn ""
                        otherwise                   -> return ()

runTest :: Store -> Text -> Bool -> IO ()
runTest store name shouldBatch = do
  counter <- newIORef 0
  let listOfNums = [1..50]
  let requests = fmap (S.fromList . L.take 10) $ L.tails listOfNums :: [S.HashSet Int]
  let service request = do
                          atomicModifyIORef' counter (\c -> (c + (S.size request), ()))
                          threadDelay (500 * S.size request)
                          return $ M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList request
  statsWrappedService <- recordDistribution store name service
  possiblyBatchedService <- if shouldBatch then return statsWrappedService else fmap (\(_, b) -> b) $ batchingService defaultBatchingOptions statsWrappedService
  (mapConcurrently possiblyBatchedService requests)
  printStats store name

-- Tests the batchingService by comparing the stats with and without it.
main :: IO ()
main = do
  store <- newStore
  runTest store "Unbatched Service" False
  runTest store "Batched Service" True
