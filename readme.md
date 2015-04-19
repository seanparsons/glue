Building Better Services
========================

[![Build Status](https://secure.travis-ci.org/seanparsons/glue.svg)](http://travis-ci.org/seanparsons/glue)

This package provides methods to deal with the common needs of services talking to other services, like timeouts and retries, treating these as cross-cutting concerns that aren't tied to a specific transport like HTTP.

Examples
========

Batching
--------

In an active service lots of calls would be being made simultaneously to look different instances of the same thing. For instance in a social network there may be another service that returns user records. Rather than 200 requests all incurring individual HTTP requests for a single user, it would likely be more efficient to capture multiple calls and dispatch them as one multi-get call.

The batchingService function creates both single and multi-get calls (hence the "fmap snd" below), which accumulate requests over a user defined window in time and dispatch them once that window has passed from the first request coming in that triggers it.

```haskell
data User = User Int String

userService :: S.HashSet Int -> IO (M.HashMap Int User)
userService request = do
  threadDelay 500
  return $ M.fromList $ fmap (\r -> (r, User r ("User " ++ (show r)))) $ S.toList request

batchedUserService :: IO (S.HashSet Int -> IO (M.HashMap Int User))
batchedUserService = fmap snd $ batchingService defaultBatchingOptions userService
```


Retries
-------

Often we want to retry service calls if they've failed because it might've been a transient error that subsequently succeeds.

```haskell
failingService :: IORef Int -> Int -> IO Int
failingService ref request = do
  counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
  if counter `mod` 3 == 0 then fail "Bang!" else return (request * 2)

notSoFailingService :: IO (Int -> IO Int)
notSoFailingService = do
  ref <- liftIO $ newIORef 0
  return $ retryingService defaultRetryOptions $ failingService ref
```

Caching
-------

```haskell
serviceThatNeedsCaching :: Int -> IO Int
serviceThatNeedsCaching request = do
  putStrLn "Doing Something Expensive!"
  return (request * 2)

cachedService :: IO (Int -> IO Int)
cachedService = do
  cache <- newAtomicLRU $ Just 100
  return $ cacheWithLRU cache serviceThatNeedsCaching
```
