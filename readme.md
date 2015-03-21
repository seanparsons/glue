Building Better Services
========================

[![Build Status](https://secure.travis-ci.org/seanparsons/glue.svg)](http://travis-ci.org/seanparsons/glue)

This package provides methods to deal with the common needs of services talking to other services, like timeouts and retries, treating these as cross-cutting concerns that aren't tied to a specific transport like HTTP.

Examples
========

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
