module ServiceFabric.Types where

import Control.Concurrent
import Control.Exception.Base

type BasicService m a b = a -> m b
type ResultVar a = MVar (Either SomeException a)
