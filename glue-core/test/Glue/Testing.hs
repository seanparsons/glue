{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.Testing where

import Data.Hashable
import Test.QuickCheck
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M