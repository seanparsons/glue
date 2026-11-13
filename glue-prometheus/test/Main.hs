module Main where

import qualified Spec
import           Test.Hspec.Runner

customConfig :: Config
customConfig = defaultConfig
  { configColorMode       = ColorAlways
  , configPrintCpuTime    = True
  }


main :: IO ()
main = hspecWith customConfig Spec.spec
