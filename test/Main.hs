module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

customConfig :: Config
customConfig = defaultConfig 
  { configColorMode     = ColorAlways
  , configPrintCpuTime  = True
  }


main :: IO ()
main = hspecWith customConfig Spec.spec