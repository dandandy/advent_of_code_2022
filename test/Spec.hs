module Main (main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Day1Spec

main :: IO ()
main =
  defaultMain
    [testGroup "Day1" libTests] 
