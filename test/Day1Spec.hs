module Day1Spec where

import Test.Framework (Test)
import Day1
import Test.HUnit.Base hiding (Test)
import Test.Framework.Providers.HUnit

libTests :: [Test]
libTests = [
        testCase "given single elf" (day1part1 "0\n" @?= Right "0"),
        testCase "given single elf" (day1part1 "1\n" @?= Right "1")
        -- testCase "given two elfs" ((day1part1 "0\n\n1") @?= "1"),
    ]
