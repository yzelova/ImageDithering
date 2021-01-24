module Tests.RunTests where

import Test.HUnit (Counts, Test (TestList), runTestTT)
import Tests.Algorithms (testAtkinson)

testsUtils :: Test
testsUtils = TestList [testAtkinson]

main :: IO Counts
main = runTestTT testsUtils