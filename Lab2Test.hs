module TestLab2 where

import Test.HUnit --cabal install hunit
import qualified Lab2 as L2

main :: IO Counts
main = runTestTT $ TestList [testParseSimple]

testParseSimple :: Test
testParseSimple = 
  TestCase $ assertEqual "message" 1 1