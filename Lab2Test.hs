module TestLab2 where

import Test.HUnit --cabal install hunit
import qualified Lab2 as L2
import Data.Text

main :: IO Counts
main = runTestTT $ TestList [testGetLinesKeyValue] --не забыть добавлять функции

testGetLinesKeyValue :: Test
testGetLinesKeyValue =
	TestCase $ assertEqual "parsing single line to custom JSON Object" 
			   			   (L2.String "\"i love you\"","\"hello\"")
			   			   (L2.getLinesKeyValue $ pack $ "\"hello\": \"i love you\"") 
	--(let x = x :: ReadS (L2.JSON) in let x = (L2.Object [], "hello") in x)