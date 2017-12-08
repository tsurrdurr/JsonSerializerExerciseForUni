module TestLab2 where

import Test.HUnit --cabal install hunit
import qualified Lab2 as L2
import Data.Text

main :: IO Counts
main = runTestTT $ TestList [testGetLinesKeyValue, testParseInt, testGetLinesKeyValueWithList] --не забыть добавлять функции

testGetLinesKeyValue :: Test
testGetLinesKeyValue =
	TestCase $ assertEqual "parsing single line to custom JSON Object" 
			   			   (L2.String "\"i love you\"","\"hello\"")
			   			   (L2.getLinesKeyValue $ pack $ "\"hello\": \"i love you\"") 

testParseInt :: Test
testParseInt =
    TestCase $ assertEqual "parsing single int to custom JSON Object" 
                           (L2.Int 12,"\"hello\"")
                           (L2.getLinesKeyValue $ pack $ "\"hello\": 12") 

testGetLinesKeyValueWithList :: Test
testGetLinesKeyValueWithList =
	TestCase $ assertEqual "parsing json with list to custom JSON Object" 
			   			   (L2.List ["\"i love you\"", "\"is it me you're looking for\""],"\"hello\"")
			   			   (L2.getLinesKeyValue $ pack $ "\"hello\": [\"i love you\",\"is it me you're looking for\"]") 
	--(let x = x :: ReadS (L2.JSON) in let x = (L2.Object [], "hello") in x)