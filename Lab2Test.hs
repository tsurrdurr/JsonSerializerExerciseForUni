module TestLab2 where

import Test.HUnit --cabal install hunit
import qualified Lab2 as L2
import Data.Text

main :: IO Counts
main = runTestTT $ TestList [testGetLinesKeyValue, testParseInt, testParseNull, testParseBool, splitFirstTest, testGetLinesKeyValueWithList] --не забыть добавлять функции

testGetLinesKeyValue :: Test
testGetLinesKeyValue =
	TestCase $ assertEqual "parsing single line to custom JSON Object" 
			   			   (L2.String "\"i love you\"","\"hello\"")
			   			   (fst $ L2.getSimpleKV $ pack $ "\"hello\": \"i love you\"") 

testParseInt :: Test
testParseInt =
    TestCase $ assertEqual "parsing int to custom JSON Object" 
                           (L2.Int 12,"\"hello\"")
                           (fst $ L2.getSimpleKV $ pack $ "\"hello\": 12") 

testParseNull :: Test
testParseNull =
    TestCase $ assertEqual "parsing null to custom JSON Object" 
                           (L2.Null,"\"hello\"")
                           (fst $ L2.getSimpleKV $ pack $ "\"hello\": null")

testParseBool :: Test
testParseBool =
    TestCase $ assertEqual "parsing bool to custom JSON Object" 
                           (L2.Bool True,"\"hello\"")
                           (fst $ L2.getSimpleKV $ pack $ "\"hello\": true")

splitFirstTest :: Test
splitFirstTest = 
    TestCase $ assertEqual "split"
                           ([pack $ "hey", pack $ " vsauce, michael here"]) 
                           (L2.splitFirst (pack $ "hey, vsauce, michael here") ",")

testGetLinesKeyValueWithList :: Test
testGetLinesKeyValueWithList =
	TestCase $ assertEqual "parsing json with list to custom JSON Object" 
			   			   (L2.List ["\"i love you\"", "\"is it me you're looking for\""],"\"hello\"")
			   			   (Prelude.head $ L2.parseLevel [] (pack $ "\"hello\": [\"i love you\",\"is it me you're looking for\"]"))
	--(let x = x :: ReadS (L2.JSON) in let x = (L2.Object [], "hello") in x)