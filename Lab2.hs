module Lab2 where

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import System.Random
import Control.Exception
import Control.Monad
import Control.Monad.State
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Data.String
--import qualified StringUtils as SU --cabal install stringutils
import qualified Data.Text as T
import Data.Text
import qualified Text.Read as RD
import Network (withSocketsDo)

-- почтовый адрес
email = "notlaika@protonmail.com"
data JSON = Object [(String, JSON)] | String String | Int Int | Bool Bool | Null |  List [String] | JSONList [JSON] deriving(Eq)

-- добавим сответствующие классы типов для JSON
instance Show JSON where
  show = stringify
instance Read JSON where
  readsPrec _ x = parse x

jsonSample = [(Object [], "sample json")]
jsonError = [(Object [], "parse error")]

parse :: ReadS (JSON)
parse "{}" = [(Object [], "")]


parse json | (validateLevel json) = parseLevel [] (trimFig $ T.pack $ json)
           | otherwise = jsonError

isListOfValues :: [Text] -> Bool 
isListOfValues keyRestOfDoc = let openingSymbol = T.head $ T.strip $ Prelude.last $ keyRestOfDoc in
                              if (openingSymbol=='[') then True
                              else False

hasDepth :: Text -> Bool
hasDepth keyRestOfDoc = validateHead $ T.strip $ Prelude.last $ (splitFirst keyRestOfDoc ":")

buildDepth :: Text -> ((JSON, String), Text)
buildDepth json = let parts = splitFirst json ":" in
                  let key =  T.strip $ trimFig $ T.strip $ Prelude.head $ parts in
                  let value = T.strip $ trimFig $ T.strip (T.takeWhile (/='}') (Prelude.last $ parts)) in 
                  ((Object (twist  (parseLevel [] value)), T.unpack $ key), T.drop 1 (T.dropWhile (/='}') json))

getListOfValues :: [Text] -> ((JSON, String), Text)
getListOfValues [key, rest] =  let contents = Prelude.last (splitFirst (Prelude.head $ (splitFirst rest "]")) "[") in
                               let restOfDoc = Prelude.last $ (splitFirst rest "]") in
                               ((getListValues $ T.unpack $ T.strip $ contents, T.unpack $ T.strip $ key), T.strip $ restOfDoc)


getListValues :: String -> JSON
getListValues str = let textarray = splitByCommas $ trimSquare $ T.pack $ str in
                    List [ T.unpack $ T.strip $ text | text <- textarray ]

whatsNext :: [(JSON, String)] -> ((JSON, String), Text) -> [(JSON, String)]
whatsNext acc (parseResult, json) | (T.any (== ',') json) = parseLevel (addToAcc acc parseResult) (T.strip (T.drop 1 (T.strip $ json)))
                                  | otherwise = addToAcc acc parseResult

addToAcc :: [(JSON, String)] -> (JSON, String) -> [(JSON, String)]
addToAcc acc new = new : acc 


parseLevel :: [(JSON, String)] -> Text -> [(JSON, String)]
parseLevel acc json = let kv = splitFirst json ":" in
                        if isListOfValues (kv) then whatsNext acc (getListOfValues kv) 
                        else if (hasDepth json) then whatsNext acc (buildDepth json)
                        else whatsNext acc (getSimpleKV $ json)

twist :: [(JSON, String)] -> [(String, JSON)]
twist xs = Prelude.map (\(a, b) -> (b, a)) xs



validateLevel :: String -> Bool
validateLevel json = validateTrimmed $ trimWS $ json 

isList :: String -> Bool
isList value = (Prelude.head value == '[') && (Prelude.last value == ']')

tryParseInt :: String -> (Bool, Int)
tryParseInt s =
    case reads s of
        [(i, "")] -> (True, i)
        _ -> (False, 0)

parseBool :: String -> Bool
parseBool b = 
  case b of
    "true" -> True
    "false" -> False

validateHead :: Text -> Bool
validateHead json = ((T.head json) == '{')

validateTrimmed :: Text -> Bool
validateTrimmed json = ((T.head json) == '{') && ((T.last json) == '}')

trimWS :: String -> Text
trimWS json = T.strip $ T.pack $ json

trimFig :: Text -> Text
trimFig json = dropAround (=='}') (dropAround (=='{') (json)) 

trimSquare :: Text -> Text
trimSquare json = dropAround (==']') (dropAround (=='[') (json)) 

splitByCommas :: Text -> [Text]
splitByCommas json = T.splitOn (T.pack ",") json

--getKeyValues :: [Text] -> [(JSON, String)]
--getKeyValues kvarray = [getLinesKeyValue kv | kv <-kvarray]
getSimpleKV :: Text -> ((JSON, String), Text)
getSimpleKV json = let kv = splitFirst json ":" in
                   getLinesKeyValue json kv


getLinesKeyValue :: Text -> [Text] -> ((JSON, String), Text)
getLinesKeyValue json [key1, value1] = 
                        let key =  T.unpack $ T.strip $ key1 in
                        let value = T.unpack (T.takeWhile (/=',')  (T.strip $ value1)) in
                        (valueCheck key value, T.dropWhile (/=',') json)

valueCheck :: String -> String -> (JSON, String)
valueCheck key value -- | (validateLevel value) =  ([], "") getLinesKeyValue $ T.pack $ value
                     | (isList value) = (getListValues value, key)
                     | (fst $ tryParseInt $ value) = (Int $ snd $ tryParseInt $ value, key)
                     | (value == "null") = (Null, key)
                     | (value == "true" || value == "false") = (Bool $ parseBool $ value, key)
                     | otherwise = (String value, key)

splitFirst :: Text -> String -> [Text]
splitFirst text separator = let splitResult = breakOn (T.pack $ separator) text in
                  [fst splitResult,  T.drop (Prelude.length separator) (snd splitResult) ]

lab3 (Object list) = 0

stringify :: JSON -> String
stringify (String str) = show str
stringify (Object []) = "{}"
stringify (Object tuples) = show tuples -- parse (fst $ list) --Show $ parse $ list --(stringify $ fst $ list) + " " + (snd $ list)
stringify (List []) = "[]"
stringify (List vals) = show vals
stringify (Int num) = show num
stringify (Null) = "null"
stringify (Bool bool) = if bool then "true" else "false"

getListOfKeys :: [(String, JSON)] -> [String]
getListOfKeys [] = []
getListOfKeys (x:xs) = unfold(x) ++ getListOfKeys(xs)
--getListOfKeys jsonObjects = Prelude.map (\x -> fst x) jsonObjects

unfold :: (String, JSON) -> [String]
unfold (x, Object []) = [x]
unfold (x, Object y) = [x] ++  [fst $ arg | arg <- y]
unfold (x, _) = [x]

-- вариант с монадой IO
generateIO :: IO JSON
generateIO = do
  num <- randomRIO (1, 11) :: IO Int
  let json = case num of
               1 -> Object [];
               2 -> Object [("empty", Object [])];
               3 -> Object [("string", String "yes")];
               4 -> Object [("num", Int 69)];
               5 -> Object [("bool", Bool True)];
               6 -> Object [("null", Null)];
               7 -> Object [("kv", Object [("key", String "value")])];
               8 -> String "just string";
               9 -> Int 420;
               10 -> List ["elem1", "elem2"]
               11 -> Null;
  return json

-- чистый вариант с генератором, заключённым в состояние
-- мы храним в состоянии генератор, каждый раз используя
-- его, возвращаем в состояние новый


type GeneratorState = State StdGen

generate' :: GeneratorState JSON
generate' = do
  gen <- get
  let (num, newGen) = randomR (1, 2) gen :: (Int, StdGen)
  let json = case num of
               1 -> Object [];
               2 -> Object [("pure", Object [])]
  put newGen
  return json

generate :: JSON
generate = evalState generate' (mkStdGen 0)

main :: IO() 
main = do
  handle <- openFile "sample.json" ReadMode
  contents <- hGetContents handle
  print (parse (contents))
  print ("My task: " ++ (show (getListOfKeys $ twist $ (parseLevel [] (trimFig $ T.pack $ contents)))))
  rands <- sequence [generateIO, generateIO, generateIO, generateIO, generateIO, generateIO]
  print "random JSON-objects:" 
  print $ show $ rands
  


{-main :: IO()
main = withSocketsDo $ do
  dir <- getCurrentDirectory
  initReq <- parseUrl "http://91.239.142.110:13666/lab2"
  handle <- openFile (dir ++ "/Lab2.hs") ReadMode
  hSetEncoding handle utf8_bom
  content <- hGetContents handle
  let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  hClose handle
  L.putStrLn $ responseBody response-}
