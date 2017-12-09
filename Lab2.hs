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


parse json | (validateLevel json) = parseLevel [] json
           | otherwise = jsonError

isListOfValues :: [Text] -> Bool 
isListOfValues keyRestOfDoc = let openingSymbol = T.head $ Prelude.last $ keyRestOfDoc in
                              if (openingSymbol=='[') then True
                              else False

hasDepth :: [Text] -> Bool
hasDepth keyRestOfDoc = False

getListOfValues :: [Text] -> (JSON, String)
getListOfValues [key, rest] =  let contents = Prelude.last (splitFirst (Prelude.head $ (splitFirst rest "]")) "[") in
                               let restOfDoc = Prelude.last $ (splitFirst rest "]") in
                               (getListValues $ T.unpack $ contents, T.unpack $ key)

parseLevel :: IO [(JSON, String)] -> String -> [(JSON, String)]
parseLevel acc json = do
                      let keyAndRestOfDocument = splitFirst (T.strip $ trimFig $ T.pack $ json) ":" in 
                        let key = T.unpack $ Prelude.head $ keyAndRestOfDocument in
                          let acc1 = [] in
                          if (isListOfValues keyAndRestOfDocument) then do (getListOfValues $ keyAndRestOfDocument) : acc
                          else if (hasDepth keyAndRestOfDocument) then do (Object (ankle (parseLevel [] "")), key) : acc
                          else do (getLinesKeyValue $ keyAndRestOfDocument) : acc
                      return Prelude.head acc


twist :: (a, b) -> (b, a)
twist (x,y) = (y,x)

ankle :: [(JSON, String)] -> [(String, JSON)]
ankle xs = Prelude.map twist xs

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

getKeyValues :: [Text] -> [(JSON, String)]
getKeyValues kvarray = [getLinesKeyValue kv | kv <-kvarray]

getListValues :: String -> JSON
getListValues str = let textarray = splitByCommas $ trimSquare $ T.pack $ str in
                    List [ T.unpack text | text <- textarray ]

getLinesKeyValue :: [Text] -> (JSON, String)
getLinesKeyValue [key1, value1] = 
                        let key =  T.unpack $ T.strip $ key1 in
                        let value = T.takeWhile (/=',')  (T.unpack $ T.strip $ value1) in
                        valueCheck key value

valueCheck :: String -> String -> (JSON, String)
valueCheck key value | (validateLevel value) = getLinesKeyValue $ T.pack $ value
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
stringify (String str) = "\"" ++ str ++ "\""
stringify (Object []) = "{}"
stringify (Object list) = "{1}" -- parse (fst $ list) --Show $ parse $ list --(stringify $ fst $ list) + " " + (snd $ list)
stringify (List []) = "[]"
stringify (List str) = show str
stringify (Int num) = show num
stringify (Null) = "null"
stringify (Bool bool) = if bool then "true" else "false"

-- вариант с монадой IO
generateIO :: IO JSON
generateIO = do
  num <- randomRIO (1, 2) :: IO Int
  let json = case num of
               1 -> Object [];
               2 -> Object [("io", Object [])]
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
