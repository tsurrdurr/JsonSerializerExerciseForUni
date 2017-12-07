module Lab2 where

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import System.Random
import Control.Monad
import Control.Monad.State
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Data.String
--import qualified StringUtils as SU --cabal install stringutils
import qualified Data.Text as T
import Data.Text
import Network (withSocketsDo)

-- почтовый адрес
email = "notlaika@protonmail.com"

data JSON = Object [(String, JSON)] | String String | List [String] | JSONList [JSON] deriving(Eq)

-- добавим сответствующие классы типов для JSON
instance Show JSON where
  show = stringify
instance Read JSON where
  readsPrec _ x = parse x

jsonSample = [(Object [], "sample json")]
jsonError = [(Object [], "parse error")]

parse :: ReadS (JSON)
parse "{}" = [(Object [], "")]


parse json | (validateLevel json) = parseLevel $ json
           | otherwise = jsonError

parseLevel :: String -> [(JSON, String)]
parseLevel json = (if True then getKeyValues else getKeyValues) $ splitJson $ T.strip $ trimFig $  T.pack $ json

validateLevel :: String -> Bool
validateLevel json = validateTrimmed $ trimWS $ json 

validateTrimmed :: Text -> Bool
validateTrimmed json = ((T.head json) == '{') && ((T.last json) == '}')

trimWS :: String -> Text
trimWS json = T.strip $ T.pack $ json

trimFig :: Text -> Text
trimFig json = dropAround (=='}') (dropAround (=='{') (json)) 

splitJson :: Text -> [Text]
splitJson json = T.splitOn (T.pack ",") json

getKeyValues :: [Text] -> [(JSON, String)]
getKeyValues kvarray = [getLinesKeyValue kv | kv <-kvarray]

--getListValues :: [Text] -> [String]
--getListValues textarray = [ T.strip $ T.pack $ txt | txt <- textarray] 

getLinesKeyValue :: Text -> (JSON, String)
getLinesKeyValue line = let kvpair = T.splitOn (T.pack $ ":") line in
                        let key =  T.unpack $ T.strip $ Prelude.head $ kvpair in
                        let value = T.unpack $ T.strip $ Prelude.last $ kvpair in
                        if (validateLevel value) then getLinesKeyValue $ T.pack $ value
                        else (String value, key)


splitFirst :: Text -> [Text]
splitFirst text = let splitResult = breakOn (T.pack $ ":") text in
                  [fst splitResult,  T.drop 1 (snd splitResult) ]

lab3 (Object list) = 0

stringify :: JSON -> String
stringify (String str) = "\"" ++ str ++ "\""
stringify (Object []) = "{}"
stringify (Object list) = "{1}" -- parse (fst $ list) --Show $ parse $ list --(stringify $ fst $ list) + " " + (snd $ list)
stringify (List []) = "[]"
stringify (List str) = show str

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
