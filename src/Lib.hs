{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import Network.Wai
import Network.Wai.Handler.Warp
-- import Network.Wai(Response(..))
import System.IO.Unsafe (unsafePerformIO)
-- import Blaze.ByteString.Builder
import Blaze.ByteString.Builder (fromByteString)
import Network.Wai.Internal
import Network.Wai.Parse (parseRequestBody,lbsBackEnd)
import Network.HTTP.Types (status200, unauthorized401, status404)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Data.ByteString.UTF8 (toString)
import Data.Text.Encoding (decodeUtf8)
import Data.String (fromString)
import qualified Data.ByteString as DB (concat)
import Data.Monoid
import Debug.Trace
import Data.Aeson (encode)
import Network.AWS.Data.Log
import qualified Text.HTML.TagStream.ByteString as THTB (cc)
-- import Data.Aeson.Parser (json)
import Model

someFunc = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req respond = respond $ 
    case pathInfo req of
        -- 参数传递的问题
        ["linux"] -> 
            -- Response
            -- unsafePerformIO $ testParam req
            testParam req
        ["static", subDir, fileName] -> 
            serveStatic subDir fileName
        [] -> 
            resFile "text/html" "static/index.html"
        ["favicon.ico"] -> 
            resPlaceholder
        _ -> res404    
-- get 

g :: Data.ByteString.Lazy.Internal.ByteString -> Data.ByteString.Builder.Internal.Builder
g a = ...

-- 仅post
testParam :: Request ->Response
testParam req=do
    (params, _) <- parseRequestBody lbsBackEnd req
    -- type Param = (ByteString, ByteString)  Data.ByteString params =[param] [("code","ls")]
    withParams params ["code"] answer
    where answer [code]=responseBuilder status200 [("Content-Type",contentType)] $ codeout
          codeout = encode(CodeOutput {code=decodeUtf8 code,output="i get ur code"})
          contentType= "application/json"

withParams :: BSAssoc -> [BS.ByteString] -> ([String] ->Response) -> Response
withParams params paramNames fn = 
  case extractParams params paramNames of
    Just paramVals -> 
      fn paramVals
    Nothing ->
      resError $ concat ["Need '", paramsList, "' parameters"]
      where paramsList = BS.unpack $ BS.intercalate "', '" paramNames

extractParams :: BSAssoc -> [BS.ByteString] -> Maybe [String]
extractParams params paramNames = do
  res <- allLookups params paramNames
  return $ map BS.unpack res

allLookups :: Eq a => [(a, a)] -> [a] -> Maybe [a]
allLookups assoc keys = sequence $ map (\k -> lookup k assoc) keys
    
resError :: String -> Response
resError message =responseLBS unauthorized401 [] $ fromString message

resFile :: BS.ByteString -> FilePath -> Response
resFile contentType filename = responseFile status200 [("Content-Type", contentType)] filename Nothing    

-- fromString :: String -> Builder
res404 :: Response
res404 = responseLBS status404 [] $ fromString "Not Found"

serveStatic :: Text.Text -> Text.Text -> Response
serveStatic subDir fName = 
  case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "img" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", Text.unpack fName]
        sub=Text.unpack subDir
        
resPlaceholder :: Response
resPlaceholder = responseLBS status404 [] $ fromString "Not implemented yet"