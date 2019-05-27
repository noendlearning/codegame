module HTTP.API.Handler where

import ClassyPrelude
import Network.Wai
import Network.HTTP.Types (status200, unauthorized401, status404)
import Tool.Types
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy.Internal as LI (ByteString)


resFile :: ByteString -> FilePath -> Response
resFile contentType filename = responseFile status200 [("Content-Type", contentType)] filename Nothing

-- 可以携带cookie到页面，cookie里放数据
resFile' ::ByteString-> FilePath -> Response
resFile' cookies filename = responseFile status200 [("Content-Type", "text/html"),("Cookie",cookies)] filename Nothing

serveStatic :: Text -> Text -> Response
serveStatic subDir fName =
    case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "images" -> serve "image/png"
    _ -> res404
    where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", unpack fName]
          sub = unpack subDir

res404 :: Response
res404 = responseLBS status404 [] $ fromString "Not Found"

resJson::LI.ByteString->IO Response
resJson jsonstr=
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ jsonstr

{-
可以携带cookie的放回json数据的方法
-}
resJson'::ByteString->LI.ByteString->IO Response
resJson' cookies jsonstr=
    return $ responseBuilder status200 [("Content-Type","application/json"),("Cookie",cookies)] $ lazyByteString $ jsonstr