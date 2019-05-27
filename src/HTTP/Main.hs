module HTTP.Main where


import ClassyPrelude

import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import qualified HTTP.API.Auth as Api
import Network.HTTP.Types (status200, unauthorized401, status404)
import Tool.Types

main ::Int -> IO ()
main port = do
  putStrLn "open port:"
  print port
  run port app

app :: Application
app req respond =  do
  res <- Api.hasCookieInfo req
  -- traceM(show(res))
  case res of
    Nothing->
      respond $
      case pathInfo req of
      -- fixme: 转发到index请求
        ["loginUser"] ->
          unsafePerformIO $ Api.loginUser req
        ["registerUser"] ->
          unsafePerformIO $ Api.registerUser req
        ["static", subDir, fileName] ->
          serveStatic subDir fileName
        ["toplay"]->
          resFile "static/play.html"
        ["addpuzzle"]->
          resFile "static/addpuzzle.html"
          -- 去practice页面
        ["easy"]->
          resFile "static/easy.html"
        ["gohome"]->
          resFile "static/home.html"
        ["list"]->
          resFile "static/list.html"
          -- 获取所有的puzzle
        ["allpuzzles"]->
          unsafePerformIO $ Api.listAll req
        ["easypuzzles"]->
          trace "easypuzzles" unsafePerformIO $ Api.categoryPuzzles Easy req
        _->
          resFile "static/index.html"
    Just cookieMess->
      respond $
        case pathInfo req of
          ["toplay"]->
            resFile "static/html/play.html"
          ["quitUser"] ->
              unsafePerformIO $ Api.quitUser cookieMess req
          ["loginUser"] ->
              unsafePerformIO $ Api.loginUser req
          ["registerUser"] ->
              unsafePerformIO $ Api.registerUser req
          ["play"] ->
                -- unsafePerformIO 函数是取出IO中的 Response
                unsafePerformIO $ Api.testParam cookieMess req
          ["init"] ->
            unsafePerformIO $ Api.initCode req
          ["allpuzzles"]->
            unsafePerformIO $ Api.listAll req
          ["static", subDir, fileName] ->
                serveStatic subDir fileName
          [] ->
            resFile "static/index.html"
          ["index"] ->
            resFile "static/index.html"
          ["gohome"] ->
            resFile "static/home.html"
          ["addpuzzle"] ->
            resFile "static/addpuzzle.html"
          -- 去easy页面
          ["easy"]->
            resFile "static/easy.html"
          ["easypuzzles"]->
            unsafePerformIO $ Api.categoryPuzzles Easy req
          ["code"] ->
            unsafePerformIO $ Api.resData req
          _ -> res404

resFile :: FilePath -> Response
resFile filename = responseFile status200 [("Content-Type", "text/html")] filename Nothing

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