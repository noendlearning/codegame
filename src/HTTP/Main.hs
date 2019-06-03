module HTTP.Main where


import ClassyPrelude

import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import qualified HTTP.API.Auth as Api
import Network.HTTP.Types (status200, unauthorized401, status404)
import HTTP.API.Handler

main ::Int -> IO ()
main port = do
  putStrLn "open port:"
  print port
  run port app

app :: Application
app req respond =  do
  res <- Api.hasCookieInfo req
  --traceM(show(res))
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
        -- ["toplay"]->
        --   resFile "text/html" "static/play.html"
        ["addpuzzle"]->
          resFile "text/html" "static/addpuzzle.html"
          -- 去easy页面
        ["easy"]->
          resFile "text/html" "static/easy.html"
        ["gohome"]->
          resFile "text/html" "static/home.html"
        ["training"]->
          unsafePerformIO $ Api.playWithPuzzleUUID req
        ["list"]->
          resFile "text/html" "static/list.html"
          -- 获取所有的puzzle
        ["allpuzzles"]->
          unsafePerformIO $ Api.listAll req
        ["easypuzzles"]->
          unsafePerformIO $ Api.categoryPuzzles "Easy" req
        ["play"]->
          unsafePerformIO $ Api.testParam "cookieMess" req
        ["init"] ->
          unsafePerformIO $ Api.initCode req
        ["language"]->
          unsafePerformIO $ Api.selectAllLanguage
        _->
          resFile "text/html" "static/list.html"
    Just cookieMess->
      respond $
        case pathInfo req of
          ["toplay"]->
            resFile "text/html" "static/html/play.html"
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
            resFile "text/html" "static/index.html"
          ["index"] ->
            resFile "text/html" "static/index.html"
          ["gohome"] ->
            resFile "text/html" "static/home.html"
          ["addpuzzle"] ->
            resFile "text/html" "static/addpuzzle.html"
          -- 去easy页面
          ["easy"]->
            resFile "text/html" "static/easy.html"
          ["easypuzzles"]->
            unsafePerformIO $ Api.categoryPuzzles "Easy" req
          ["code"] ->
            unsafePerformIO $ Api.resData req
          _ -> res404