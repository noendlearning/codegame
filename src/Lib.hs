{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import Network.Wai
import Network.Wai.Handler.Warp
-- import Network.Wai(Response(..))
import System.IO.Unsafe (unsafePerformIO)
-- import Blaze.ByteString.Builder
import Data.ByteString.Builder (lazyByteString)
import Blaze.ByteString.Builder (fromByteString)
import Network.Wai.Internal
import Network.Wai.Parse (parseRequestBody,lbsBackEnd)
import Network.HTTP.Types (status200, unauthorized401, status404)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Data.ByteString.UTF8 (toString)
import Data.Text.Encoding (decodeUtf8)
import Data.String (fromString)
import qualified Data.ByteString as DB (concat,hPutStrLn)
import Data.Monoid
import Debug.Trace
import Data.Aeson (encode)
import Network.AWS.Data.Log
import qualified Text.HTML.TagStream.ByteString as THTB (cc)
import System.IO as IO
import Data.List as DL
-- import Data.ByteString.Builder (lazyByteString)
-- import Data.Aeson.Parser (json)
import Model
import System.Process
someFunc = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req respond = respond $ 
    case pathInfo req of
        -- 参数传递的问题
        ["linux"] -> 
            -- unsafePerformIO 函数是取出IO中的 Response
            unsafePerformIO $ testParam req
            -- testParam req
        ["static", subDir, fileName] -> 
            serveStatic subDir fileName
        [] -> 
            resFile "text/html" "static/index.html"
        ["favicon.ico"] -> 
            resPlaceholder
        ["init"] -> 
          trace "init" unsafePerformIO $ initCode req 
        _ -> res404    
-- get 

-- 返回初始的代码
initCode :: Request ->IO Response
initCode req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let pathName = "./static/init/"++(BS.unpack . snd $ head params)
    inpStr <- readFile pathName
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeList {codeList = inpStr})
-- 仅post
testParam :: Request ->IO Response
testParam req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    -- type Param = (ByteString, ByteString)  Data.ByteString params =[param] [("code","ls")]
    -- 使用JSON数据中的第一个元组的key当作文件名
    --traceM(show(params))
    let fileName = (BS.unpack . fst $ head params) ++".py"
    -- JSON数据写入文件的路径
    let pathName = "./static/code/" ++ fileName
    -- shell运行
    let order= "python "++ fileName
    -- 写入文件文件名不存在的时候会新建，每次都会重新写入
    outh <- IO.openFile pathName WriteMode
    DB.hPutStrLn outh $ BS.append (BS.pack "#!/user/bin/env python\r")  (snd $ head params)
    IO.hClose outh
    -- 用shell命令去给定位置找到文件运行脚本。得到输出的句柄。（输入句柄，输出句柄，错误句柄，不详）
   
    --获取输入参数的文件路径
    let factorPath = head $ getPath $ BS.unpack $ snd $ last params 
    inh <- openFile factorPath ReadMode
    (_,Just hout,Just err,_) <- createProcess (shell order){cwd=Just"./static/code",std_in = UseHandle inh,std_out=CreatePipe,std_err=CreatePipe}
    hClose inh
    -- 获取文件运行的结果
    content <- hGetContents hout
    let contents = DL.lines content
    -- 获取文件的错误信息
    errMessage <- hGetContents err
    --获取正确答案的路径
    let answerPath = last $ getPath $ BS.unpack $ snd $ last params
    --读取文件中保存的正确答案
    inpStr <- readFile answerPath
    let inpStrs = DL.lines inpStr
    let codeOutput =  if DL.null $ contents \\  inpStrs
                      then encode (CodeOutput {output=Text.pack content, message="Success", found="", expected="", errMessage=Text.pack errMessage})
                      else encode (CodeOutput {output=Text.pack content, message="Failure", found=DL.head inpStrs, expected=DL.head inpStrs, errMessage=Text.pack errMessage})
    -- 打印数据的方法 traceM(show(content))
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeOutput

getPath :: String -> [String]
getPath factorPath 
      | factorPath == "1" = ["./static/factor/factor1.txt","./static/answer/answer1.txt"]
      | factorPath == "2" = ["./static/factor/factor2.txt","./static/answer/answer2.txt"]
      | factorPath == "3" = ["./static/factor/factor3.txt","./static/answer/answer3.txt"]
      | otherwise         = ["./static/factor/factor4.txt","./static/answer/answer4.txt"]
    --暂时废废弃了
{- answer :: [String] -> Response
answer [name] =responseBuilder status200 [("Content-Type",contentType)] $ lazyByteString $ encode (CodeOutput {code=Text.pack name,output=message})
       where  contentType= "application/json"
              message ="i get ur code" 

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
resError message =responseLBS unauthorized401 [] $ fromString message -}

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