{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import Network.Wai
import Network.Wai.Handler.Warp
-- import Network.Wai(Response(..))
import System.IO.Unsafe (unsafePerformIO)
-- import Blaze.ByteString.Builder
import Data.ByteString.Builder (lazyByteString)
import Blaze.ByteString.Builder (fromByteString)
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
import Data.Aeson (encode,decode)
import Network.AWS.Data.Log
import qualified Text.HTML.TagStream.ByteString as THTB (cc)
import System.IO as IO
import qualified Data.List as DL
import qualified Data.Map.Lazy as DML
-- import Data.ByteString.Builder (lazyByteString)
-- import Data.Aeson.Parser (json)
import Model
import System.Process
import System.IO.Strict as IS (hGetContents)
import System.Timeout
import qualified Data.Maybe as M
import qualified Database.MySQL.Base as Sql
import qualified System.IO.Streams as Streams
someFunc = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req respond = respond $ 
    case pathInfo req of
        -- 参数传递的问题
        ["play"] -> 
            -- unsafePerformIO 函数是取出IO中的 Response
            unsafePerformIO $ testParam req
            -- testParam req
        ["linuxget"] ->
            unsafePerformIO $ testParamGet req
        ["registUser"] -> 
          unsafePerformIO $ registUser req 
        ["loginUser"] -> 
            unsafePerformIO $ loginUser req       
        ["static", subDir, fileName] -> 
            serveStatic subDir fileName
        [] -> 
            resFile "text/html" "static/index.html"
        ["favicon.ico"] -> 
            resPlaceholder
        ["init"] -> 
            unsafePerformIO $ initCode req 
        _ -> res404    
-- get 
testParamGet :: Request ->IO Response
testParamGet req = do
    -- type Param = (ByteString, ByteString)  Data.ByteString params =[param] [("code","ls")]
    -- 使用JSON数据中的第一个元组的key当作文件名
    --traceM(show(params))
    let params = paramFoldr (queryString req)
    return res404
-- 返回初始的代码
initCode :: Request ->IO Response
initCode req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let paramsMap = DML.fromList $ changRequestType params
    let language =(paramsMap DML.! "language")
    -- FIXME 文件名
    let pathName = "./static/init/"++ case language of
                                          -- "python"->"python.py" 
                                          "java"->"Solution.java"
                                          "haskell"->"haskell.hs"
                                          _->"python.py"
    inpStr <- readFile pathName
    -- ,language=if language == "" then "python" else language
    let codeList=encode (CodeList {codeList = inpStr})
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeList
-- 仅post
testParam :: Request ->IO Response
testParam req = do
    (params, _) <- parseRequestBody lbsBackEnd req
     --开启一个连接 这个方法是自己封装的 使用完之后记得关闭
    let conn = getConn
    -- 曾删改时使用 execute_ 方法 会返回是否成功
    result <- Sql.execute_ conn "insert into user (user_name,user_pawd,creation_time) values ('王五','159753',now())"
    traceM(show(result))
    --查询数据库时使用 query_ 方法 返回的是个元祖，元祖中第一位返回的是一些数据的信息，第二位才是查询的结果信息
    (base,value) <- Sql.query_ conn "select * from user"
    --返回的value实际是 InputStream [MySQLValue] 这个类型，所以通过toList方法变成[MySQLValue] 这个类型
    result1 <- Streams.toList value
    traceM(show(result1))
    -- 关闭连接
    Sql.close getConn 
    --返回代码写入文件的路径和shell脚本在哪个路径下运行的命令
    let paramsMap = DML.fromList $ changRequestType params
    let languageSetting = getLanguageSetting  (paramsMap DML.! "language") (paramsMap DML.! "code")
    -- 写入文件文件名不存在的时候会新建，每次都会重新写入
    outh <- IO.openFile (head languageSetting) WriteMode
    DB.hPutStrLn outh (BS.pack ((!!) languageSetting 2))
    IO.hClose outh
    -- 用shell命令去给定位置找到文件运行脚本。得到输出的句柄。（输入句柄，输出句柄，错误句柄，不详）
    --获取输入参数的文件路径
    let factorPath = head $ getPath (paramsMap DML.! "testIndex")
    inh <- openFile factorPath ReadMode
    (_,Just hout,Just err,_) <- createProcess (shell (last languageSetting)){cwd=Just((!!) languageSetting 1),std_in = UseHandle inh,std_out=CreatePipe,std_err=CreatePipe}
    hClose inh
    -- 获取文件运行的结果
    content <- timeout 2000000 (IS.hGetContents hout)
    errMessage <- IS.hGetContents err
    case content of
      Nothing     -> 
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output=Text.pack "Timeout: your program did not provide an input in due time.", message="Failure", found="", expected="", errMessage=Text.pack errMessage})
      Just value  -> do 
        let contents = DL.lines value
        -- 读取文件中保存的正确答案
        inpStr <- readFile (last $ getPath (paramsMap DML.! "testIndex"))
        let inpStrs = DL.lines inpStr
        let codeOutput =  if contents == inpStrs
                          then encode (CodeOutput {output=Text.pack value, message="Success", found="", expected="", errMessage=Text.pack errMessage})
                          else encode (CodeOutput {output=Text.pack value, message="Failure", found=DL.head contents, expected=DL.head inpStrs, errMessage=Text.pack errMessage})
                  -- 打印数据的方法 traceM(show(content))
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeOutput  
    
    
    


changRequestType :: [(BS.ByteString,BS.ByteString)] -> [(String,String)]
changRequestType [] = []
changRequestType (x:xs) = 
  [(BS.unpack (fst x),BS.unpack (snd x))]  ++ changRequestType xs
  
getLanguageSetting :: String -> String -> [String]
getLanguageSetting language code
      | language == "python" = ["./static/code/Python3.py", "./static/code", "#!/user/bin/env python\r" ++ code, "python Python3.py"]
      | language == "java"   = ["./static/code/Solution.java", "./static/code", code, "javac Solution.java && java Solution"]
      | language == "haskell"   = ["./static/code/Haskell.hs", "./static/code", code, "runghc Haskell.hs"]
      | otherwise            = ["",""]    

getPath :: String -> [String]
getPath factorPath 
      | factorPath == "1" = ["./static/factor/factor1.txt","./static/answer/answer1.txt"]
      | factorPath == "2" = ["./static/factor/factor2.txt","./static/answer/answer2.txt"]
      | factorPath == "3" = ["./static/factor/factor3.txt","./static/answer/answer3.txt"]
      | factorPath == "4" = ["./static/factor/factor4.txt","./static/answer/answer4.txt"]
      | otherwise         = ["./static/factor/factor5.txt","./static/answer/answer5.txt"]      
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
    "images" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", Text.unpack fName]
        sub=Text.unpack subDir
        
resPlaceholder :: Response
resPlaceholder = responseLBS status404 [] $ fromString "Not implemented yet"

paramFoldr :: [(a,Maybe b)] -> [(a,b)]
paramFoldr xs = foldr (\x acc -> (fst x , M.fromJust $ snd x) : acc) [] xs

--用户登录
loginUser :: Request ->IO Response
loginUser req = do
    (params, _) <- parseRequestBody lbsBackEnd req
  
    let pathName = "./static/code/User.txt" 
    --获取目标文件中的内容
    contentsp<-readFile pathName
    --比较登录时的用户名和密码是否和文件中一样
    --traceM(show(contentsp))
    let boolPar = elem (userPass params) (lines $ contentsp)
    
    if   boolPar
    then   return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= "欢迎登录", message="", found="", expected="", errMessage=""})
    else   return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= "用户不存在，请注册", message="", found="", expected="", errMessage=""})


--注册用户信息到文件中
registUser :: Request ->IO Response
registUser req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  let pathName = "./static/code/User.txt"
  contentsp <- readFile pathName
  let boolPar = elem (takeWhile (/= ',') $ userPass params) $ map (takeWhile (/= ',')) (lines $ contentsp)
  registResponse pathName params boolPar
  
registResponse :: String ->[(BS.ByteString,BS.ByteString)] -> Bool -> IO Response
registResponse pathName params boolPar
    | boolPar == False =  do
      outh <- IO.openFile pathName AppendMode 
      DB.hPutStrLn outh $ fromString $ userPass params
      IO.hClose outh
      return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= "注册成功", message="", found="", expected="", errMessage=""})
    | boolPar == True =  
      return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= "注册失败", message="", found="", expected="", errMessage=""})
     
userPass :: [(BS.ByteString,BS.ByteString)] -> [Char]
userPass [x] = BS.unpack (snd x)
userPass (x:xs) = 
  BS.unpack (snd x)  ++ "," ++ userPass xs
--连接数据库获取连接
getConn :: Sql.MySQLConn
getConn =  
  -- 可以使用 ciHost 设置ip, ciPort 设置端口。 默认的ip 127.0.0.1 端口号 3306
  unsafePerformIO $ Sql.connect $ Sql.defaultConnectInfo {Sql.ciUser = "root", Sql.ciPassword = "1", Sql.ciDatabase = "codegame"}