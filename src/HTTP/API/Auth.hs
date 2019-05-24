module HTTP.API.Auth where

import ClassyPrelude
--import Adapter.HTTP.Common
import Network.Wai
import Network.Wai.Handler.Warp
import Data.ByteString.Builder (lazyByteString)
import Network.Wai.Parse (parseRequestBody,lbsBackEnd)
import Network.HTTP.Types (status200, unauthorized401, status404)
import qualified Mysql.Database as M
import qualified Redis.Auth as R
import qualified Data.Map.Lazy as MAP
--import Domain.Auth
import Model
import qualified Data.Maybe as DM
import qualified HTTP.API.Tool as Tool
import Data.Aeson
import System.Process
import qualified System.IO.Strict as IS (hGetContents)
import qualified Data.List as List
import System.IO as IO
import qualified HTTP.SetCookie as Cookie
import Tool.Types
import qualified System.Directory as Dir
import Tool.Types
import Text.Read
import Control.Exception
import Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Internal as LI (ByteString)

-- 获取初始化代码
initCode ::Request ->IO Response
initCode req = do
    {- M.insertValidationWithPuzzleId $ M.Validation ""  "1" "4\n5\nE\n #  ##   ## ##  ### ### ##  # # ###  ## # # #   # # ###  #  ##   #  ##  ##  ### # # # # # # # # # # ### ### \n# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # \n### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## \n# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       \n# # ##   ## ##  ### #   ##  # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  "
        "### \n#   \n##  \n#   \n### " Major 1 "createBy" Nothing (Just "updateBy")  Nothing Normal "" -}
    (params, _) <- parseRequestBody lbsBackEnd req
    let paramsMap = mapFromList params :: Map ByteString ByteString
    solution <- M.selectSolutionByUUID ((unpack . decodeUtf8) $ paramsMap MAP.! "puzzleId") $ (unpack . decodeUtf8) $ paramsMap MAP.! "languageId"
    let code = M.solutionCode $ List.head solution
        codeList= encode (CodeList {codeList = code})
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeList

--用户登录
loginUser :: Request ->IO Response
loginUser req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    --traceM(show(params))
    let paramsMap = mapFromList params :: Map ByteString ByteString
    -- 用户登录
    result <-M.login ((unpack . decodeUtf8) $ paramsMap MAP.! "email") $ (unpack . decodeUtf8) $ paramsMap MAP.! "passw"
   -- traceM(show("==============="))
   -- traceM(show(result))
    case result of
        [] -> return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (Output {msg= "用户不存在", state="3"})
        [_] ->  do
            --用户登录成功后新建一个 session ,session里面存的是用户邮箱
            sessionId <- liftIO $ R.newSession ((unpack . decodeUtf8) $ paramsMap MAP.! "email")
            -- 把上一步返回的sessionId为设置cookie里面
            cookies <- Cookie.setSessionIdInCookie sessionId
            return $ responseBuilder status200 [("Content-Type","application/json"),("Cookie",cookies)] $ lazyByteString $ encode (Output {msg= "欢迎登录", state="5"})

--退出登录
quitUser ::ByteString ->  Request ->IO Response
quitUser cookieMess req = do
      sessionId <- getCookie cookieMess "sessionId"
      number <- R.deleteUserIdBySessionId sessionId
      case number of
        0 -> return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (Output {msg= "用户不存在",state="3"})
        1 ->  do
            return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (Output {msg= "欢迎下次登录",state="4"})

-- 用户注册
registerUser::Request->IO Response
registerUser req = do
  (params, _) <- parseRequestBody lbsBackEnd req
 -- traceM(show(params))
  let paramsMap = mapFromList params :: Map ByteString ByteString
      email=((unpack . decodeUtf8) $ paramsMap MAP.! "email")
      pwd=(unpack . decodeUtf8) $ paramsMap MAP.! "passw"
  --根据email 查找 userId
  user <- M.selectUserByUserEmail email
  case user of
    [_] ->  return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (Output {msg= "该Email已经注册", state="1"})
    [] -> do
      M.insertUser $ M.User "" email pwd Nothing Nothing Normal
      return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (Output {msg= "注册成功", state="2"})

-- 提交代码验证是否正确
testParam ::ByteString -> Request ->IO Response
testParam cookieMess req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    sessionId <- getCookie cookieMess "sessionId"
    email <- R.findUserIdBySessionId sessionId
    case email of
      Nothing -> do
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= fromString "cookie失效，请先登录在提交代码", message="", found="", expected="", errMessage= ""})
      Just email -> do
        -- 为每个用户新建一个文件夹，这个是文件夹的路径。使用完之后删除
        let userFolder = "./static/"++ email
        --新建文件夹
        Dir.createDirectory userFolder
        --返回代码写入文件的路径和shell脚本在哪个路径下运行的命令
        let paramsMap = mapFromList params :: Map ByteString ByteString
            language = (unpack . decodeUtf8) (paramsMap MAP.! "language")
            code = (unpack . decodeUtf8) (paramsMap MAP.! "code")
            testIndex = (unpack . decodeUtf8) (paramsMap MAP.! "testIndex")
            puzzleId = (unpack . decodeUtf8) (paramsMap MAP.! "puzzleId")
            languagesId = (unpack . decodeUtf8) (paramsMap MAP.! "languagesId")
            languageSetting = Tool.getLanguageSetting  language code userFolder
        -- 写入文件文件名不存在的时候会新建，每次都会重新写入
        outh <- IO.openFile (List.head languageSetting) WriteMode
        hPutStrLn outh (languageSetting List.!! 1)
        IO.hClose outh
        -- 查询数据库中的正确答案和题目需要的参数
        puzzle <- M.selectValidationByPuzzleId puzzleId (read $ (unpack . decodeUtf8) (paramsMap MAP.! "testIndex") :: Int)
        let input = M.validationInput $ List.head puzzle
        --把查询出来的题目参数写入文件
        out <- IO.openFile (userFolder ++ "/factor.txt") WriteMode
        hPutStrLn out input
        IO.hClose out
        inh <- openFile (userFolder ++ "/factor.txt") ReadMode
        -- 用shell命令去给定位置找到文件运行脚本。得到输出的句柄。（输入句柄，输出句柄，错误句柄，不详）
        (_,Just hout,Just err,_) <- createProcess (shell (List.last languageSetting)){cwd=Just userFolder,std_in = UseHandle inh,std_out=CreatePipe,std_err=CreatePipe}
        IO.hClose inh
        -- 获取文件运行的结果
        content <- timeout 2000000 (IS.hGetContents hout)
        errMessage <- IS.hGetContents err
        --删除文件夹及其内容和子目录
        Dir.removeDirectoryRecursive userFolder
        case content of
          Nothing  ->
            return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= fromString "Timeout: your program did not provide an input in due time.", message="Failure", found="", expected="", errMessage= fromString errMessage})
          Just value  -> do
            let contents = List.lines value
            --从数据库中获取正确的答案
            let output = M.validationOutput $ List.head puzzle
                inpStrs = List.lines output
                codeOutput =  if contents == inpStrs
                              then
                                encode (CodeOutput {output=fromString value, message="Success", found="", expected="", errMessage= fromString errMessage})
                              else encode (CodeOutput {output=fromString value, message="Failure", found=List.head contents, expected=List.head inpStrs, errMessage= fromString errMessage})
            --保存用户提交的代码
            --M.insertCode $ M.Code "" userId "1" "python" code Nothing Nothing
            --根据email 查找 userId
            user <- M.selectUserByUserEmail email
            --更新用户提交的代码
            --M.updateCode $ M.Code "" (M.userEmail $ List.head user) puzzleId languagesId code Nothing Nothing
            return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeOutput

--通过puzzleid查询Validateion表，Puzzle表，Solution表内容
selectAllByPuzzleUUID ::Request->IO Response
selectAllByPuzzleUUID req = do 
             let params = paramFoldr (queryString req) 
                 paramsMap = mapFromList params
                 uuid = (unpack . decodeUtf8) (paramsMap MAP.! "uuid")   
             all <- M.selectPuzzleAll uuid  
             case all of
                ([],[],[])->
                  return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ "数据库查询出错"
                (_)->
                  return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode all


getPuzzleInput :: String ->IO PuzzleInput
getPuzzleInput puzzleInput = do
     case decode $ fromString puzzleInput :: Maybe PuzzleInput of
       Just puzzle -> return puzzle
getTestCase :: String ->IO TestCase
getTestCase testCase = do
     case decode $ fromString testCase :: Maybe TestCase of
       Just testcase -> return testcase
getStandCase :: String ->IO StandCase
getStandCase standCase = do
     case decode $ fromString standCase :: Maybe StandCase of
       Just standcase -> return standcase

resData ::Request ->IO Response
resData req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let paramsMap = mapFromList params :: Map ByteString ByteString
    a <-getPuzzleInput $ (unpack . decodeUtf8) (paramsMap MAP.! "puzzle-one")
    b <-getTestCase $ (unpack . decodeUtf8) (paramsMap MAP.! "puzzle-two")
    c <-getStandCase $ (unpack . decodeUtf8)  (paramsMap MAP.! "puzzle-three")
    let title'=title a
        statement'=statement a
        inputDescription'=inputDescription a
        outputDescription'=outputDescription a
        constraints'=constraints a
    let testName'=testName b
        validator'=validator b
    let input'=input c
        oput'=oput c
    --traceM(show(a,b,c))
    outh <- IO.openFile "./static/code/User.txt" WriteMode
    hPutStrLn outh (show $ title'<>statement'<>inputDescription'<>outputDescription'<>constraints')
    hPutStrLn outh (testName'<>validator')
    hPutStrLn outh (input'<>oput')
    IO.hClose outh
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ " "

-- 请求里是否携带cookie信息
hasCookieInfo::Request ->IO (Maybe ByteString)
hasCookieInfo req=do
  let reqHeaders = requestHeaders req
  --把请求头变成Map
  let reqMap = MAP.fromList reqHeaders
      --获取具体的请求头的value
  return $ reqMap MAP.!? (fromString "Cookie")

--解析Cookie 参数cokieKey是，请求头中的cookie里面key.返回key对应的value值
getCookie :: ByteString -> String -> IO String
getCookie cookieMess cokieKey= do
  sesso <- Cookie.getCookie cookieMess cokieKey
  case sesso of
    Just realityMess -> return $ realityMess
    Nothing -> return $ "根据这个" <> cokieKey <> "为key没有对应的value "

listAll::Request ->IO Response
listAll req=do
  puzzles<-M.selectPuzzleByState Public
  case puzzles of
    []->
      return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ "数据库查询出错"
    _->
      return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode puzzles

{-
根据类别查询puzzles，
easy medium hard professional
-}
-- encode (Output {msg= "该Email已经注册", state="1"})
categoryPuzzles::PCategory->Request->IO Response
categoryPuzzles p req=do
    puzzles<-M.selectPuzzleByCategory  p 0
    case puzzles of
      []->
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ "数据库查询出错"
      _->
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode puzzles

--处理get参数的方法
paramFoldr :: [(a,Maybe b)] -> [(a,b)] 
paramFoldr xs = foldr (\x acc -> (fst x , DM.fromJust $ snd x) : acc) [] xs 
