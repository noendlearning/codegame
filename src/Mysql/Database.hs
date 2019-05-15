module Mysql.Database   where

import ClassyPrelude
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader

import           Database.Persist   --
import           Database.Persist.MySQL  --
import           Database.Persist.TH   --

import Database.Persist.Class (PersistField(toPersistValue))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import qualified Data.UUID                           as DU (UUID, toString)
import qualified Data.UUID.V4                        as UV (nextRandom)

import           Database.Esqueleto                  as E    --

import           Database.Persist.Sql.Types.Internal
import Data.Password.Instances
import Data.Text.Internal
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Time
import qualified Tool.Constant as Constant

share  
  [mkPersist sqlSettings, mkMigrate "migrateAll"] 
  [persistLowerCase|
User
    uuid String
    UniqueUserUuid uuid
    email String
    UniqueUserEmail email
    password String 
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateTime UTCTime Maybe
    state Int Maybe
    deriving Show
Puzzle
    author String
    uuid String
    UniquePuzzleUuid uuid  
    title String
    createTime UTCTime default=CURRENT_TIMESTAMP
    createBy String 
    updateBy String Maybe
    updateTime UTCTime Maybe
    inputDescription String
    outputDescription String
    constraints String
    category String
    state Int 
    deriving Show
Solution
    uuid String
    UniqueSolutionUuid uuid   --Unique：唯一
    language String  
    code String
    puzzleId String
    updateTime UTCTime Maybe
    updateBy String Maybe
    createTime UTCTime default=CURRENT_TIMESTAMP
    createBy String
    unsolve String
    state Int 
    deriving Show
Languages
    uuid String
    UniqueLanguagesUuid uuid
    UniqueLanguagesLanguage language
    language String
    createBy String
    createTime UTCTime default=CURRENT_TIMESTAMP
    updateBy String Maybe
    updateTime UTCTime Maybe
    state Int 
    deriving Show
Validation
    uuid String
    UniqueValidationUuid uuid
    puzzleId String
    input String
    output String 
    category Int 
    orders Int 
    createBy String
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateBy String Maybe
    updateTime UTCTime Maybe
    state Int 
    deriving Show
|]

-- 共用mysql数据库连接信息
inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStderrLoggingT $ withMySQLPool conInfo 5 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        action

{- 
? puzzle crud
-}    

insertPuzzle ::Puzzle->IO ()
insertPuzzle = undefined


--通过PuzzleUuid 查询所有Puzzle相关内容 包含Puzzle,Validation,Solution三张表
queryAllPuzzlesByUuid :: String -> IO [String]
queryAllPuzzlesByUuid uuid = undefined
 --           let puzzle = selectPuzzleByUUID uuid
 --               valida = selectValidationByUUID uuid
 --               solution = selectSolutionByUUID uuid
 

--通过Puzzle表的uuid 查询Puzzle表内容
selectPuzzleByUUID::String->IO [Puzzle]
selectPuzzleByUUID uuid=
    inBackend  $ do
        puzzle <- E.select $
                  E.from $ \p -> do
                  E.where_ (p ^. PuzzleUuid E.==. E.val uuid)
                  return p
        liftIO $ mapM (return . entityVal) (puzzle :: [Entity Puzzle])

--通过Puzzle表的uuid 查询Validateion表内容
selectValidationByUUID :: String -> IO [Validation]
selectValidationByUUID uuid = 
    inBackend  $ do
        valida <-   E.select $
                    E.from $ \p -> do
                    E.where_ (p ^. ValidationPuzzleId E.==. E.val uuid)
                    return p
        liftIO $ mapM (return .entityVal) (valida :: [Entity Validation])

--通过Puzzle表的uuid 查询Solution表内容
selectSolutionByUUID :: String -> IO [Solution]
selectSolutionByUUID uuid = 
    inBackend $ do
        solution  <- E.select $
                     E.from $ \p -> do
                     E.where_ (p ^. SolutionPuzzleId E.==. E.val uuid)
                     return p
        liftIO $ mapM (return .entityVal) (solution :: [Entity Solution])                

updatePuzzle :: Puzzle->IO ()
updatePuzzle = undefined
{- 
? solution crud
-}
-- todo
insertSolutionWithPuzzleId::Solution->IO()
insertSolutionWithPuzzleId (Solution _ languagesUuid _ puzzleId _ _ _ userUuid _ _) =
    inBackend  $ do
        let uuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        do
            insert_ $ Solution ( DU.toString uuid ) languagesUuid "ssss" puzzleId  (Just now)  Nothing   now  userUuid  "bbb"   0
--      insert_ $ Solution (DU.toString uuid) (getLanguage uuid) (code) (getPuzzleId uuid) () () () () (unsolve)   0
--获取 Soultion表中puzzleId
getPuzzleId :: String -> IO [String]
getPuzzleId uuid =
    inBackend $ do
        puzzleId <- E.select $
                    E.from $ \p  -> do
                    E.where_ ( p^. PuzzleAuthor  E.==. E.val uuid)
                    return p
        liftIO $ mapM (return . puzzleUuid . entityVal) (puzzleId :: [Entity Puzzle] )          
--获取 Soultion表中language
getLanguage :: String -> IO [String]
getLanguage state =
    inBackend $ do 
        language <- E.select $ 
                    E.from $ \l -> do
                    E.where_ (l ^. LanguagesState E.==. E.val Constant.normalState)
                    return l
        liftIO $ mapM (return . languagesUuid . entityVal)  (language :: [Entity Languages] )           
-- todo
updateSolution ::Solution->IO ()
updateSolution=undefined


{- 我写
? validation crud
-}
-- todo
insertValidationWithPuzzleId::Validation -> IO()
insertValidationWithPuzzleId (Validation _ puzzleid input output category orders createBy _ updateBy _ _) =
    inBackend $ do
        let uuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        --getCurrentTime：从系统时间获取当前utctime
        insert_ $ Validation (DU.toString uuid) puzzleid "a" "a" 0 1 createBy  (Just now)  updateBy  (Just now)  0
--类别：1 2 3 4 简单 中等 困难 专家
--      insert_ $ Validation (DU.toString uuid) puzzleId input output category orders createBy createTime updateBy updateTime (Just 0)
--                              uuid            迷题uuid 输入   输出   类别     序号  创建人    创建时间   更新人   更新时间   state

--获取Puzzle，根据创建人的uuid获取Puzzle表的uuid
selectPuzzleuuidByUserUuid :: String -> IO [String]
selectPuzzleuuidByUserUuid uuid=
    inBackend $ do
        puzzle<- E.select $ 
                 E.from $ \p->do
                 E.where_ (p ^. PuzzleCreateBy E.==. E.val uuid)
                 return p
        liftIO $ mapM (return . puzzleUuid . entityVal) (puzzle::[Entity Puzzle])

--获取Puzzle，根据创建人的uuid获取Puzzle所有内容
selectPuzzleByUserUuid :: String -> IO [Puzzle]
selectPuzzleByUserUuid uuid=
    inBackend $ do
        puzzle<- E.select $ 
                 E.from $ \p->do
                 E.where_ (p ^. PuzzleCreateBy E.==. E.val uuid)
                 return p
        liftIO $ mapM (return . entityVal) (puzzle::[Entity Puzzle])        

--通过email查找用户uuid        
selectUserUuidByUserEmail :: String -> IO [String]
selectUserUuidByUserEmail email=
    inBackend $ do
        user<- E.select $ 
                 E.from $ \u->do
                 E.where_ (u ^. UserEmail E.==. E.val email)
                 return u
        liftIO $ mapM (return . userUuid . entityVal) (user::[Entity User])
        

--通过email查找用户       
selectUserByUserEmail :: String -> IO [User]
selectUserByUserEmail email=
    inBackend $ do
        user<- E.select $ 
                 E.from $ \u->do
                 E.where_ (u ^. UserEmail E.==. E.val email)
                 return u
        liftIO $ mapM (return . entityVal) (user :: [Entity User])        
-- todo
deleteSolutionByUUID::String->IO ()
deleteSolutionByUUID =undefined
-- todo
updateValidation::Solution->IO ()
updateValidation=undefined
{- 
? languages crud
-}
-- todo
insertLanguage::Languages->IO ()
insertLanguage =undefined


insertAllLanguage::IO ()
insertAllLanguage =
    inBackend $ do
        now <- liftIO getCurrentTime
        mapM_ (\x->insert_ $ Languages (snd x) (fst x) Constant.admin now (Just Constant.admin) (Just now) Constant.normalState) Constant.languages

{- 
该方法返回一个列表，包含所有的编程语言：
["Bash","C","C#","C++","Clojure","Dart","F#","Go",
"Groovy","Haskell","Java","Javascript Kotlin","Lua",
"Objective OCaml","Pascal","Perl","PHP",
"Python","Python3 ","Ruby","Rust","Scala","Swift","VB.NET"]
-}
queryAllLanguageWithNormalState::IO [String]
queryAllLanguageWithNormalState = 
    inBackend $ do
        languages<- E.select $ 
                    E.from $ \l->do
                    E.where_ (l ^. LanguagesState E.==. E.val Constant.normalState)
                    return l
        liftIO $ mapM (return . languagesLanguage . entityVal) (languages::[Entity Languages])

-- todo
updateLanguage::Languages->IO ()
updateLanguage=undefined

{- 
* user表的增删改查
* email
*
-}
-- 插入用户
insertUser :: User -> IO ()
insertUser (User _ email pwd _ _ _)= 
    inBackend $ do
        let uuid=unsafePerformIO UV.nextRandom
        --UV.nextRandom:生成随机的uuid
        now <- liftIO getCurrentTime
        --getCurrentTime：从系统时间获取当前utctime
        insert_ $ User (DU.toString uuid) email (getStrictPwd pwd) (Just now) Nothing (Just 0)
    

-- 对密码进行加密
getStrictPwd :: String -> String
getStrictPwd password=
    let
        pwd =Pass $ pack password
        salt = Salt Constant.originalsalt
        hashedPassword=hashPassWithSalt pwd salt
    in
        unpack $ unPassHash hashedPassword

-- getBySpjValue :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User)) getBySpjValue = getByValue $ User SPJ 999
-- 根据uuid查询用户
selectUserByUUID ::String->IO [Entity User]
selectUserByUUID uuid =inBackend . 
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p
-- fixme

-- selectUserByUUID ::String->IO (Maybe User)
-- selectUserByUUID uuid = 
--     inBackend $ do
--         p<- E.select $
--             E.from $ \p -> do
--             E.where_ (p ^. UserUuid E.==. val uuid)
--             return p
--         liftIO $ mapM_ (return . head . entityVal) (p::[Entity User])
-- PersistEntity a => SqlSelect (SqlExpr (Maybe (Entity a))) (Maybe (Entity a))Source#	
-- You may return a possibly-NULL Entity from a select query
-- select via email

-- fixme
selectUserByEmail ::String->IO [Entity User]
selectUserByEmail email =inBackend .
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserEmail E.==. val email)
    return p

-- update email  via uuid
updateUserEmailByUUID :: [Char] -> [Char] -> IO ()
updateUserEmailByUUID uuid email =inBackend .
    E.update $ \p -> do
    E.set p [UserEmail E.=. val email]
    E.where_ (p ^. UserUuid E.==. val uuid) 

--update pwd by uuid
updateUserPwdByUUID :: [Char] -> [Char] -> IO ()
updateUserPwdByUUID uuid pwd =inBackend .
    E.update $ \p -> do 
    E.set p [UserPassword E.=. val pwd]
    E.where_ (p ^. UserUuid E.==. val uuid)

-- 用户登录
login ::String -> String-> IO [Entity User]
login email pass =
  inBackend . E.select $ E.from $ \p -> do
    E.where_ (p ^. UserEmail E.==. val email &&. p ^. UserPassword E.==. val (getStrictPwd pass))
    return p

-- mysql 数据库连接
conInfo :: ConnectInfo
conInfo = ConnectInfo{ 
      connectHost = Constant.dpip
    , connectPort = Constant.dbport
    , connectUser = Constant.dpuser
    , connectPassword = Constant.dppwd
    , connectDatabase = Constant.dbbase
    , connectOptions = []
    , connectPath = ""
    , connectSSL = Nothing
    }


--根据难度级别(category)和条数查询puzzle表
selectPuzzleByCategory :: String -> Int -> IO [Puzzle]
selectPuzzleByCategory category number =
    inBackend $ do
        puzzle<- E.select $ 
                 E.from $ \p->do
                 E.where_ (p ^. PuzzleCategory E.==. E.val category)
                 return p
        fmap (take number) $ liftIO $ mapM (return . entityVal) (puzzle::[Entity Puzzle])

selectUser1 :: IO [User]
selectUser1 =
    inBackend $ do
        user<- E.select $ 
               E.from $ \u->do
               return u
        liftIO $  mapM (return . entityVal) (user :: [Entity User]) 

selectUser2 :: Int ->IO [User]
selectUser2 number =
    inBackend $ do
        user<- E.select $ 
               E.from $ \u->do
               return u
        fmap (take number) $ liftIO $  mapM (return . entityVal) (user :: [Entity User])      


 