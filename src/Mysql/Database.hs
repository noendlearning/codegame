
{-# LANGUAGE DeriveGeneric #-}

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
import Tool.Types
import GHC.Generics
import Data.Aeson


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
    state String
    deriving Show Generic
Puzzle
    uuid String
    UniquePuzzleUuid uuid
    title String
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    createBy String
    updateBy String Maybe
    updateTime UTCTime Maybe default=CURRENT_TIMESTAMP
    inputDescription String
    outputDescription String
    constraints String
    category PCategory
    star Star
    -- exp Exp
    picture String
    state PuzzleState
    deriving Show Generic
Solution
    uuid String
    UniqueSolutionUuid uuid   --Unique：唯一
    language String
    code String
    puzzleId String
    updateTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateBy String Maybe
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    createBy String
    unsolve String
    state String
    deriving Show Generic
Languages
    uuid String
    UniqueLanguagesUuid uuid
    UniqueLanguagesLanguage language
    language String
    createBy String
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateBy String Maybe
    updateTime UTCTime Maybe default=CURRENT_TIMESTAMP
    state String
    deriving Show Generic
Validation
    uuid String
    UniqueValidationUuid uuid
    puzzleId String
    input String
    output String
    category Category
    orders Int
    createBy String
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateBy String Maybe
    updateTime UTCTime Maybe default=CURRENT_TIMESTAMP
    state String
    title String
    deriving Show Generic
Code
    uuid String
    UniqueCodeUuid uuid
    userId String
    puzzleId String
    languagesId String
    userCode String
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateTime UTCTime Maybe default=CURRENT_TIMESTAMP
    deriving Show Generic
|]

instance ToJSON Puzzle
instance FromJSON  Puzzle

instance ToJSON User
instance FromJSON  User

instance ToJSON Solution
instance FromJSON  Solution

instance ToJSON Languages
instance FromJSON  Languages

instance ToJSON Validation
instance FromJSON  Validation

instance ToJSON Code
instance FromJSON  Code

-- 共用mysql数据库连接信息
inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStderrLoggingT $ withMySQLPool conInfo 5 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        action



--对code表的操作

insertCode :: Code -> IO ()
insertCode (Code _ userId puzzleId languagesId userCode _ _) =
    inBackend  $ do
        let uuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        do
            insert_ $ Code ( DU.toString uuid ) userId puzzleId languagesId userCode  (Just now)  Nothing


updateCode :: Code -> IO ()
updateCode (Code _ userId puzzleId languagesId userCode _ _)  = do
    now <- liftIO getCurrentTime
    inBackend .
        E.update $ \p -> do
        E.set p [CodeUserCode E.=. val userCode, CodeUpdateTime E.=. val (Just now)]
        E.where_ (p ^. CodeUserId E.==. val userId &&. p ^. CodePuzzleId E.==. val puzzleId &&. p ^. CodeLanguagesId E.==. val languagesId)




{-
? puzzle crud
-}
--向Puzzle插入数据 涉及 Puzzle  Solution  Validation 三张表
insertPuzzle ::Puzzle->Solution->Validation->IO ()
insertPuzzle (Puzzle _ title  _ createBy  updateBy _  inputDescription outputDescription constraints categor star picture pstate) (Solution _ language code _ _  update _ create unsolve  sstate)(Validation _ _ input output category orders  createB  _  updateB  _  vstate vtitle)=
    inBackend  $ do
        let puuid=unsafePerformIO UV.nextRandom
            suuid=unsafePerformIO UV.nextRandom
            vuuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        insert_ $ Puzzle  (DU.toString puuid)   title   (Just now)   createBy  updateBy  (Just now)  inputDescription  outputDescription  constraints    categor    star  picture  pstate
        insert_ $ Solution ( DU.toString suuid ) language code (DU.toString puuid)  (Just now)  update   (Just now)  create  unsolve   sstate
        insert_ $ Validation (DU.toString vuuid)  (DU.toString puuid) input output category orders createB  (Just now)  updateB  (Just now)  vstate vtitle


selectPuzzleAll :: String -> IO ([Solution], [Puzzle], [Validation])
selectPuzzleAll uuid = do
    solution <- selectSolutionByPuzzleUuid uuid
    puzzle <- selectPuzzleByUUID  uuid
    validation <- selectValidationByUUID  uuid
    return (solution,puzzle,validation)

selectPuzzleAll' :: String->String -> IO ([Solution], [Puzzle], [Validation])
selectPuzzleAll' uuid languageId = do
    solution <- selectSolutionByUUID uuid languageId
    puzzle <- selectPuzzleByUUID  uuid
    validation <- selectValidationByUUID  uuid
    return (solution,puzzle,validation)


--通过Puzzle表的uuid 查询Solution表内容
selectSolutionByPuzzleUuid :: String -> IO[Solution]
selectSolutionByPuzzleUuid uuid =
    inBackend $ do
        solution <- E.select $
                    E.from $ \s -> do
                    E.where_ (s ^. SolutionPuzzleId E.==. E.val uuid)
                    return s
        liftIO $ mapM (return . entityVal) (solution :: [Entity Solution])


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
--根据题目的ID和题目的序号查询Validateion表中的输入参数和正确答案
selectValidationByPuzzleId :: String -> Int -> IO [Validation]
selectValidationByPuzzleId uuid orders =
    inBackend  $ do
        valida <-   E.select $
                    E.from $ \p -> do
                    E.where_ (p ^. ValidationPuzzleId E.==. E.val uuid &&. p ^. ValidationOrders E.==. E.val orders )
                    return p
        liftIO $ mapM (return .entityVal) (valida :: [Entity Validation])


--通过Puzzle表的uuid 和 language表的uuid 查询Solution表内容
selectSolutionByUUID :: String -> String -> IO [Solution]
selectSolutionByUUID puzzleId languageId=
    inBackend $ do
        solution  <- E.select $
                     E.from $ \p -> do
                     E.where_ (p ^. SolutionPuzzleId E.==. E.val puzzleId &&. p ^. SolutionLanguage E.==. E.val languageId)
                     return p
        liftIO $ mapM (return .entityVal) (solution :: [Entity Solution])

{-
? solution crud
-}
insertSolutionWithPuzzleId::Solution->IO()
insertSolutionWithPuzzleId (Solution _ language code puzzleId _ updateBy _ createBy unsolve _) =
    inBackend  $ do
        let uuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        do
            insert_ $ Solution ( DU.toString uuid ) language  code  puzzleId  (Just now)  Nothing   (Just now)  createBy  unsolve   "Normal"
--          insert_ $ Solution uuid  language   code   puzzleId   updateTime   updateBy   createTime   createBy   unsolve    state


--根据UserUUID获取 Soultion表中puzzleId
getPuzzleId :: String -> IO [String]
getPuzzleId uuid =
    inBackend $ do
        puzzleId <- E.select $
                    E.from $ \p  -> do
                    E.where_ ( p^. PuzzleCreateBy E.==. E.val uuid)
                    return p
        liftIO $ mapM (return . puzzleUuid . entityVal) (puzzleId :: [Entity Puzzle] )


--根据State获取language
getLanguage :: String -> IO [String]
getLanguage state =
    inBackend $ do
        language <- E.select $
                    E.from $ \l -> do
                    E.where_ (l ^. LanguagesState E.==. E.val "Normal")
                    return l
        liftIO $ mapM (return . languagesUuid . entityVal)  (language :: [Entity Languages] )

{-
? validation crud
-}
insertValidationWithPuzzleId::Validation -> IO()
insertValidationWithPuzzleId (Validation _ puzzleid input output category orders createBy _ updateBy _ state title) =
    inBackend $ do
        let uuid=unsafePerformIO UV.nextRandom
        now <- liftIO getCurrentTime
        --getCurrentTime：从系统时间获取当前utctime
        insert_ $ Validation (DU.toString uuid) puzzleid input output category orders createBy  (Just now)  updateBy  (Just now)  state title
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
        user<-   E.select $
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

{-
? languages crud
-}
insertAllLanguage::IO ()
insertAllLanguage =
    inBackend $ do
        now <- liftIO getCurrentTime
        mapM_ (\x->insert_ $ Languages (snd x) (fst x) Constant.admin (Just now) (Just Constant.admin) (Just now) "Normal") Constant.languages

{-
该方法返回一个列表，包含所有的编程语言：
["Bash","C","C#","C++","Clojure","Dart","F#","Go",
"Groovy","Haskell","Java","Javascript Kotlin","Lua",
"Objective OCaml","Pascal","Perl","PHP",
"Python","Python3 ","Ruby","Rust","Scala","Swift","VB.NET"]
-}
queryAllLanguageWithNormalState::IO [Languages]
queryAllLanguageWithNormalState =
    inBackend $ do
        languages<- E.select $
                    E.from $ \l->do
                    E.where_ (l ^. LanguagesState E.==. E.val "Normal")
                    return l
        liftIO $ mapM (return . entityVal) (languages::[Entity Languages])


--通过language查询Language表的uuid
selectLanguagesUuidByLanguage :: String -> IO [String]
selectLanguagesUuidByLanguage languageId =
    inBackend $ do
        uuid <-  E.select $
                 E.from $ \l -> do
                 E.where_ (l ^. LanguagesUuid  E.==. E.val languageId)
                 return l
        liftIO $ mapM (return . languagesLanguage . entityVal) (uuid :: [Entity Languages])



{-
* user表的增删改查
* email
*
-}
-- 插入用户
insertUser :: User -> IO ()
insertUser (User _ email pwd _ _ _)=
    inBackend $ do
        let uuid=DU.toString . unsafePerformIO $ UV.nextRandom
        now <- liftIO getCurrentTime
        insert_ $ User uuid email pwd (Just now) Nothing "Normal"

-- 对密码进行加密
-- getStrictPwd :: String -> String
-- getStrictPwd password=
--     let
--         pwd =Pass $ pack password
--         salt = Salt Constant.originalsalt
--         hashedPassword=hashPassWithSalt pwd salt
--     in
--         unpack $ unPassHash hashedPassword

-- getBySpjValue :: MonadIO m => ReaderT SqlBackend m (Maybe (Entity User)) getBySpjValue = getByValue $ User SPJ 999
-- 根据uuid查询用户
selectUserByUUID ::String->IO [Entity User]
selectUserByUUID uuid =inBackend .
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p

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
    E.where_ (p ^. UserEmail E.==. val email &&. p ^. UserPassword E.==. val pass)
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
selectPuzzleByCategory :: PCategory -> Int64 -> IO [Puzzle]
selectPuzzleByCategory category number =
    inBackend $ do
        puzzle<- E.select $
                 E.from $ \p->do
                 E.where_ (p ^. PuzzleCategory E.==. E.val category &&. p ^. PuzzleState E.==. E.val Public)
                 E.limit number
                 return p
        liftIO $ mapM (return . entityVal) (puzzle::[Entity Puzzle])


{-
根据状态查询相应的puzzle
-}
selectPuzzleByState :: PuzzleState->IO [Puzzle]
selectPuzzleByState sta=
    inBackend $ do
        traceM(show("11111111111"))
        puzzle<- E.select $
                 E.from $ \p->do
                 E.where_ (p ^. PuzzleState E.==. E.val sta)
                --  E.limit number
                 return p
        traceM(show("222222222222"))
        traceM(show(puzzle))
        liftIO $ mapM (return . entityVal) (puzzle::[Entity Puzzle])

--上面方法的第二种实现，不过上面number为0时，会显示所有。这个则是一个都不显示
-- selectPuzzleByCategory :: PCategory -> Int -> IO [Puzzle]
-- selectPuzzleByCategory category number =
--     inBackend $ do
--         puzzle<- E.select $
--                  E.from $ \p->do
--                  E.where_ (p ^. PuzzleCategory E.==. E.val category)
--                  return p
--         fmap (take number) $ liftIO $ mapM (return . entityVal) (puzzle::[Entity Puzzle])

--查询user表所有内容
selectUser :: Int ->IO [User]
selectUser number=
    inBackend $ do
        user<- E.select $
               E.from $ \u->do
               return u
        liftIO $  mapM (return . entityVal) (user :: [Entity User])

--查询Validation表中所有内容
selectValidation :: IO [Validation]
selectValidation  =
    inBackend $ do
        validation<- E.select $
               E.from $ \v->do
               return v
        liftIO $  mapM (return . entityVal) (validation :: [Entity Validation])


--接受puzzle validation solution表的对象，来进行更新三张表（通过puzziduuid进行更新）
updatePuzzleAllByPuzzleUuid :: Puzzle -> Validation -> Solution -> IO()
updatePuzzleAllByPuzzleUuid (Puzzle uuid title _ _ updateByP _ inputDescription outputDescription constraints categoryP star picture stateP) (Validation _ puzzleIdV input output categoryV orders _ _ updateByV _ stateV tit) (Solution _ language code puzzleIdS _ updateByS _ _ unsolve stateS) =
                inBackend $ do
                        updateTime <- liftIO getCurrentTime
                        E.update $ \p -> do
                            E.set p  [PuzzleTitle E.=.E.val title , PuzzleUpdateBy E.=.E.val updateByP , PuzzleUpdateTime E.=. E.just(E.val updateTime) , PuzzleInputDescription E.=.E.val inputDescription ,
                                      PuzzleOutputDescription E.=.E.val outputDescription , PuzzleConstraints E.=.E.val constraints , PuzzleCategory E.=.E.val categoryP , PuzzleStar E.=.E.val star ,
                                      PuzzlePicture E.=.E.val picture , PuzzleState E.=.E.val stateP ]
                            E.where_ (p ^. PuzzleUuid E.==. val uuid)
                        E.update $ \p -> do
                            E.set p  [ValidationInput E.=.E.val input , ValidationOutput E.=.E.val output , ValidationCategory E.=.E.val categoryV ,
                                        ValidationOrders E.=.E.val orders , ValidationUpdateBy E.=.E.val updateByV , ValidationUpdateTime E.=. E.just(E.val updateTime) ,
                                        ValidationState E.=.E.val stateV ,ValidationTitle E.=.E.val tit]
                            E.where_ (p ^. ValidationPuzzleId E.==. val puzzleIdV)
                        E.update $ \p -> do
                            E.set p  [SolutionLanguage E.=.E.val language , SolutionCode E.=.E.val code , SolutionUpdateTime E.=. E.just(E.val updateTime),
                                        SolutionUpdateBy E.=.E.val updateByS , SolutionUnsolve E.=.E.val unsolve , SolutionState E.=. E.val stateS ]
                            E.where_ (p ^. SolutionPuzzleId E.==. val puzzleIdS)


--根据email删除user表
deleteUser :: String -> IO()
deleteUser email = inBackend .
        E.delete $
        E.from $ \p -> do
        E.where_ (p ^. UserEmail E.==. E.val email)