module Mysql.Database   where

import ClassyPrelude
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import Database.Persist.Class (PersistField(toPersistValue))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import qualified Data.UUID                           as DU (UUID, toString)
import qualified Data.UUID.V4                        as UV (nextRandom)
import           Database.Esqueleto                  as E
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
    email String
    password String 
    createTime UTCTime Maybe default=CURRENT_TIMESTAMP
    updateTime UTCTime Maybe
    state Int Maybe
    deriving Show
Puzzle
    uuid String
    title String
    createTime UTCTime default=CURRENT_TIMESTAMP
    createBy String 
    updateBy String Maybe
    updateTime UTCTime Maybe
    inputDescription String
    outputDescription String
    constraints String
    state Int 
    deriving Show
Solution
    uuid String
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
    langeuage String
    createBy String
    createTime UTCTime default=CURRENT_TIMESTAMP
    updateBy String Maybe
    updateTime UTCTime Maybe
    state Int 
    deriving Show
Validation
    uuid String
    puzzleId String
    input String
    output String 
    category Int 
    orders Int 
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

selectPuzzleByUUID::String->IO [Entity Puzzle]
selectPuzzleByUUID=undefined

updatePuzzle :: Puzzle->IO ()
updatePuzzle = undefined
{- 
? solution crud
-}
insertSolutionWithPuzzleId::Solution->String->IO()
insertSolutionWithPuzzleId=undefined

updateSolution ::Solution->IO ()
updateSolution=undefined
{- 
? validation crud
-}
insertValidationWithPuzzleId::Validation->String->IO()
insertValidationWithPuzzleId=undefined

deleteSolutionByUUID::String->IO ()
deleteSolutionByUUID=undefined

updateValidation::Solution->IO ()
updateValidation=undefined
{- 
? languages crud
-}
insertLanguage::Languages->IO ()
insertLanguage =undefined


insertAllLanguage::IO ()
insertAllLanguage =
    inBackend $ do
        now <- liftIO getCurrentTime
        mapM_ (\x->insert_ $ Languages (snd x) (fst x) Constant.admin now (Just Constant.admin) (Just now) Constant.normalState) Constant.languages

queryAllLanguage::IO [Entity Languages]
queryAllLanguage = undefined

updateLanguage::Languages->IO ()
updateLanguage=undefined

{- 
* user表的增删改查
-}
-- 插入用户
insertUser :: User -> IO ()
insertUser (User _ email pwd _ _ _)= 
  inBackend $ do
    let uuid=unsafePerformIO UV.nextRandom
    now <- liftIO getCurrentTime
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

-- 根据uuid查询用户
selectUserByUUID ::String->IO [Entity User]
selectUserByUUID uuid =inBackend . 
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p

-- select via email
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
