{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database
  ( 
    selectByUUID
  , selectByEmail
  , updateEmailByUUID
  , updatePwdByUUID
  , insertUser
  ) where

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
import           Debug.Trace
import Data.Text (unpack,pack)
import Data.Password.Instances
import Data.Text.Internal
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Time
import Constant 

share  
  [mkPersist sqlSettings, mkMigrate "migrateAll"] 
  [persistLowerCase|
User
    uuid String
    email String
    password String
    createTime UTCTime default=CURRENT_TIMESTAMP
    updateTime UTCTime Maybe
    deriving Show
Puzzle
    uuid String
    title String
    createTime UTCTime default=CURRENT_TIMESTAMP
    createBy String 
    updateBy String
    updateTime UTCTime Maybe
    inputDescription String
    outputDescription String
    constraints String
    deriving Show
Solution
    uuid String
    language String
    code String
    puzzleId String
    updateTime UTCTime Maybe
    updateBy String
    createTime UTCTime default=CURRENT_TIMESTAMP
    createBy String
    unsolve String
    deriving Show
Languages
    uuid String
    langeuage String
    createBy String
    createTime UTCTime default=CURRENT_TIMESTAMP
    updateBy String
    updateTime UTCTime Maybe
    deriving Show
Validation
    uuid String
    puzzleId String
    input String
    output String 
    category Int 
    orders Int 
    deriving Show
|]

-- 共用mysql数据库连接信息
inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStderrLoggingT $ withMySQLPool conInfo 5 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

{- 
* user表的增删改查
-}
-- 插入用户
insertUser :: User -> IO ()
insertUser (User _ email pwd _ _)= 
  inBackend $ do
    -- uuid<-UV.nextRandom
    let uuid=unsafePerformIO UV.nextRandom
    now <- liftIO getCurrentTime
    insert_ $ User (DU.toString uuid) email (getStrictPwd pwd) now Nothing
    -- comment <- get commentId
    -- liftIO $ print comment
    -- let uuid=unsafePerformIO UV.nextRandom
    -- in
    --   inBackend . insert_ $ User (DU.toString uuid) email $ getStrictPwd pwd
    -- return ()
-- originalsalt = "hnbrina2019XN9dUU8uhnbrina2019bQSkvEZIRhnbrina2019UWr9UVWCjzOLsU=hnbrina2019LbmItlhltyIHhnbrina20194Nro2YyMFeCCKwtV0=hnbrina2019"

-- 对密码进行加密
getStrictPwd :: String -> String
getStrictPwd password=
        let
            pwd =Pass $ pack password
            salt = Salt originalsalt
            hashedPassword=hashPassWithSalt pwd salt
        in
            unpack $ unPassHash hashedPassword

-- 根据uuid查询用户
selectByUUID ::String->IO [Entity User]
selectByUUID uuid =inBackend . 
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p

-- select via email
selectByEmail ::String->IO [Entity User]
selectByEmail email =inBackend .
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserEmail E.==. val email)
    return p

-- update email  via uuid
updateEmailByUUID :: [Char] -> [Char] -> IO ()
updateEmailByUUID uuid email =inBackend .
    E.update $ \p -> do
    E.set p [UserEmail E.=. val email]
    E.where_ (p ^. UserUuid E.==. val uuid)

--update pwd by uuid
updatePwdByUUID :: [Char] -> [Char] -> IO ()
updatePwdByUUID uuid pwd =inBackend .
    E.update $ \p -> do 
    E.set p [UserPassword E.=. val pwd]
    E.where_ (p ^. UserUuid E.==. val uuid)

-- mysql 数据库连接
conInfo :: ConnectInfo
conInfo = ConnectInfo{ 
      connectHost = dpip
    , connectPort = dbport
    , connectUser = dpuser
    , connectPassword = dppwd
    , connectDatabase = dbbase
    , connectOptions = []
    , connectPath = ""
    , connectSSL = Nothing
    }
