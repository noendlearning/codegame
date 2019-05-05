{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE PackageImports #-}

module Database
    ( testFunc,selectByUUID,selectByEmail,updateEmailByUUID,updatePwdByUUID,insertUser
    ) where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Reader
import Database.Persist 
import  Database.Persist.MySQL 
import    Database.Persist.TH
-- import  "persistent"         Database.Persist 
-- import "persistent-mysql" Database.Persist.MySQL 
-- import    "persistent-template"       Database.Persist.TH
import           Control.Monad.Logger
import           Database.Esqueleto as E
import qualified  Data.UUID.V4 as UV (nextRandom)
import qualified Data.UUID  as DU (toString,UUID)
import Debug.Trace
import Control.Monad.Trans.Reader
import Database.Persist.Sql.Types.Internal
import Control.Monad.IO.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    uuid String
    email String
    password String
    deriving Show
|]

testFunc :: IO ()
testFunc = do
    uuid1 <- UV.nextRandom
    -- runStdoutLoggingT . withMySQLPool conInfo 10 . runSqlPool $ do
    runNoLoggingT . withMySQLPool conInfo 10 . runSqlPool $ do
        runMigration migrateAll
        -- johnId <- insert $ User (DU.toString uuid1) "123@qq.com" "123"
        -- johnId <- insert $ User (DU.toString uuid2) "234@qq.com" "234"
        -- johnId <- insert $ User (DU.toString uuid3) "345@qq.com" "345"
        -- people <- E.select $ E.from $ \user -> do where_ (user E.^. userUuid E==. "")
        -- traceM(show(johnId))
        -- people <- E.select $
        --             E.from $ \p -> do
        --             E.where_ (p ^. UserUuid E.==. val "fe95c189-5720-49d6-9059-8c94dd373fd2")
        --             return p
        -- updateEmailByUUID "fe95c189-5720-49d6-9059-8c94dd373fd2" "402635876@qq.com"
        -- updatePwdByUUID "fe95c189-5720-49d6-9059-8c94dd373fd2" "5mayiwen"
        -- people<-selectByUUID "fe95c189-5720-49d6-9059-8c94dd373fd2"
        -- traceM(show(people))  
        -- [Entity {entityKey = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 7}}, 
        -- entityVal = User {userUuid = "fe95c189-5720-49d6-9059-8c94dd373fd2", userEmail = "123@qq.com", userPassword = "123"}}]          
        -- liftIO $ mapM_ (putStrLn . userEmail . entityVal) people
        insertUser "godev@gmail.com" "jbandmyw" uuid1
        people <- E.select $ E.from $ \user -> return user
        liftIO $ mapM_ (putStrLn . userEmail . entityVal) people
-- 插入用户
insertUser:: (MonadIO m,PersistStoreWrite backend,BaseBackend backend ~ SqlBackend) =>String-> String-> DU.UUID -> ReaderT backend m ()
insertUser email pwd uuid=
    insert_ $ User (DU.toString uuid) email pwd

-- 根据uuid查询用户
selectByUUID :: (MonadIO m,BackendCompatible SqlBackend backend,PersistQueryRead  backend,PersistUniqueRead  backend) =>[Char]-> ReaderT backend m [Entity User]
selectByUUID uuid=  E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p

-- select via email
selectByEmail :: (MonadIO m,BackendCompatible SqlBackend backend,PersistQueryRead  backend,PersistUniqueRead  backend) =>[Char]-> ReaderT backend m [Entity User]
selectByEmail email=
        E.select $
        E.from $ \p -> do
        E.where_ (p ^. UserEmail E.==. val email)
        return p


-- update email  via uuid
updateEmailByUUID :: (MonadIO m,BackendCompatible SqlBackend backend,PersistQueryWrite backend,PersistUniqueWrite backend) =>[Char] -> [Char] -> ReaderT backend m ()
updateEmailByUUID uuid email=
        E.update $ \p -> do
        E.set p [ UserEmail E.=. val email ]
        E.where_ (p ^. UserUuid E.==. val uuid)

--update pwd by uuid
updatePwdByUUID :: (MonadIO m,BackendCompatible SqlBackend backend,PersistQueryWrite backend,PersistUniqueWrite backend) =>[Char] -> [Char] -> ReaderT backend m ()
updatePwdByUUID uuid pwd=
    E.update $ \p -> do
    E.set p [ UserPassword E.=. val pwd ]
    E.where_ (p ^. UserUuid E.==. val uuid)

-- mysql 数据库连接
conInfo :: ConnectInfo
conInfo =ConnectInfo {
    connectHost="localhost",
    connectPort=3306,
    connectUser="root",
    connectPassword="1",
    connectDatabase="test",
    connectOptions=[],
    connectPath="",
    connectSSL=Nothing
}