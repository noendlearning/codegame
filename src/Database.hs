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
  ( 
    -- testFunc
  -- , 
    selectByUUID
  , selectByEmail
  , updateEmailByUUID
  , updatePwdByUUID
  , insertUser
  -- , getStrictPwd
  ) where

import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import Database.Persist.Class (PersistField(toPersistValue))
import           Control.Monad.IO.Class
-- import  "persistent"         Database.Persist
-- import "persistent-mysql" Database.Persist.MySQL
-- import    "persistent-template"       Database.Persist.TH
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

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    uuid String
    email String
    password String
    deriving Show
|]

-- testFunc :: IO ()
-- testFunc = do
--   -- uuid1 <- UV.nextRandom
--   runNoLoggingT . withMySQLPool conInfo 10 . runSqlPool   $ do
--   -- runNoLoggingT . withMySQLPool conInfo 10 . runSqlPool
--     runMigration migrateAll
    -- do后面的不能取消 返回值不对
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
        -- traceM(show(people))
        -- [Entity {entityKey = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 7}},
        -- entityVal = User {userUuid = "fe95c189-5720-49d6-9059-8c94dd373fd2", userEmail = "123@qq.com", userPassword = "123"}}]
        -- liftIO $ mapM_ (putStrLn . userEmail . entityVal) people
    -- let pwd =Pass "5mayiwen"
    -- let salt = Salt "hnbrina2019"
    -- let hashedPassword=hashPassWithSalt pwd salt
    -- let strictPwd=unpack $ unPassHash hashedPassword
    -- insertUser "godev1" "5mayiwen" uuid1
    -- people <- E.select $ E.from $ \user -> return user
    -- liftIO $ mapM_ (putStrLn . userEmail . entityVal) people

-- 插入用户
insertUser ::
     (MonadIO m, PersistStoreWrite backend, BaseBackend backend ~ SqlBackend)
  => String
  -> String
  -> ReaderT backend m ()
insertUser email pwd= 
  let uuid=unsafePerformIO UV.nextRandom
  in
    insert_ $ User (DU.toString uuid) email $ getStrictPwd pwd
    -- return ()

originalsalt = "hnbrina2019XN9dUU8uhnbrina2019bQSkvEZIRhnbrina2019UWr9UVWCjzOLsU=hnbrina2019LbmItlhltyIHhnbrina20194Nro2YyMFeCCKwtV0=hnbrina2019"

getStrictPwd :: String -> String
getStrictPwd password=
        let
            pwd =Pass $ pack password
            salt = Salt originalsalt
            hashedPassword=hashPassWithSalt pwd salt
        in
            unpack $ unPassHash hashedPassword

-- 根据uuid查询用户
selectByUUID ::( MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend)=> [Char]-> ReaderT backend m [Entity User]
selectByUUID uuid =
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserUuid E.==. val uuid)
    return p

-- select via email
selectByEmail ::( MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend)=> [Char]-> ReaderT backend m [Entity User]
selectByEmail email =
    E.select $
    E.from $ \p -> do
    E.where_ (p ^. UserEmail E.==. val email)
    return p

-- update email  via uuid
updateEmailByUUID ::( MonadIO m, BackendCompatible SqlBackend backend, PersistQueryWrite backend, PersistUniqueWrite backend)=> [Char]-> [Char]-> ReaderT backend m ()
updateEmailByUUID uuid email =
    E.update $ \p -> do
    E.set p [UserEmail E.=. val email]
    E.where_ (p ^. UserUuid E.==. val uuid)

--update pwd by uuid
updatePwdByUUID ::( MonadIO m, BackendCompatible SqlBackend backend, PersistQueryWrite backend, PersistUniqueWrite backend)=> [Char]-> [Char]-> ReaderT backend m ()
updatePwdByUUID uuid pwd =
    E.update $ \p -> do
    E.set p [UserPassword E.=. val pwd]
    E.where_ (p ^. UserUuid E.==. val uuid)

-- mysql 数据库连接
conInfo :: ConnectInfo
conInfo = ConnectInfo{ 
      connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "root"
    , connectPassword = "1"
    , connectDatabase = "test"
    , connectOptions = []
    , connectPath = ""
    , connectSSL = Nothing
    }
