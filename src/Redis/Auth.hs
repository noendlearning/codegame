module Redis.Auth where
  
import ClassyPrelude
import qualified Database.Redis as R
import qualified Control.Exception.Safe as S
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.UUID  as DU (UUID, toString)
import qualified Data.UUID.V4 as UV (nextRandom)

type Redis m = (MonadIO m, S.MonadThrow m)
type UserId = String
type SessionId = String
    
redConn :: IO R.Connection
redConn =  case R.parseConnectInfo "redis://localhost:6379/0" of
  Left _ ->
    throwString $ "Redis连接的URL无效:"
  Right connInfo -> do
    R.checkedConnect connInfo
withConn :: Redis  m => R.Redis a -> m a
withConn action = do
  liftIO $ R.runRedis (unsafePerformIO redConn) action

newSession :: Redis  m => UserId -> m SessionId
newSession userId = do
  --生成UUID
  uuid <- liftIO $ UV.nextRandom
  let sId  = DU.toString uuid
  result <- withConn $ R.set (fromString sId) (fromString userId) 
  case result of
    Right R.Ok -> return $ unpack sId
    err -> S.throwString $ "意外的redis错误: " <> show err

findUserIdBySessionId :: Redis m => SessionId -> m (Maybe UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (fromString sId)
  traceM(show(result))
  return $ case result of
    Right (Just uIdStr) -> readMay . unpack . decodeUtf8 $ uIdStr
    err -> S.throwString $ "意外的redis错误: " <> show err