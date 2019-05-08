module HTTP.SetCookie where

import ClassyPrelude
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Data.Time.Lens
--import Control.Exception.Lifted
import qualified Data.Map.Lazy as MAP

type SessionId = String

-- 解析cookie
getCookie ::  ByteString -> String -> IO (Maybe String)
getCookie cookies key  = do
  return $ do
    let cookie = parseCookies cookies
        bsKey = fromString key
    val <- lookup bsKey cookie
    return $ (unpack . decodeUtf8) val

-- 设置cookie
setSessionIdInCookie :: MonadIO m => SessionId -> m ByteString
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  evaluate $ toStrict . toLazyByteString . renderSetCookie $ def{ setCookieName = "sessionId",
                                                                  setCookieMaxAge = Just (86400000000000000*30),
                                                                  setCookieValue = fromString sId , 
                                                                  setCookieExpires = Just $ modL month (+1) curTime}

{- getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> return Nothing
    Just sId -> lift $ resolveSessionId sId -}

{- reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUserId <- getCurrentUserId
  case mayUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId ->
      return userId -}