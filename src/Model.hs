{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Data.Aeson
import ClassyPrelude
import qualified Data.ByteString.Char8 as BS
-- import Control.Monad.Trans.Resource (ResourceT)
import Data.Text    
import GHC.Generics
-- type RES = ResourceT IO Response
type BSAssoc = [(BS.ByteString, BS.ByteString)]
data CodeOutput = CodeOutput {
      output  :: Text,
      message  :: String,
      found  :: String,
      expected  :: String,
      errMessage  :: Text
    } deriving (Generic,Show)
instance ToJSON CodeOutput 
instance FromJSON CodeOutput
{- 
通用返回給页面的json格式信息
msg：信息
state：与信息一一对应的状态
<10 登录注册的状态
1 邮箱已经被注册
2 注册成功
3 邮箱不存在
4 退出登录成功
5 登录成功
-}
data Output=Output{
    msg::String,
    state::String
}deriving (Generic,Show)
instance ToJSON Output
instance FromJSON Output


-- 保存代码的类型
data CodeList = CodeList {
      codeList :: String
    } deriving (Generic,Show)
instance ToJSON CodeList 
instance FromJSON CodeList 

data PuzzleInput = PuzzleInput{
    title::Text,
    statement :: Text,
    inputDescription::Text,
    outputDescription::Text,
    constraints::Text
}deriving (Generic,Show)
instance ToJSON PuzzleInput
instance FromJSON PuzzleInput 

data TestCase = TestCase{
    testName::String,
    validator::String
} deriving (Generic,Show)
instance ToJSON TestCase
instance FromJSON TestCase
data StandCase = StandCase{
    input::String,
    oput::String
} deriving (Generic,Show)
instance ToJSON StandCase
instance FromJSON StandCase


