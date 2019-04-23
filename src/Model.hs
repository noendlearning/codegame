{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
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
    constraints::Text,
    -- gameModes:
    testCases :: [TestCase]

}deriving (Generic,Show)
instance ToJSON PuzzleInput
instance FromJSON PuzzleInput

data TestCase = TestCase{
    testName::String,
    validator::String,
    test::StandCase,
    validater::StandCase
} deriving (Generic,Show)
instance ToJSON TestCase
instance FromJSON TestCase
data StandCase = StandCase{
    input::String,
    oput::String
} deriving (Generic,Show)
instance ToJSON StandCase
instance FromJSON StandCase
{- instance ToJSON CodeOutput where
    -- this generates a Value
    toJSON (CodeOutput code output) =
        object ["code" .= code, "output" .= output]

    -- this encodes directly to a bytestring Builder
    toEncoding (CodeOutput code output) =
        pairs ("code" .= code <> "output" .= output)

instance FromJSON CodeOutput where
    parseJSON = withObject "CodeOutput" $ \v -> CodeOutput
        <$> v .: "code"
        <*> v .: "output" -}

