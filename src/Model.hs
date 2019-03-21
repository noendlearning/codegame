{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
-- import Control.Monad.Trans.Resource (ResourceT)
import Data.Text    

-- type RES = ResourceT IO Response
type BSAssoc = [(BS.ByteString, BS.ByteString)]
data CodeOutput = CodeOutput {
      code :: Text
    , output  :: Text
    } deriving (Show)

instance ToJSON CodeOutput where
    -- this generates a Value
    toJSON (CodeOutput code output) =
        object ["code" .= code, "output" .= output]

    -- this encodes directly to a bytestring Builder
    toEncoding (CodeOutput code output) =
        pairs ("code" .= code <> "output" .= output)

instance FromJSON CodeOutput where
    parseJSON = withObject "CodeOutput" $ \v -> CodeOutput
        <$> v .: "code"
        <*> v .: "output"

