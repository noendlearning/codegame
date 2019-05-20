{-# LANGUAGE DeriveGeneric #-}

module Tool.Types where

import ClassyPrelude
import Database.Persist.TH
import GHC.Generics
import Data.Aeson

data Star =One | Two | Three | Four | Five
    deriving (Show,Read,Eq,Generic)
derivePersistField "Star"    

instance ToJSON Star 
instance FromJSON Star 

data PuzzleState=Private | Public | Failure
    deriving (Show,Read,Eq,Generic)
derivePersistField "PuzzleState" 

instance ToJSON PuzzleState 
instance FromJSON PuzzleState 

data Category = Major | Second
    deriving (Show,Read,Eq,Generic)
derivePersistField "Category"    

instance ToJSON Category 
instance FromJSON Category   

data MyState = Normal | Abnormal
    deriving (Show,Read,Eq,Generic)
derivePersistField "MyState"    

instance ToJSON MyState 
instance FromJSON MyState   

data PCategory=Easy | Medium | Hard | Professional
    deriving (Show,Read,Eq,Generic)
derivePersistField "PCategory"   

instance ToJSON PCategory 
instance FromJSON PCategory   