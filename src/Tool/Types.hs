module Tool.Types where

import ClassyPrelude
import Database.Persist.TH

data Star =One | Two | Three | Four | Five
    deriving (Show,Read,Eq)
derivePersistField "Star"    

data PuzzleState=Private | Public | Failure
    deriving (Show,Read,Eq)
derivePersistField "PuzzleState"    

data Category = Major | Second
    deriving (Show,Read,Eq)
derivePersistField "Category"    

data MyState = Normal | Abnormal
    deriving (Show,Read,Eq)
derivePersistField "MyState"    

data PCategory=Easy | Medium | Hard | Professional
    deriving (Show,Read,Eq)
derivePersistField "PCategory"    