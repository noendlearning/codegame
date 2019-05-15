module Tool.Types where

import ClassyPrelude
import Database.Persist.TH

-- data MyState = Employed | Unemployed | Retired
--     deriving (Show, Read, Eq)
-- derivePersistField "Employment"   

data Star =One | Two | Three | Four | Five
    deriving (Show,Read,Eq)
derivePersistField "Star"    

-- data Exp = Fifty | SeventyFive | Hundred
--     deriving (Show,Read,Eq)
-- derivePersistField "Exp" 

data PuzzleState=Private | Public | Failure
    deriving (Show,Read,Eq)
derivePersistField "PuzzleState"    

-- data SolutionState = InUse | NoUse
--     deriving (Show,Read,Eq)
-- derivePersistField "SolutionState"   

-- data LanguagesState=Normal | Abnormal
--     deriving (Show,Read,Eq)
-- derivePersistField "LanguagesState"   

data Category = Major | Second
    deriving (Show,Read,Eq)
derivePersistField "Category"    

data MyState = Normal | Abnormal
    deriving (Show,Read,Eq)
derivePersistField "MyState"    