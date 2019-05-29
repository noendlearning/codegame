module HTTP.API.Tool where

import ClassyPrelude

getLanguageSetting :: String -> String -> String -> [String]
getLanguageSetting language code folder
      | language == "Python3" = [folder ++ "/Python3.py",  "#!/user/bin/env python\r" ++ code, "python Python3.py"]
      | language == "Java"   = [folder ++ "/Solution.java", code, "javac Solution.java && java Solution"]
      | language == "Haskell"   = [folder ++ "/Haskell.hs", code, "runghc Haskell.hs"]
      | otherwise            = ["",""]    

    
       