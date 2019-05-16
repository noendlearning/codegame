module HTTP.API.Tool where

import ClassyPrelude

getLanguageSetting :: String -> String -> String -> [String]
getLanguageSetting language code folder
      | language == "python" = [folder ++ "/Python3.py",  "#!/user/bin/env python\r" ++ code, "python Python3.py"]
      | language == "java"   = [folder ++ "/Solution.java", code, "javac Solution.java && java Solution"]
      | language == "haskell"   = [folder ++ "/Haskell.hs", code, "runghc Haskell.hs"]
      | otherwise            = ["",""]    

    
       