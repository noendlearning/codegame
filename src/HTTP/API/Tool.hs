module HTTP.API.Tool where

import ClassyPrelude

getLanguageSetting :: String -> String -> [String]
getLanguageSetting language code
      | language == "python" = ["./static/code/Python3.py", "./static/code", "#!/user/bin/env python\r" ++ code, "python Python3.py"]
      | language == "java"   = ["./static/code/Solution.java", "./static/code", code, "javac Solution.java && java Solution"]
      | language == "haskell"   = ["./static/code/Haskell.hs", "./static/code", code, "runghc Haskell.hs"]
      | otherwise            = ["",""]    

getPath :: String -> [String]
getPath factorPath 
      | factorPath == "1" = ["./static/factor/factor1.txt","./static/answer/answer1.txt"]
      | factorPath == "2" = ["./static/factor/factor2.txt","./static/answer/answer2.txt"]
      | factorPath == "3" = ["./static/factor/factor3.txt","./static/answer/answer3.txt"]
      | factorPath == "4" = ["./static/factor/factor4.txt","./static/answer/answer4.txt"]
      | otherwise         = ["./static/factor/factor5.txt","./static/answer/answer5.txt"]      
    
       