module Viewer exposing (..) 

import Json.Decode as Decode exposing (Decoder,string)



minPasswordChars : Int
minPasswordChars =
    6


-- 通用json解析
type alias Viewer =
    { output : String
    , errMessage : String
    , message : String
    , found : String
    , expected : String
    }

decoder : Decoder Viewer
decoder =
    Decode.map5 Viewer
        (Decode.field "output" string)
        (Decode.field "errMessage" string)
        (Decode.field "message" string)
        (Decode.field "found" string)
        (Decode.field "expected" string)

