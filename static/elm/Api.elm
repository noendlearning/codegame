port module Api exposing (..)


import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)

type alias Form =
    { email : String
    , password : String
    }


storeFormWith : Form -> Cmd msg
storeFormWith form =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "password", Encode.string form.password )
                        , ( "email", Encode.string form.email )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg

-- decodeErrors : Http.Error -> List String
-- decodeErrors error =
--     case error of
--         Http.BadStatus response ->
--             response.body
--                 |> decodeString (field "errors" errorsDecoder)
--                 |> Result.withDefault [ "Server error" ]

--         err ->
--             [ "Server error" ]


-- errorsDecoder : Decoder (List String)
-- errorsDecoder =
--     Decode.keyValuePairs (Decode.list Decode.string)
--         |> Decode.map (List.concatMap fromPair)

-- fromPair : ( String, List String ) -> List String
-- fromPair ( field, errors ) =
--     List.map (\error -> field ++ " " ++ error) errors        