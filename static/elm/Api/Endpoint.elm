module Api.Endpoint exposing (Endpoint, register)

import Http exposing (..)

type alias Endpoint
    =  String


register : Endpoint
register = "/registUser"

login : Endpoint
login = "/loginUser"
