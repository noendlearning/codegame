module Api.Endpoint exposing (..)

import Http exposing (..)

type alias Endpoint
    =  String


register : Endpoint
register = "/registUser"

login : Endpoint
login = "/loginUser"
