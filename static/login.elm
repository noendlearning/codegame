module Main exposing (main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { loadState : StateModel
    , login : Login
    , singup : Singup
    }
type alias Login =
    { email : String
    , password : String
    }
type alias Singup =
    { email : String
    , password : String
    }
init : () -> (Model, Cmd Msg)
init _ =
        ({loadState = StateModel
        , login =
          { email = ""
          , password = ""
          }
        , singup =
          { email = ""
          , password = ""
          }
        }
        , Http.post
          { url = ""
          , expect = Http.expectString GotText
          }
        )



type StateModel
  = Fail
  | Loading
  | Success

type Msg
    = GotText (Result Http.Error String)
    | LoginSubmit Login                              --登录提交
    | SingupSubmit Singup                           --注册提交


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
          case result of
            Ok fullText ->

            Err _ ->
              (Fail,Cmd.none)

        LoginSubmit login ->
            case login of
                Ok fullText ->
                  if model.login.password == /^([a-zA-Z0-9_-])+@([a-zA-Z0-9_-])+(.[a-zA-Z0-9_-])+/ then


                  else




view : Model -> Html Msg
view model =
  case model.loadState of
    Loading ->
    div
        [ class "all" ]
        [ div
            [ class "logo" ]
            [ button
                [ id "btn2" ]
                [ text "SING UP" ]
            , button
                [ id "btn1" ]
                [ text "LOG IN" ]
            , div
                [ class "word1" ]
                [ a
                    [ href "#" ]
                    [ text "COMPNIES" ]
                ]
            , div
                [ class "icon" ]
                [ img
                    [ src "static/images/icon.png" ]
                    []
                ]
            ]
        , div
            [ id "LoginBox" ]
            [ div
                [ class "row1" ]
                [ span
                    []
                    [ text "Sign in to an existing CodinGame account" ]
                , a
                    [ href "#", title "关闭窗口", class "close_btn", id "closeBtn" ]
                    [ text "×" ]
                ]
            , div
                [ class "row" ]
                [ span
                    []
                    [ text "Email:" ]
                , span
                    [ class "inputBox" ]
                    [ input
                        [ id "txtName", placeholder "Email" ]
                        []
                    ]
                , a
                    [ href "javascript:void(0)", title "提示", class "warning", id "warn" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ span
                    []
                    [ text "Password:" ]
                , span
                    [ class "inputBox" ]
                    [ input
                        [ id "txtPwd", placeholder "Password" ]
                        []
                    ]
                , a
                    [ href "javascript:void(0)", title "提示", class "warning", id "warn2" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ a
                    [ href "#", id "loginbtn", onClick LoginSubmit ]
                    [ text "LOG IN" ]
                ]
            ]
        , div
            [ id "SingupBox" ]
            [ div
                [ class "row1" ]
                [ span
                    []
                    [ text "Sign up and start playing, for free" ]
                , a
                    [ href "#", title "关闭窗口", class "close_btn", id "closeBtn1" ]
                    [ text "×" ]
                ]
            , div
                [ class "row" ]
                [ span
                    []
                    [ text "Email:" ]
                , span
                    [ class "inputBox" ]
                    [ input
                        [ id "txtName1", placeholder "Email" ]
                        []
                    ]
                , a
                    [ href "javascript:void(0)", title "提示", class "warning", id "warn3" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ span
                    []
                    [ text "Password:" ]
                , span
                    [ class "inputBox" ]
                    [ input
                        [ id "txtPwd", placeholder "Password" ]
                        []
                    ]
                , a
                    [ href "javascript:void(0)", title "提示", class "warning", id "warn4" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ a
                    [ href "#", id "singupbtn", onClick SingupSubmit ]
                    [ text "SING UP" ]
                ]
            ]
        , div
            [ class "banner" ]
            []
        , div
            [ class "bottom" ]
            []
        ]
