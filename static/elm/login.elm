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
    , emailState : Int
    , pwdState : Int
    }
-- 0未填写 默认
-- 1 填写正确
-- 2 填写错误

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
          ,emailState=0
          ,pwdState=0
        }
        ,
        Cmd.none
        --  Http.post
        --   { url = "/login"
        --   , expect = Http.expectString GotText
        --   }
        )



type StateModel
  = Fail
  | Loading
  | Success

type Msg
    = GotText (Result Http.Error String)
    | LoginSubmit                              --登录提交
    | SingupSubmit Singup                           --注册提交
    | CheckEmail String --验证邮箱
    | CheckPwd String --验证密码


loginUser : String -> String -> Cmd Msg
loginUser email pwd = 
    Http.post
        {
            url="/loginUser",
            body=
                multipleBody
                    [
                        stringPart "" email,
                        stringPart "" pwd
                    ]
        , expect = Http.f
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
          case result of
            Ok fullText ->

            Err _ ->
              (Fail,Cmd.none)

        LoginSubmit ->
           if Bool.not (String.isEmpty email) && Bool.not (String.isEmpty pwd) then
           -- email && pwd both not empty
                --check correct
                if (checkEmail model.email) && (checkPwd model.pwd) then
                    -- can post req
                    ({model|emailState=1,pwdState=1},)
                else
                     if checkEmail email then
                        ({model|emailState=1},Cmd.none)
                    else
                        ({model|emailState=2},Cmd.none)
                     if checkPwd pwd then
                        ({model|pwdState=1},Cmd.none)
                    else
                        ({model|pwdState=2},Cmd.none)    
           else
           -- check one is empty
            if String.isEmpty email then
                ({model|emailState=0},Cmd.none)
            else
                (model,Cmd.none)
            if String.isEmpty pwd then
               ({model|pwdState=0},Cmd.none)
            else           
                (model,Cmd.none)

        CheckEmail email->
            if String.isEmpty email then
                ({model|emailState=0},Cmd.none)
            else
                if checkEmail email then
                    ({model|emailState=1},Cmd.none)
                else
                    ({model|emailState=2},Cmd.none)
        CheckPwd pwd->
            if String.isEmpty pwd then
                ({model|pwdState=0},Cmd.none)
            else
                if checkPwd pwd then
                    ({model|pwdState=1},Cmd.none)
                else
                    ({model|pwdState=2},Cmd.none)

checkEmail : String -> Bool
checkEmail email=email==/^([a-zA-Z0-9_-])+@([a-zA-Z0-9_-])+(.[a-zA-Z0-9_-])+/

checkPwd : String -> Bool
checkPwd pwd=pwd==/(a-zA-Z0-9){7,}/


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
                    [ src "/static/images/icon.png" ]
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
                    [ href "#", title "提示", class "warning", id "warn" ]
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
                    [ href "#", title "提示", class "warning", id "warn2" ]
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
                    -- TODO check email
                        [ id "txtName1", placeholder "Email",onInput (CheckEmail email) ]
                        []
                    ]
                , a
                    [ href "#", title "提示", class "warning", id "warn3" ]
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
                    -- TODO check password
                        [ id "txtPwd", placeholder "Password" ,onInput (CheckPwd pwd)]
                        []
                    ]
                , a
                    [ href "#", title "提示", class "warning", id "warn4" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ a
                -- TODO check both email and pwd then post 
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
