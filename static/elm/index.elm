module Main exposing (main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Http exposing (..)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type alias Model =
    { loadState : StateModel
    , form : Form
    -- , singup : Form
    , emailState : Int
    , pwdState : Int
    }

type alias Form =
    { email : String
    , password : String
    }

init : () -> (Model, Cmd Msg)
init _ =
        ({loadState = Loading
        , form =
            { email = ""
            , password = ""
            }
        ,emailState=0
        ,pwdState=0
        }
        ,Cmd.none
        )

type StateModel
  = Fail
  | Loading
  | Success

type Msg
    = LoginSubmit                            --登录提交
    | SingupSubmit                            --注册提交
    | GotText (Result Http.Error String)
    | ChangeEmail String
    | ChangePwd String

loginUser : Form-> Cmd Msg
loginUser login=
    Http.post
        {
            url="/loginUser",
            body=
                multipartBody
                    [
                        stringPart "email" login.email,
                        stringPart "passw" login.password
                    ]
        , expect = Http.expectString GotText
        }

registerUser : Form-> Cmd Msg
registerUser singup=
    Http.post
        {
            url="/registerUser",
            body=
                multipartBody
                    [
                        stringPart "email" singup.email,
                        stringPart "passw" singup.password
                    ]
        , expect = Http.expectString GotText
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginSubmit->
            let login=model.form
            in
                Debug.log "login" (model,loginUser login)
        SingupSubmit->
            let login=model.form
            in
                (model,registerUser login)
        GotText result->
            case result of
                    Ok fullText ->
                        case Decode.decodeString outputDecoder fullText of
                            Ok output ->
                                if output.state=="1" then
                                    (model,Cmd.none)
                                else if output.state=="2" || output.state=="5" then
                                    (model,Nav.load "/practice")
                                else
                                -- fixme: 給model设置错误消息
                                    (model,Cmd.none)
                            Err _ ->
                                    (model,Cmd.none)
                    Err _ ->
                        (model,Cmd.none)

        ChangeEmail str->
            updateForm (\form -> { form | email = str }) model
        ChangePwd str->
            updateForm (\form -> { form | password = str }) model

type alias Output=
    {
        msg:String,
        state:String
    }

outputDecoder : Decoder Output
outputDecoder =
    Decode.map2 Output
        (Decode.field "msg" string)
        (Decode.field "state" string)

-- checkEmail : String -> Bool
-- checkEmail email=email==/^([a-zA-Z0-9_-])+@([a-zA-Z0-9_-])+(.[a-zA-Z0-9_-])+/

-- checkPwd : String -> Bool
-- checkPwd pwd=pwd==/(a-zA-Z0-9){7,}/

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )

view : Model -> Html Msg
view model =
  case model.loadState of
    _ ->
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
                        [ id "txtName", placeholder "Email" ,onInput ChangeEmail]
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
                        [ id "txtPwd", placeholder "Password" ,onInput ChangePwd]
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
                        [ id "txtName1", placeholder "Email" ,onInput ChangeEmail]
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
                        [ id "txtPwd", placeholder "Password",onInput ChangePwd ]
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
