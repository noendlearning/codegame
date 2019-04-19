module Main exposing (main, view)

-- import Viewer exposing (..)

import Api exposing (..)
import Api.Endpoint as Endpoint exposing (..)
import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Viewer exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { problems : List Problem
    , form : Form
    }


-- type alias Form =
--     { email : String
--     , password : String
--     }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form =
            { email = ""
            , password = ""
            }
      , problems = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , register validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        -- fixme
        CompletedRegister result ->
            case result of
                Ok fullText ->
                    case Decode.decodeString Viewer.decoder fullText of
                        Ok codes ->
                            Debug.log "ok get code"
                                ( model
                                , Api.storeFormWith model.form
                                )

                        Err _ ->
                            Debug.log "err" (model, Cmd.none )

                Err _ ->
                    Debug.log "fail" ( model, Cmd.none )



-- CompletedRegister (Err error) ->
--     -- let
--     --     serverErrors =
--     --         Api.decodeErrors error
--     --             |> List.map ServerError
--     -- in
--     ( model
--     , Cmd.none
--     )
-- CompletedRegister (Ok viewer) ->
--     ( model
--     , Api.storeUserWith viewer
--     )


type TrimmedForm
    = Trimmed Form


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else if String.length form.password < Viewer.minPasswordChars then
                    [ "password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]

                else
                    []


minPasswordChars : Int
minPasswordChars =
    6


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedRegister (Result Http.Error String)



-- | GotSession Session


type ValidatedField
    = Email
    | Password


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]



-- todo test


register : TrimmedForm -> Cmd Msg
register (Trimmed form) =
    let
        body =
            multipartBody
                [ stringPart "email" form.email
                , stringPart "password" form.password
                ]
    in
    Http.post
        { url = Endpoint.register
        , body = body
        , expect = Http.expectString CompletedRegister
        }


view : Model -> Html Msg
view model =
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
                        -- todo
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
                        -- todo
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
                    -- , onClick LoginSubmit FIxme
                    [ href "#", id "loginbtn" ]
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
                        [ id "txtName1"
                        , placeholder "Email"
                        , onInput EnteredEmail
                        , Html.Attributes.value model.form.email
                        ]
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
                        [ id "txtPwd"
                        , placeholder "Password"
                        , onInput EnteredPassword
                        , type_ "password"
                        , Html.Attributes.value model.form.password
                        ]
                        []
                    ]
                , a
                    [ href "#", title "提示", class "warning", id "warn4" ]
                    [ text "*" ]
                ]
            , div
                [ class "row" ]
                [ a
                    [ href "#", id "singupbtn", onClick SubmittedForm ]
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
