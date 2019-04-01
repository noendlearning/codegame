-- module Name exposing (Model, Msg, update, view, subscriptions, init)


module Main exposing (Model, Msg(..), StateModel(..), init, jsonReq, main, outputDecoder, subscriptions, update, view)

import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--提交代码请求服务器返回的状态
--页面初始化状态
--服务器返回json数据在页面进行解析的状态


type StateModel
    = Fail
    | Success
    | Loading


type alias Model =
    { loadState : StateModel
    , --页面初始化
      code :String
    , --代码
      codeOutput : CodeOutput
    , --代码解析结果
      parseJson : StateModel
    , --json解析状态
      jsonReqState : StateModel -- 后台代码返回状态
    , errMessage : String
    , codeState : StateModel
    }


type Msg
    = GotText (Result Http.Error String)
    | ChangeCode String --输入代码
    | RenderOutput (Result Http.Error String) --代码运行结果填充页面
    | SubmitCode -- 提交代码


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    case Decode.decodeString codeDecoder fullText of
                        Ok codes ->
                          Debug.log "ok get code" ( { model | code = codes,codeState=Success,loadState=Success}, Cmd.none )

                        Err _ ->
                           Debug.log "err" ( { model | codeState = Fail,loadState=Success }, Cmd.none )

                Err _ ->
                  Debug.log "fail"  ( { model | loadState = Fail }, Cmd.none )

        ChangeCode str ->
            ( { model | code = str }, Cmd.none )

        SubmitCode ->
            ( model, jsonReq model )

        RenderOutput result ->
            --渲染代码运行的结果
            case result of
                Ok fullText ->
                    --服务器成功返回数据
                    case Decode.decodeString outputDecoder fullText of
                        Ok output ->
                            Debug.log "output" ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success }, Cmd.none )

                        Err _ ->
                            ( { model | parseJson = Fail, jsonReqState = Success }, Cmd.none )

                Err _ ->
                    --服务器返回失败
                    ( { model | jsonReqState = Fail }, Cmd.none )
type alias Code =String

type alias Codes = List Code

codeDecoder : Decoder Code
codeDecoder =
    Decode.field "codeList"  string

type alias CodeOutput =
    { output : String
    , errMessage : String
    }


outputDecoder : Decoder CodeOutput
outputDecoder =
    Decode.map2 CodeOutput
        (Decode.field "output" string)
        (Decode.field "errMessage" string)


jsonReq : Model -> Cmd Msg
jsonReq model =
    Http.post
        { url = "/linux"

        --todo post 请求携带参数
        , body =
            multipartBody
                [ stringPart "code" model.code
                , stringPart "language" "python"
                ]
        , expect = Http.expectString RenderOutput
        }


view : Model -> Html Msg
view model =
    case model.loadState of
        Loading ->
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6" ] [ text "output:" ]
                    , div [ class "col-md-6" ]
                        [ textarea [] [ text "loading..." ]
                        , -- todo >> 代码
                          button [] [ text "submit" ]
                        ]
                    ]
                ]

        Success ->
            Debug.log "success"
                div
                [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6" ]
                        [ pre [ class ".pre-scrollable" ]
                            [ if model.parseJson == Fail then
                                text "解析失败"

                              else
                                text model.codeOutput.output
                            ]
                        , pre []
                            [ text model.codeOutput.errMessage
                            ]
                        ]
                    , div [ class "col-md-6" ]
                        [ div [ class "row" ]
                            [ textarea [ class "col-md-12", onInput ChangeCode ] [ text model.code ]
                            ]
                        , div [ class "row" ] [ button [ class "btn btn-info col-md-12", onClick SubmitCode ] [ text "submit" ] ]
                        ]
                    ]
                ]

        Fail ->
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] [ text "网络加载失败，请稍后重试...." ]
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loadState = Loading
      , code = ""
      , codeOutput =
            { output = ""
            , errMessage = ""
            }
      , parseJson = Loading
      , jsonReqState = Loading
      , errMessage = ""
      , codeState = Loading
      }
    , --Cmd.none
      initCode
    )

initCode :Cmd Msg
initCode  =
    Http.post
        { url = "/init"
        --todo post 请求携带参数
        , body =
            multipartBody
                [ stringPart "code" "python.py" 
                ]
        , expect = Http.expectString GotText
        }
