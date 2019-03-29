-- module Name exposing (Model, Msg, update, view, subscriptions, init)


module Main exposing (Model, Msg(..), StateModel(..), init, initCode, jsonReq, main, outputDecoder, subscriptions, update, view)

import Browser
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
      code : String
    , --代码
      outPut : String
    , --代码解析结果
      parseJson : StateModel
    , --json解析状态
      jsonReqState : StateModel -- 后台代码返回状态
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
                    ( { model | loadState = Success, code = fullText }, Cmd.none )

                Err _ ->
                    ( { model | loadState = Fail }, Cmd.none )

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
                            ( { model | outPut = output, jsonReqState = Success, parseJson = Success }, Cmd.none )

                        Err _ ->
                            ( { model | parseJson = Fail, jsonReqState = Success }, Cmd.none )

                Err _ ->
                    --服务器返回失败
                    ( { model | jsonReqState = Fail }, Cmd.none )


outputDecoder : Decoder String
outputDecoder =
    Decode.field "output" string


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
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6" ]
                        [ pre []
                            [ if model.parseJson == Fail then
                                text "解析失败"

                              else
                                text model.outPut
                            ]
                        ]
                    , div [ class "col-md-6" ]
                        [ textarea [ onInput ChangeCode ] [ text model.code ]
                        , button [ onClick SubmitCode ] [ text "submit" ]
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
      , outPut = ""
      , parseJson = Loading
      , jsonReqState = Loading
      }
    , initCode
    )


initCode =
    Http.get
        { url = "/temp/temp1.txt"
        , expect = Http.expectString GotText
        }
