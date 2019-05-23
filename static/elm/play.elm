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

type alias CodeList=
    {
        code : String,
        language : String
    }

type alias Model =
    { loadState : StateModel
    , --页面初始化
      code : String
    , --代码
      codeOutput : CodeOutput
    , --代码解析结果
      parseJson : StateModel
    , --json解析状态
      jsonReqState : StateModel -- 后台代码返回状态
    , errMessage : String
    , codeState : StateModel
    , testIndex : Int
    , language : String
    , batchSubmit : Bool
    }


type Msg
    = GotText (Result Http.Error String)
    | ChangeCode String --输入代码
    | RenderOutput (Result Http.Error String) --代码运行结果填充页面
    | SubmitCode Int -- 提交代码
    | CheckLanguage String --选择语言
    | BatchSubmitCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    case Decode.decodeString codeDecoder fullText of
                        Ok codes ->
                            Debug.log "ok get code" ( { model | code = codes, codeState = Success, loadState = Success }, Cmd.none )

                        Err _ ->
                            Debug.log "err" ( { model | codeState = Fail, loadState = Success }, Cmd.none )

                Err _ ->
                    Debug.log "fail" ( { model | loadState = Fail }, Cmd.none )

        ChangeCode str ->
            ( { model | code= str }, Cmd.none )

        SubmitCode index ->
            ( { model | testIndex = index }, jsonReq index model.code model.language )

        RenderOutput result ->
            --渲染代码运行的结果
            case result of
                Ok fullText ->
                    --服务器成功返回数据
                    case Decode.decodeString outputDecoder fullText of
                        Ok output ->
                            if model.batchSubmit then
                                if model.testIndex<5 then
                                    Debug.log (String.fromInt model.testIndex) ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success,testIndex=model.testIndex+1 }, jsonReq (model.testIndex+1) model.code model.language )

                                else
                                    Debug.log (String.fromInt model.testIndex) ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success,testIndex=5,batchSubmit=False }, jsonReq 5 model.code model.language )

                            else
                                Debug.log "output3" ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success }, Cmd.none )

                        Err _ ->
                            ( { model | parseJson = Fail, jsonReqState = Success }, Cmd.none )

                Err _ ->
                    --服务器返回失败
                    ( { model | jsonReqState = Fail }, Cmd.none )

        CheckLanguage str ->
            ( { model | language = str }, initCode str)
        BatchSubmitCode ->
            ({model|batchSubmit=True,testIndex=1},jsonReq 1 model.code model.language)


type alias Code =
    String


codeDecoder : Decoder Code
codeDecoder =
    Decode.field "codeList" string

type alias CodeOutput =
    { output : String
    , errMessage : String
    , message : String
    , found : String
    , expected : String
    }

outputDecoder : Decoder CodeOutput
outputDecoder =
    Decode.map5 CodeOutput
        (Decode.field "output" string)
        (Decode.field "errMessage" string)
        (Decode.field "message" string)
        (Decode.field "found" string)
        (Decode.field "expected" string)


jsonReq : Int -> String -> String -> Cmd Msg
jsonReq testIndex code language =
    Http.post
        { url = "/play"

        --todo post 请求携带参数
        , body =
            multipartBody
                [ stringPart "code" code
                , stringPart "language" language
                , stringPart "testIndex" (String.fromInt testIndex)
                ]
        , expect = Http.expectString RenderOutput
        }


view : Model -> Html Msg
view model =
    case model.loadState of
        Loading ->
            text "loding"

        Success ->
            div [ class "all" ]
                [ div
                    [ class "nag" ]
                    [ a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/head.png" ]
                            []
                        ]
                    , a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/level.png" ]
                            []
                        ]
                    , a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/btn1.png" ]
                            []
                        ]
                    , a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/btn2.png" ]
                            []
                        ]
                    , a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/btn3.png" ]
                            []
                        ]
                    , a
                        [ href "#" ]
                        [ img
                            [ src "/static/images/btn4.png" ]
                            []
                        ]
                    , div
                        [ class "friends" ]
                        [ a
                            [ href "#" ]
                            [ img
                                [ src "/static/images/friends.png" ]
                                []
                            ]
                        ]
                    ]
                , div
                    [ class "chat" ]
                    []
                , div
                    [ class "mains" ]
                    [ div
                        [ class "banner" ]
                        [ div
                            [ class "word_one" ]
                            [ text "ASCII Art" ]
                        ]
                    , div
                        [ class "containers" ]
                        [ div
                            [ class "left" ]
                            [ div
                                [ class "discription" ]
                                []
                            , div
                                [ class "console_output" ]
                                [ pre
                                    []
                                    [ text
                                        (if model.parseJson == Fail then
                                            "解析失败"

                                         else
                                            model.codeOutput.output ++ "\n" ++ model.codeOutput.errMessage
                                        )
                                    ]
                                , div
                                    [ class "put" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "right" ]
                            [ div
                                [ class "write_code" ]
                                [ div
                                    [ class "write_top" ]
                                    [ select
                                        [ class "drop-down" ]
                                        [ --     option
                                          --     []
                                          --     [ text "Elm" ]
                                          -- ,
                                        --   FIXME 数据库查询 放在session内？
                                          option
                                            [case model.language of
                                            "haskell" ->selected True
                                            _->selected False
                                            ,onClick (CheckLanguage "haskell")]
                                            [ text "Haskell" ]
                                        , option
                                            [case model.language of
                                            "java" ->selected True
                                            _->selected False
                                            , onClick (CheckLanguage "java") ]
                                            [ text "Java" ]
                                        , option
                                            [ case model.language of
                                            "python3" ->selected True
                                            _->selected False
                                            ,onClick (CheckLanguage "python3")]
                                            [ text "Python3" ]

                                        -- , option
                                        --     []
                                        --     [ text "PHP" ]
                                        ]
                                    ]
                                , textarea
                                    [ id "codeTextarea" ]
                                    [ text model.code ]
                                ]
                            , div
                                [ class "right_bottom" ]
                                [ div
                                    [ class "test_cases" ]
                                    [ div
                                        [ class "top" ]
                                        [ div
                                            [ class "word_two" ]
                                            [ text "Test cases" ]
                                        , div
                                            [ class "img_one" ]
                                            [ img
                                                [ src "/static/images/menu.png" ]
                                                []
                                            ]
                                        ]
                                    , div
                                        [ class "bottom" ]
                                        [ div
                                            [ class "test" ]
                                            [ button
                                                [ class "btn_test", onClick (SubmitCode 1) ]
                                                [ span
                                                    []
                                                    [ text "▶ PLAY TESTCASES" ]
                                                ]
                                            , span
                                                [ class "img_0" ]
                                                [ img
                                                    [ src "/static/images/01.png" ]
                                                    []
                                                ]
                                            , div
                                                [ class "word_0" ]
                                                [ text "Test only letter:E" ]
                                            ]
                                        , div
                                            [ class "test" ]
                                            [ button
                                                [ class "btn_test", onClick (SubmitCode 2) ]
                                                [ span
                                                    []
                                                    [ text "▶ PLAY TESTCASES" ]
                                                ]
                                            , span
                                                [ class "img_0" ]
                                                [ img
                                                    [ src "/static/images/02.png" ]
                                                    []
                                                ]
                                            , div
                                                [ class "word_0" ]
                                                [ text "Test MANHATTAN" ]
                                            ]
                                        , div
                                            [ class "test" ]
                                            [ button
                                                [ class "btn_test", onClick (SubmitCode 3) ]
                                                [ span
                                                    []
                                                    [ text "▶ PLAY TESTCASES" ]
                                                ]
                                            , span
                                                [ class "img_0" ]
                                                [ img
                                                    [ src "/static/images/03.png" ]
                                                    []
                                                ]
                                            , div
                                                [ class "word_0" ]
                                                [ text "Test ManhAtTan" ]
                                            ]
                                        , div
                                            [ class "test" ]
                                            [ button
                                                [ class "btn_test", onClick (SubmitCode 4) ]
                                                [ span
                                                    []
                                                    [ text "▶ PLAY TESTCASES" ]
                                                ]
                                            , span
                                                [ class "img_0" ]
                                                [ img
                                                    [ src "/static/images/04.png" ]
                                                    []
                                                ]
                                            , div
                                                [ class "word_0" ]
                                                [ text "Test M@NH@TT@N" ]
                                            ]
                                        , div
                                            [ class "test_0" ]
                                            [ button
                                                [ class "btn_test", onClick (SubmitCode 5) ]
                                                [ span
                                                    []
                                                    [ text "▶ PLAY TESTCASES" ]
                                                ]
                                            , span
                                                [ class "img_0" ]
                                                [ img
                                                    [ src "/static/images/05.png" ]
                                                    []
                                                ]
                                            , div
                                                [ class "word_0" ]
                                                [ text "MANHATTAN with..." ]
                                            ]
                                        ]
                                    ]
                                , div
                                    [ class "actions" ]
                                    [ div
                                        [ class "actions_top" ]
                                        [ text "Action" ]
                                    , div
                                        [ class "actions_bottom" ]
                                        [ button
                                            [ class "btn_1", onClick BatchSubmitCode ]
                                            [ text "▶ PLAY ALL   TESTCASES" ]
                                        , button
                                            [ class "btn_2" ]
                                            [ text "✔ SUBMIT" ]
                                        ]
                                    ]
                                ]
                            ]
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
            , message = ""
            , found = ""
            , expected = ""
            }
      , parseJson = Loading
      , jsonReqState = Loading
      , errMessage = ""
      , codeState = Loading
      , testIndex = 0
      , language = "python3"
      , batchSubmit=False
      }
    , --Cmd.none
      initCode ""
    )


initCode : String->Cmd Msg
initCode language=
    Http.post
        { url = "/init"

        --todo post 请求携带参数
        , body =
            multipartBody
            -- FIXME 初始默认是python
                [ stringPart "language" ( if (String.isEmpty language) then "python3" else language)
                ]
        , expect = Http.expectString GotText
        }