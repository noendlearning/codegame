module Main exposing (main)

import Array exposing (..)
import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Title =
    String


type alias Statement =
    String


type alias InputDescription =
    String


type alias OutputDescription =
    String


type alias Constraints =
    String


type alias TestCases_Test =
    { input : String, output : String }


type alias TestCases_Validater =
    { input : String, output : String }


type alias TestCases =
    { testName : String
    , validator : String
    , test : TestCases_Test
    , validater : TestCases_Validater
    }


type alias Model =
    { title : Title --标题
    , statement : Statement --声明
    , inputDescription : InputDescription --输入说明
    , outputDescription : OutputDescription --输出说明
    , constraints : Constraints --限制条件
    , testCases : TestCases --测试用例
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , statement = ""
      , inputDescription = ""
      , outputDescription = ""
      , constraints = ""
      , testCases =
            { testName = "test1"
            , validator = "validator1"
            , test =
                { input = ""
                , output = ""
                }
            , validater =
                { input = ""
                , output = ""
                }
            }
      }
    , Cmd.none
    )


type Msg
    = Display (Result Http.Error String)
    | PushDate
    | ChangeTitle String
    | ChangeStatemen String
    | ChangeInputDescription String
    | ChangeOutputDescription String
    | ChangeConstraints String
    | ChangeTestCasesTestName String
    | ChangeTestCasesValidator String
    | ChangeTestCasesTestInput String
    | ChangeTestCasesTestOutput String
    | ChangeTestCasesValidaterInput String
    | ChangeTestCasesValidaterOutput String
    | ChangeTestCases TestCases


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Display result ->
            case result of
                Ok fullt ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PushDate ->
            Debug.log "ceshi"
                ( model
                , Http.post
                    { url = "/code"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "test"
                                  , Encode.object
                                        [ ( "title", Encode.string model.title )
                                        , ( "statement", Encode.string model.statement )
                                        , ( "inputDescription", Encode.string model.inputDescription )
                                        , ( "outputDescription", Encode.string model.outputDescription )
                                        , ( "constraints", Encode.string model.constraints )
                                        , ( "testCases"
                                          , Encode.object
                                                [ ( "testName", Encode.string model.testCases.testName )
                                                , ( "validator", Encode.string model.testCases.validator )
                                                , ( "test"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases.test.input )
                                                        , ( "output", Encode.string model.testCases.test.output )
                                                        ]
                                                  )
                                                , ( "validater"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases.validater.input )
                                                        , ( "output", Encode.string model.testCases.validater.output )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                            )
                    , expect = Http.expectString Display
                    }
                )

        ChangeTitle newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        ChangeStatemen newStatement ->
            ( { model | statement = newStatement }, Cmd.none )

        ChangeInputDescription newInputDescription ->
            ( { model | inputDescription = newInputDescription }, Cmd.none )

        ChangeOutputDescription newOutputDescription ->
            ( { model | outputDescription = newOutputDescription }, Cmd.none )

        ChangeConstraints newConstraints ->
            ( { model | constraints = newConstraints }, Cmd.none )

        ChangeTestCasesTestName newTestCasesTestName ->
            updateTestName (\testCases -> { testCases | testName = newTestCasesTestName }) model

        ChangeTestCasesValidator newTestCasesValidator ->
            updateTestName (\testCases -> { testCases | validator = newTestCasesValidator }) model

        ChangeTestCasesTestInput newTestCasesTestInput ->
            updateTestCasesTestInput (\test -> { test | input = newTestCasesTestInput }) model.testCases model

        ChangeTestCasesTestOutput newTestCasesTestOutput ->
            updateTestCasesTestInput (\test -> { test | output = newTestCasesTestOutput }) model.testCases model

        ChangeTestCasesValidaterInput newTestCasesValidaterInput ->
            updateTestCasesValidater (\validater -> { validater | input = newTestCasesValidaterInput }) model.testCases model

        ChangeTestCasesValidaterOutput newTestCasesValidaterOutput ->
            updateTestCasesValidater (\validater -> { validater | output = newTestCasesValidaterOutput }) model.testCases model

        ChangeTestCases newTestCases ->
            ( model, Cmd.none )


updateTestName : (TestCases -> TestCases) -> Model -> ( Model, Cmd Msg )
updateTestName transform model =
    ( { model | testCases = transform model.testCases }, Cmd.none )


updateTestCasesTestInput : (TestCases_Test -> TestCases_Test) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesTestInput transdate testCases model =
    ( { model | testCases = { testCases | test = transdate model.testCases.test } }, Cmd.none )


updateTestCasesValidater : (TestCases_Validater -> TestCases_Validater) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesValidater transdate testCases model =
    ( { model | testCases = { testCases | validater = transdate model.testCases.validater } }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "background-color" "#454c55"
        , style "width" "100%"
        , style "height" "3500px"
        ]
        [ div
            [ style "position" "absolute"
            , style "top" "50%"
            , style "left" "50%"
            , style "height" "3300px"
            , style "width" "50%"
            , style "margin" "-15% 0 0 -25%"
            , style "border" "1px solid #aaaaaa"
            , style "background-color" "#ffffff"
            ]
            [ Html.form
                [ style "margin" "50px"
                , style "display" "block"
                , style "width" "90%"
                , style "height" "90%"
                ]
                [ p
                    []
                    [ text "在这个页面上你可以设计自己的拼图！点击"
                    , a
                        [ href "#" ]
                        [ text "这里" ]
                    , text
                        "获取更多有关信息"
                    ]
                , p
                    []
                    [ text "标题" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "40px"
                    , value model.title
                    , onInput ChangeTitle
                    ]
                    []
                , p [] [ text "声明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    , value model.statement
                    , onInput ChangeStatemen
                    ]
                    []
                , p [] [ text "输入说明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    , value model.inputDescription
                    , onInput ChangeInputDescription
                    ]
                    []
                , p [] [ text "输出说明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    , value model.outputDescription
                    , onInput ChangeOutputDescription
                    ]
                    []
                , p [] [ text "限制条件" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    , value model.constraints
                    , onInput ChangeConstraints
                    ]
                    []
                , div
                    --Game modes
                    [ style "width" "100%"
                    , style "height" "120px"
                    ]
                    [ p [] [ text "游戏模式" ]
                    , label [] [ input [ type_ "checkbox" ] [], text "Fastest" ]
                    , label [] [ input [ type_ "checkbox" ] [], text "Shortest" ]
                    , label [] [ input [ type_ "checkbox" ] [], text "Reverse" ]
                    ]
                , div
                    --Test cases
                    [ style "width" "100%"
                    ]
                    [ text "测试用例"
                    , p [ style "color" "#838891" ] [ text "测试1" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases.test.input
                        , onInput ChangeTestCasesTestInput
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases.test.output
                        , onInput ChangeTestCasesTestOutput
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证器1" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases.validater.input
                        , onInput ChangeTestCasesValidaterInput
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases.validater.output
                        , onInput ChangeTestCasesValidaterOutput
                        ]
                        []
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试2" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证器2" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试3" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证3" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试4" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证4" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        []
                    , button
                        [ style "width" "150px"
                        , style "height" "30px"
                        , style "background-color" "rgba(69,76,85,0.15)"
                        ]
                        [ text "添加一个测试用例" ]
                    ]
                , div
                    [ style "width" "100%"
                    , style "margin-top" "20px"
                    ]
                    [ p [] [ text "解决语言" ]
                    , select
                        [ style "width" "100px"
                        , style "height" "30px"
                        ]
                        [ option [] [ text "Java" ]
                        , option [] [ text "Python" ]
                        , option [] [ text "Haskell" ]
                        ]
                    ]
                , div []
                    --
                    [ p [] [ text "解决方案" ]
                    , textarea
                        [ style "width" "100%"
                        , style "height" "200px"
                        ]
                        []
                    ]
                , div []
                    --Stub generator input
                    [ p [] [ text "生成器输入" ]
                    , textarea
                        [ style "width" "100%"
                        , style "height" "200px"
                        ]
                        []
                    ]
                , div
                    --Preview - Stub generator language
                    [ style "width" "100%"
                    , style "margin-top" "20px"
                    ]
                    [ p [] [ text "生成器语言" ]
                    , select
                        [ style "width" "100px"
                        , style "height" "30px"
                        ]
                        [ option [] [ text "Java" ]
                        , option [] [ text "Python" ]
                        , option [] [ text "Haskell" ]
                        ]
                    ]
                , div []
                    [ p [] [ text "预览 生成的存根" ]
                    , textarea
                        [ style "width" "100%"
                        , style "height" "200px"
                        ]
                        []
                    ]
                , div []
                    [ div [ style "margin" "50px 0px 20px 0px" ]
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                ]
                                []
                            , text "草案（他不会被其他编辑器看到）"
                            ]
                        ]
                    , div
                        [ style "display" "inline-block"
                        , style "width" "50%"
                        , style "height" "50px"
                        , style "background-color" "rgba(242,187,19,0.7)"
                        , onClick PushDate
                        ]
                        [ text "保存" ]
                    , div
                        [ style "display" "inline-block"
                        , style "width" "50%"
                        , style "height" "50px"
                        , style "background-color" "rgba(69,76,85,0.1)"
                        ]
                        [ text "测试在IDE" ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
