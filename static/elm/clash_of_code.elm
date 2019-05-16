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
    , testCases1 : TestCases --测试用例
    , testCases2 : TestCases
    , testCases3 : TestCases
    , testCases4 : TestCases
    , radioValue : String --用于构建单选框
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , statement = ""
      , inputDescription = ""
      , outputDescription = ""
      , constraints = ""
      , testCases1 =
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
      , testCases2 =
            { testName = "test2"
            , validator = "validator2"
            , test =
                { input = ""
                , output = ""
                }
            , validater =
                { input = ""
                , output = ""
                }
            }
      , testCases3 =
            { testName = "test3"
            , validator = "validator3"
            , test =
                { input = ""
                , output = ""
                }
            , validater =
                { input = ""
                , output = ""
                }
            }
      , testCases4 =
            { testName = "test4"
            , validator = "validator4"
            , test =
                { input = ""
                , output = ""
                }
            , validater =
                { input = ""
                , output = ""
                }
            }
      , radioValue = "简单"
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
    | ChangeTestCasesTestInput1 String
    | ChangeTestCasesTestOutput1 String
    | ChangeTestCasesValidaterInput1 String
    | ChangeTestCasesValidaterOutput1 String
    | ChangeTestCasesTestInput2 String
    | ChangeTestCasesTestOutput2 String
    | ChangeTestCasesValidaterInput2 String
    | ChangeTestCasesValidaterOutput2 String
    | ChangeTestCasesTestInput3 String
    | ChangeTestCasesTestOutput3 String
    | ChangeTestCasesValidaterInput3 String
    | ChangeTestCasesValidaterOutput3 String
    | ChangeTestCasesTestInput4 String
    | ChangeTestCasesTestOutput4 String
    | ChangeTestCasesValidaterInput4 String
    | ChangeTestCasesValidaterOutput4 String
    | ChangeTestCases TestCases
    | TranRadio String


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
                        Http.multipartBody
                            [ stringPart "test"
                                (Encode.encode 1
                                    (Encode.object
                                        [ ( "title", Encode.string model.title )
                                        , ( "statement", Encode.string model.statement )
                                        , ( "inputDescription", Encode.string model.inputDescription )
                                        , ( "outputDescription", Encode.string model.outputDescription )
                                        , ( "constraints", Encode.string model.constraints )
                                        , ( "testCases1"
                                          , Encode.object
                                                [ ( "testName1", Encode.string model.testCases1.testName )
                                                , ( "validator1", Encode.string model.testCases1.validator )
                                                , ( "test1"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases1.test.input )
                                                        , ( "output", Encode.string model.testCases1.test.output )
                                                        ]
                                                  )
                                                , ( "validater1"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases1.validater.input )
                                                        , ( "output", Encode.string model.testCases1.validater.output )
                                                        ]
                                                  )
                                                ]
                                          )
                                        , ( "testCases2"
                                          , Encode.object
                                                [ ( "testName2", Encode.string model.testCases2.testName )
                                                , ( "validator2", Encode.string model.testCases2.validator )
                                                , ( "test2"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases2.test.input )
                                                        , ( "output", Encode.string model.testCases2.test.output )
                                                        ]
                                                  )
                                                , ( "validater2"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases2.validater.input )
                                                        , ( "output", Encode.string model.testCases2.validater.output )
                                                        ]
                                                  )
                                                ]
                                          )
                                        , ( "testCases3"
                                          , Encode.object
                                                [ ( "testName3", Encode.string model.testCases3.testName )
                                                , ( "validator3", Encode.string model.testCases3.validator )
                                                , ( "test3"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases3.test.input )
                                                        , ( "output", Encode.string model.testCases3.test.output )
                                                        ]
                                                  )
                                                , ( "validater3"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases3.validater.input )
                                                        , ( "output", Encode.string model.testCases3.validater.output )
                                                        ]
                                                  )
                                                ]
                                          )
                                        , ( "testCases4"
                                          , Encode.object
                                                [ ( "testName4", Encode.string model.testCases4.testName )
                                                , ( "validator4", Encode.string model.testCases4.validator )
                                                , ( "test4"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases4.test.input )
                                                        , ( "output", Encode.string model.testCases4.test.output )
                                                        ]
                                                  )
                                                , ( "validater4"
                                                  , Encode.object
                                                        [ ( "input", Encode.string model.testCases4.validater.input )
                                                        , ( "output", Encode.string model.testCases4.validater.output )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                    )
                                )
                            ]
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

        ChangeTestCasesTestInput1 newTestCasesTestInput ->
            updateTestCasesTestInput1 (\test -> { test | input = newTestCasesTestInput }) model.testCases1 model

        ChangeTestCasesTestOutput1 newTestCasesTestOutput ->
            updateTestCasesTestInput1 (\test -> { test | output = newTestCasesTestOutput }) model.testCases1 model

        ChangeTestCasesValidaterInput1 newTestCasesValidaterInput ->
            updateTestCasesValidater1 (\validater -> { validater | input = newTestCasesValidaterInput }) model.testCases1 model

        ChangeTestCasesValidaterOutput1 newTestCasesValidaterOutput ->
            updateTestCasesValidater1 (\validater -> { validater | output = newTestCasesValidaterOutput }) model.testCases1 model

        ChangeTestCasesTestInput2 newTestCasesTestInput ->
            updateTestCasesTestInput2 (\test -> { test | input = newTestCasesTestInput }) model.testCases2 model

        ChangeTestCasesTestOutput2 newTestCasesTestOutput ->
            updateTestCasesTestInput2 (\test -> { test | output = newTestCasesTestOutput }) model.testCases2 model

        ChangeTestCasesValidaterInput2 newTestCasesValidaterInput ->
            updateTestCasesValidater2 (\validater -> { validater | input = newTestCasesValidaterInput }) model.testCases2 model

        ChangeTestCasesValidaterOutput2 newTestCasesValidaterOutput ->
            updateTestCasesValidater2 (\validater -> { validater | output = newTestCasesValidaterOutput }) model.testCases2 model

        ChangeTestCasesTestInput3 newTestCasesTestInput ->
            updateTestCasesTestInput3 (\test -> { test | input = newTestCasesTestInput }) model.testCases3 model

        ChangeTestCasesTestOutput3 newTestCasesTestOutput ->
            updateTestCasesTestInput3 (\test -> { test | output = newTestCasesTestOutput }) model.testCases3 model

        ChangeTestCasesValidaterInput3 newTestCasesValidaterInput ->
            updateTestCasesValidater3 (\validater -> { validater | input = newTestCasesValidaterInput }) model.testCases3 model

        ChangeTestCasesValidaterOutput3 newTestCasesValidaterOutput ->
            updateTestCasesValidater3 (\validater -> { validater | output = newTestCasesValidaterOutput }) model.testCases3 model

        ChangeTestCasesTestInput4 newTestCasesTestInput ->
            updateTestCasesTestInput4 (\test -> { test | input = newTestCasesTestInput }) model.testCases4 model

        ChangeTestCasesTestOutput4 newTestCasesTestOutput ->
            updateTestCasesTestInput4 (\test -> { test | output = newTestCasesTestOutput }) model.testCases4 model

        ChangeTestCasesValidaterInput4 newTestCasesValidaterInput ->
            updateTestCasesValidater4 (\validater -> { validater | input = newTestCasesValidaterInput }) model.testCases4 model

        ChangeTestCasesValidaterOutput4 newTestCasesValidaterOutput ->
            updateTestCasesValidater4 (\validater -> { validater | output = newTestCasesValidaterOutput }) model.testCases4 model

        ChangeTestCases newTestCases ->
            ( model, Cmd.none )

        TranRadio value ->
            ( { model | radioValue = value }, Cmd.none )


updateTestName : (TestCases -> TestCases) -> Model -> ( Model, Cmd Msg )
updateTestName transform model =
    ( { model | testCases1 = transform model.testCases1 }, Cmd.none )

--像json中低层级的key中添加值的一组方法
updateTestCasesTestInput1 : (TestCases_Test -> TestCases_Test) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesTestInput1 transdate testCases1 model =
    ( { model | testCases1 = { testCases1 | test = transdate model.testCases1.test } }, Cmd.none )

updateTestCasesTestInput2 : (TestCases_Test -> TestCases_Test) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesTestInput2 transdate testCases2 model =
    ( { model | testCases2 = { testCases2 | test = transdate model.testCases2.test } }, Cmd.none )

updateTestCasesTestInput3 : (TestCases_Test -> TestCases_Test) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesTestInput3 transdate testCases3 model =
    ( { model | testCases3 = { testCases3 | test = transdate model.testCases3.test } }, Cmd.none )

updateTestCasesTestInput4 : (TestCases_Test -> TestCases_Test) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesTestInput4 transdate testCases4 model =
    ( { model | testCases4 = { testCases4 | test = transdate model.testCases4.test } }, Cmd.none )


updateTestCasesValidater1 : (TestCases_Validater -> TestCases_Validater) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesValidater1 transdate testCases1 model =
    ( { model | testCases1 = { testCases1 | validater = transdate model.testCases1.validater } }, Cmd.none )

updateTestCasesValidater2 : (TestCases_Validater -> TestCases_Validater) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesValidater2 transdate testCases2 model =
    ( { model | testCases2 = { testCases2 | validater = transdate model.testCases2.validater } }, Cmd.none )

updateTestCasesValidater3 : (TestCases_Validater -> TestCases_Validater) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesValidater3 transdate testCases3 model =
    ( { model | testCases3 = { testCases3 | validater = transdate model.testCases3.validater } }, Cmd.none )

updateTestCasesValidater4 : (TestCases_Validater -> TestCases_Validater) -> TestCases -> Model -> ( Model, Cmd Msg )
updateTestCasesValidater4 transdate testCases4 model =
    ( { model | testCases4 = { testCases4 | validater = transdate model.testCases4.validater } }, Cmd.none )


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
                    [ p [] [ text "游戏难度" ]
                    , label [] [ input [ type_ "radio", value "简单", checked (model.radioValue == "简单"), onClick (TranRadio "简单") ] [], text "简单" ]
                    , label [] [ input [ type_ "radio", value "中等", checked (model.radioValue == "中等"), onClick (TranRadio "中等") ] [], text "中等" ]
                    , label [] [ input [ type_ "radio", value "困难", checked (model.radioValue == "困难"), onClick (TranRadio "困难") ] [], text "困难" ]
                    , label [] [ input [ type_ "radio", value "专家", checked (model.radioValue == "专家"), onClick (TranRadio "专家") ] [], text "专家" ]
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
                        , value model.testCases1.test.input
                        , onInput ChangeTestCasesTestInput1
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases1.test.output
                        , onInput ChangeTestCasesTestOutput1
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证器1" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases1.validater.input
                        , onInput ChangeTestCasesValidaterInput1
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases1.validater.output
                        , onInput ChangeTestCasesValidaterOutput1
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
                        , value model.testCases2.test.input
                        , onInput ChangeTestCasesTestInput2
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases2.test.output
                        , onInput ChangeTestCasesTestOutput2
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证器2" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases2.validater.input
                        , onInput ChangeTestCasesValidaterInput2
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases2.validater.output
                        , onInput ChangeTestCasesValidaterOutput2
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
                        , value model.testCases3.test.input
                        , onInput ChangeTestCasesTestInput3
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases3.test.output
                        , onInput ChangeTestCasesTestOutput3
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证3" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases3.validater.input
                        , onInput ChangeTestCasesValidaterInput3
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases3.validater.output
                        , onInput ChangeTestCasesValidaterOutput3
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
                        , value model.testCases4.test.input
                        , onInput ChangeTestCasesTestInput4
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases4.test.output
                        , onInput ChangeTestCasesTestOutput4
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "验证4" ]
                    , textarea
                        [ placeholder "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        , value model.testCases4.validater.input
                        , onInput ChangeTestCasesValidaterInput4
                        ]
                        []
                    , textarea
                        [ placeholder "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        , value model.testCases4.validater.output
                        , onInput ChangeTestCasesValidaterOutput4
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
                    [ p [] [ text "未解决方案" ]
                    , textarea
                        [ style "width" "100%"
                        , style "height" "200px"
                        ]
                        []
                    ]


                , div []
                    [
                     div
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
