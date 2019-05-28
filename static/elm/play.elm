port module Main exposing (Model, Msg(..), StateModel(..), init, jsonReq, main, outputDecoder, subscriptions, update, view)

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

port receiveData : (String-> msg) -> Sub msg

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
    , languageId : String
    , batchSubmit : Bool
    ,languages:List Language
    ,uuid:String
    ,solutions:List Solution
    ,puzzles:List Puzzle
    ,validations:List Validation
    }


type Msg
    =
    -- GotText (Result Http.Error String)
    ChangeCode String --输入代码
    | RenderOutput (Result Http.Error String) --代码运行结果填充页面
    | SubmitCode Int -- 提交代码
    | CheckLanguage String --选择语言
    | BatchSubmitCode
    | ReceiveDataFromJS String
    -- 根据uuid向后台查询 返回的结果
    | GotPuzzle (Result Http.Error String)
    | GotLanguage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPuzzle result ->
            case result of
                Ok fullText ->
                    case Decode.decodeString resDecoder fullText of
                        Ok ookk->
                            --todo: tmd 这是我写过最长的json分析 恶心死了
                            let
                                o=Debug.log "000000000000" ookk
                                puzzles=ookk.puzzle
                                solutions=ookk.solution
                                validations=ookk.validation
                            in
                                ({model|puzzles=puzzles,validations=validations,solutions=solutions},Cmd.none)
                        Err _ ->
                            Debug.log "err......" (model,Cmd.none)

                Err _ ->
                    Debug.log "fail" ( { model | loadState = Fail }, Cmd.none )
        GotLanguage result->
            case result of
                Ok fullText->
                    case Decode.decodeString languagesDecoder fullText of
                        Ok langs ->

                            ({ model | languages = langs}, Cmd.none )

                        Err _ ->
                            Debug.log "err" ( model, Cmd.none )
                Err _ ->
                -- fixme: 这里可能需要为model增加一个状态，告知语言列表获取失败
                    Debug.log "fail" (model,Cmd.none)
        ChangeCode str ->
            ( { model | code= str }, Cmd.none )

        SubmitCode index ->
            ( { model | testIndex = index }, jsonReq index model.code model.languageId )

        RenderOutput result ->
            --渲染代码运行的结果
            case result of
                Ok fullText ->
                    --服务器成功返回数据
                    case Decode.decodeString outputDecoder fullText of
                        Ok output ->
                            if model.batchSubmit then
                                if model.testIndex<5 then
                                    Debug.log (String.fromInt model.testIndex) ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success,testIndex=model.testIndex+1 }, jsonReq (model.testIndex+1) model.code model.languageId )

                                else
                                    Debug.log (String.fromInt model.testIndex) ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success,testIndex=5,batchSubmit=False }, jsonReq 5 model.code model.languageId )

                            else
                                Debug.log "output3" ( { model | codeOutput = output, jsonReqState = Success, parseJson = Success }, Cmd.none )

                        Err _ ->
                            ( { model | parseJson = Fail, jsonReqState = Success }, Cmd.none )

                Err _ ->
                    --服务器返回失败
                    ( { model | jsonReqState = Fail }, Cmd.none )

        CheckLanguage languageId ->
            let
                uu=model.uuid
            in
                ( { model | languageId = languageId }, initCode uu languageId)
        BatchSubmitCode ->
            ({model|batchSubmit=True,testIndex=1},jsonReq 1 model.code model.languageId)

        ReceiveDataFromJS uuid->
        --  发送请求到后台 根据uuid查询puzzle validation solution
            let
                languageId="faf338cb-80fd-445d-b345-77c09c6d8581"
            in
                ({model|uuid=uuid},initCode uuid languageId)

-- 页面初始化 渲染页面
initCode:String -> String->Cmd Msg
initCode uuid languageId=
    Http.post
        {
            url="init",
            body=
                multipartBody
                    [
                        stringPart "puzzleId" uuid,
                        stringPart "languageId" languageId
                    ],
            expect=Http.expectString GotPuzzle
        }

type alias Validation=
    {
        input:String,
        output:String,
        title:String,
        orders:Int
    }

type alias Puzzle=
    {
        title:String,
        inputDescription:String,
        outputDescription:String,
        constraints:String
    }

puzzlesDecoder:Decode.Decoder (List Puzzle)
puzzlesDecoder=
    Decode.list puzzleDecoder

puzzleDecoder:Decoder Puzzle
puzzleDecoder=
    map4 Puzzle
        (Decode.field "puzzleTitle" string)
        (Decode.field "puzzleInputDescription" string)
        (Decode.field "puzzleOutputDescription" string)
        (Decode.field "puzzleConstraints" string)


validationsDecoder:Decode.Decoder (List Validation)
validationsDecoder=
    Decode.list validationDecoder

validationDecoder : Decoder Validation
validationDecoder =
    map4 Validation
        (Decode.field "validationInput" string)
        (Decode.field "validationOutput" string)
        (Decode.field "validationTitle" string)
        (Decode.field "validationOrders" int)

type alias Solution=
    {
        uuid:String,
        language:String,
        code:String,
        unsolve:String
    }

solutionsDecoder:Decode.Decoder (List Solution)
solutionsDecoder=
    Decode.list solutionDecoder

solutionDecoder:Decoder Solution
solutionDecoder =
    map4 Solution
        (Decode.field "solutionUuid" string)
        (Decode.field "solutionLanguage" string)
        (Decode.field "solutionCode" string)
        (Decode.field "solutionUnsolve" string)

type alias Res=
    {
        solution:List Solution,
        puzzle:List Puzzle,
        validation:List Validation
    }

resDecoder : Decoder Res
resDecoder=
    Decode.map3 Res
        (Decode.index 0 solutionsDecoder)
        (Decode.index 1 puzzlesDecoder)
        (Decode.index 2 validationsDecoder)


type alias Language=
    {
        uuid:String,
        language:String
    }

languageDecoder:Decoder Language
languageDecoder =
    map2 Language
        (Decode.field "languagesUuid" string)
        (Decode.field "languagesLanguage" string)

languagesDecoder:Decode.Decoder (List Language)
languagesDecoder=
    Decode.list languageDecoder

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
        _ ->
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
                                        (List.map
                                            (
                                                \x->
                                                    option
                                                    [(if model.languageId==x.uuid then
                                                        selected True
                                                    else
                                                        selected False)
                                                    ,onClick (CheckLanguage x.uuid)]
                                                    [ text x.language ]
                                            )
                                        model.languages)
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
                                        ( List.map
                                            (\x->
                                                div
                                                [ class "test" ]
                                                [ button
                                                    [ class "btn_test", onClick (SubmitCode x.orders) ]
                                                    [ span
                                                        []
                                                        [ text "▶ PLAY TESTCASES" ]
                                                    ]
                                                , span
                                                    [ class "img_0" ]
                                                    [ img
                                                        [ src ("/static/images/0"++String.fromInt x.orders++".png")]
                                                        []
                                                    ]
                                                , div
                                                    [ class "word_0" ]
                                                    [ text x.title ]
                                                ]

                                            )
                                        model.validations)
                                        -- [
                                        --     div
                                        --     [ class "test" ]
                                        --     [ button
                                        --         [ class "btn_test", onClick (SubmitCode 1) ]
                                        --         [ span
                                        --             []
                                        --             [ text "▶ PLAY TESTCASES" ]
                                        --         ]
                                        --     , span
                                        --         [ class "img_0" ]
                                        --         [ img
                                        --             [ src "/static/images/01.png" ]
                                        --             []
                                        --         ]
                                        --     , div
                                        --         [ class "word_0" ]
                                        --         [ text "Test only letter:E" ]
                                        --     ]
                                        -- , div
                                        --     [ class "test" ]
                                        --     [ button
                                        --         [ class "btn_test", onClick (SubmitCode 2) ]
                                        --         [ span
                                        --             []
                                        --             [ text "▶ PLAY TESTCASES" ]
                                        --         ]
                                        --     , span
                                        --         [ class "img_0" ]
                                        --         [ img
                                        --             [ src "/static/images/02.png" ]
                                        --             []
                                        --         ]
                                        --     , div
                                        --         [ class "word_0" ]
                                        --         [ text "Test MANHATTAN" ]
                                        --     ]
                                        -- , div
                                        --     [ class "test" ]
                                        --     [ button
                                        --         [ class "btn_test", onClick (SubmitCode 3) ]
                                        --         [ span
                                        --             []
                                        --             [ text "▶ PLAY TESTCASES" ]
                                        --         ]
                                        --     , span
                                        --         [ class "img_0" ]
                                        --         [ img
                                        --             [ src "/static/images/03.png" ]
                                        --             []
                                        --         ]
                                        --     , div
                                        --         [ class "word_0" ]
                                        --         [ text "Test ManhAtTan" ]
                                        --     ]
                                        -- , div
                                        --     [ class "test" ]
                                        --     [ button
                                        --         [ class "btn_test", onClick (SubmitCode 4) ]
                                        --         [ span
                                        --             []
                                        --             [ text "▶ PLAY TESTCASES" ]
                                        --         ]
                                        --     , span
                                        --         [ class "img_0" ]
                                        --         [ img
                                        --             [ src "/static/images/04.png" ]
                                        --             []
                                        --         ]
                                        --     , div
                                        --         [ class "word_0" ]
                                        --         [ text "Test M@NH@TT@N" ]
                                        --     ]
                                        -- , div
                                        --     [ class "test_0" ]
                                        --     [ button
                                        --         [ class "btn_test", onClick (SubmitCode 5) ]
                                        --         [ span
                                        --             []
                                        --             [ text "▶ PLAY TESTCASES" ]
                                        --         ]
                                        --     , span
                                        --         [ class "img_0" ]
                                        --         [ img
                                        --             [ src "/static/images/05.png" ]
                                        --             []
                                        --         ]
                                        --     , div
                                        --         [ class "word_0" ]
                                        --         [ text "MANHATTAN with..." ]
                                        --     ]
                                        -- ]
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
                                            [ text "▶ PLAY ALL  TESTCASES" ]
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


subscriptions : Model-> Sub Msg
subscriptions model =
    receiveData ReceiveDataFromJS


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
    , languageId = "faf338cb-80fd-445d-b345-77c09c6d8581"
    , batchSubmit=False
    ,languages=[]
    ,uuid=""
    ,solutions=[]
    ,puzzles=[]
    ,validations=[]
    }
    ,
        initLanguage
    )

-- 页面初始化 渲染页面
initLanguage:Cmd Msg
initLanguage =
    Http.get
        {
            url="language",
            expect=Http.expectString GotLanguage
        }