module Main exposing (main)

-- import Css exposing (..)

import Array exposing (..)
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- import Html.Styled.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "ceshi", Cmd.none )


type Msg
    = Display


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Display ->
            ( model, Cmd.none )


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
                    ]
                    []
                , p [] [ text "声明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    ]
                    []
                , p [] [ text "输入说明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    ]
                    []
                , p [] [ text "输出说明" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
                    ]
                    []
                , p [] [ text "限制条件" ]
                , input
                    [ style "border" "1px solid #bbbbbb"
                    , style "width" "100%"
                    , style "height" "80px"
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
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , p [ style "color" "#838891" ] [ text "验证器1" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试2" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , p [ style "color" "#838891" ] [ text "验证器2" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试3" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , p [ style "color" "#838891" ] [ text "验证3" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , div
                        --分割线
                        [ style "border" "1px solid #bbbbbb"
                        , style "margin-top" "20px"
                        ]
                        []
                    , p [ style "color" "#838891" ] [ text "测试4" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
                    , p [ style "color" "#838891" ] [ text "验证4" ]
                    , textarea
                        [ style "placeholder" "输入"
                        , style "width" "45%"
                        , style "height" "80px"
                        ]
                        [ text "输入" ]
                    , textarea
                        [ style "placeholder" "输出"
                        , style "width" "45%"
                        , style "height" "80px"
                        , style "margin-left" "20px"
                        ]
                        [ text "输出" ]
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
                    , button
                        [ style "width" "50%"
                        , style "height" "50px"
                        , style "background-color" "rgba(242,187,19,0.7)"
                        ]
                        [ text "保存" ]
                    , button
                        [ style "width" "50%"
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
