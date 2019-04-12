module Main exposing (main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Display


update : Msg -> Model -> Model
update msg model =
    case msg of
        Display ->
            0


view : Model -> Html Msg
view model =
    div
        [ class "all" ]
        [ div
            [ class "nag" ]
            [ a
                [ href "#" ]
                [ img
                    [ src "static/images/head.png" ]
                    []
                ]
            , a
                [ href "#" ]
                [ img
                    [ src "static/images/level.png" ]
                    []
                ]
            , a
                [ href "#" ]
                [ img
                    [ src "static/images/btn1.png" ]
                    []
                ]
            , a
                [ href "#" ]
                [ img
                    [ src "static/images/btn2.png" ]
                    []
                ]
            , a
                [ href "#" ]
                [ img
                    [ src "static/images/btn3.png" ]
                    []
                ]
            , a
                [ href "#" ]
                [ img
                    [ src "static/images/btn4.png" ]
                    []
                ]
            , div
                [ class "friends" ]
                [ a
                    [ href "#" ]
                    [ img
                        [ src "static/images/friends.png" ]
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
                        [ div
                            [ class "output" ]
                            [ text "Console output" ]
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
                                [ option
                                    []
                                    [ text "Elm" ]
                                , option
                                    []
                                    [ text "Haskell" ]
                                , option
                                    []
                                    [ text "Java" ]
                                , option
                                    []
                                    [ text "Python" ]
                                , option
                                    []
                                    [ text "PHP" ]
                                ]
                            ]
                        , textarea
                            [ id "codeTextarea" ]
                            []
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
                                        [ src "static/images/menu.png" ]
                                        []
                                    ]
                                ]
                            , div
                                [ class "bottom" ]
                                [ div
                                    [ class "test" ]
                                    [ button
                                        [ class "btn_test" ]
                                        [ span
                                            []
                                            [ text "▶ PLAY TESTCASES" ]
                                        ]
                                    , span
                                        [ class "img_0" ]
                                        [ img
                                            [ src "static/images/01.png" ]
                                            []
                                        ]
                                    , div
                                        [ class "word_0" ]
                                        [ text "Test only letter:E" ]
                                    ]
                                , div
                                    [ class "test" ]
                                    [ button
                                        [ class "btn_test" ]
                                        [ span
                                            []
                                            [ text "▶ PLAY TESTCASES" ]
                                        ]
                                    , span
                                        [ class "img_0" ]
                                        [ img
                                            [ src "static/images/02.png" ]
                                            []
                                        ]
                                    , div
                                        [ class "word_0" ]
                                        [ text "Test MANHATTAN" ]
                                    ]
                                , div
                                    [ class "test" ]
                                    [ button
                                        [ class "btn_test" ]
                                        [ span
                                            []
                                            [ text "▶ PLAY TESTCASES" ]
                                        ]
                                    , span
                                        [ class "img_0" ]
                                        [ img
                                            [ src "static/images/03.png" ]
                                            []
                                        ]
                                    , div
                                        [ class "word_0" ]
                                        [ text "Test ManhAtTan" ]
                                    ]
                                , div
                                    [ class "test" ]
                                    [ button
                                        [ class "btn_test" ]
                                        [ span
                                            []
                                            [ text "▶ PLAY TESTCASES" ]
                                        ]
                                    , span
                                        [ class "img_0" ]
                                        [ img
                                            [ src "static/images/04.png" ]
                                            []
                                        ]
                                    , div
                                        [ class "word_0" ]
                                        [ text "Test M@NH@TT@N" ]
                                    ]
                                , div
                                    [ class "test_0" ]
                                    [ button
                                        [ class "btn_test" ]
                                        [ span
                                            []
                                            [ text "▶ PLAY TESTCASES" ]
                                        ]
                                    , span
                                        [ class "img_0" ]
                                        [ img
                                            [ src "static/images/05.png" ]
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
                                    [ class "btn_1" ]
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
