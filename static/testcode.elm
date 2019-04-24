module Hello exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode as Encode


step : String -> Attribute msg
step name =
    attribute "step" name


intro : String -> Attribute msg
intro name =
    attribute "intro" name


position : String -> Attribute msg
position name =
    attribute "position" name


controls : String -> Attribute msg
controls name =
    attribute "controls" name


main : Html.Html msg
main =
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
                        [ div
                            [ class "top1", step "1", intro "<div class='step1'><div class='title'>Welcome to the onboarding</div><p class='title_detail'>CodinGame lets you improve your coding skills with games. It all starts in the IDE, where you will code and test new ideas.</p></div>", position "right" ]
                            [ video
                                [ controls "controls", class "video" ]
                                [ source
                                    [ src "" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "top2", step "2", intro "<div class='step2'><div class='title'>This is your mission statement</div><p class='title_detail'>Solo and multiplayer coding games are turn-based: at each turn, your program gets new inputs and must output the action.</p></div>", position "right" ]
                            [ div
                                [ class "goal" ]
                                [ div
                                    [ class "goal_title" ]
                                    [ text "⊙ The Goal" ]
                                , div
                                    [ class "goal_content" ]
                                    [ text "Fill in the game objectives here" ]
                                ]
                            , div
                                [ class "rules" ]
                                [ div
                                    [ class "rules_title" ]
                                    [ text "✔ Rules" ]
                                , div
                                    [ class "rules_content" ]
                                    [ text "Fill in the game rules here" ]
                                ]
                            ]
                        ]
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
                            [ div
                                [ class "top3", step "3", intro "<div class='step3'><div class='title'>Choose a programming language</div><p class='title_detail'>You can choose a language you already know or you can also learn a new language here. Ever tried 'Go'?</p></div>", position "left" ]
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
                            ]
                        , div
                            [ class "top4", step "4", intro "<div class='step4'><div class='title'>Time to code!</div><p class='title_detail'>OK, let’s tell your program to shoot the closest Alien. Copy/Paste this code at the right place in the code editor.</p><div id='code'>\n                if (dist1 < dist2) {\n                    System.out.println(enemy1);\n                } else {\n                    System.out.println(enemy2);\n                }\n              </div></div>", position "left" ]
                            [ textarea
                                [ id "codeTextarea" ]
                                []
                            ]
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
                                    [ class "top5", step "5", intro "<div class='step5'><div class='title'>Let's check your code</div><p class='title_detail'>Games have test cases to make sure you’ve solved it properly. Run your code by playing a test case.</p></div>", position "top" ]
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
                                    [ span
                                        []
                                        [ text "▶ PLAY ALL TESTCASES" ]
                                    ]
                                , div
                                    [ class "top6", step "6", intro "<div class='step6'><div class='title'>You’re ready to learn, code and play</div><p class='title_detail'>Submit your solution to verify the robustness of your program in different situations. Now, let’s play more advanced coding games!</p></div>", position "top" ]
                                    [ button
                                        [ class "btn_2" ]
                                        [ span
                                            []
                                            [ text "✔ SUBMIT" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
