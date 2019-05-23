module Hello exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Html.Html msg
main =
    div
        [ class "all" ]
        [ div
            [ class "logo" ]
            [ div
                [ class "dropdown" ]
                [ img
                    [ class "dropbtn", src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACYAAAAoCAYAAACSN4jeAAAA50lEQVRYhe3WvQrCMBSGYW+mIA4F7SIEN70OXcSpiqgoFkVExMv0B23S6mbjUOFzsleQ4iecwLuGZ0g4p+L5TTBW+TVAYAITmMAE9o+wbj+ENgm0SdAbDHlgx9MF33M6X3hgSXovYOn9wQNbbvbI8zfy/I3V9sAD8/wmAtVBoDpO7nIKc50zWC1ooVpXXLBwsoS1LzyzDKNZxAO7xbr4lbE2PLBYG07YcBrB2hcyazGer3lgnk/6+MvIGayh2mioNhdssd4VIyna7HlgtEOcdu3p9kOYJMX1FnMtimUkMIEJjC2BCazsPrYjgXgcRrhiAAAAAElFTkSuQmCC" ]
                    []
                , div
                    [ class "dropdown-content" ]
                    [ a
                        [ href "#" ]
                        [ text "BLOG" ]
                    , a
                        [ href "#" ]
                        [ text "FORUM" ]
                    , a
                        [ href "#" ]
                        [ text "ABOUT US" ]
                    , a
                        [ href "#" ]
                        [ text "COMPANY" ]
                    , a
                        [ href "#" ]
                        [ text "FAQ" ]
                    , a
                        [ href "#" ]
                        [ text "FACEBOOK" ]
                    , a
                        [ href "#" ]
                        [ text "TWITTER" ]
                    ]
                ]
            , div
                [ class "dropdown" ]
                [ button
                    [ class "dropbtn" ]
                    [ span
                        []
                        [ text "username∨" ]
                    ]
                , div
                    [ class "dropdown-content" ]
                    [ a
                        [ href "#" ]
                        [ text "LEVEL" ]
                    , a
                        [ href "#" ]
                        [ text "FRIENDS" ]
                    , a
                        [ href "#" ]
                        [ text "HOME" ]
                    , a
                        [ href "#" ]
                        [ text "MY PRIFILE" ]
                    , a
                        [ href "#" ]
                        [ text "SETTINGS" ]
                    , a
                        [ href "#" ]
                        [ text "SING OUT" ]
                    ]
                ]
            , img
                [ class "touxiang", src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACMAAAAjCAYAAAAe2bNZAAAAhElEQVRYhe3UsQqAIBSFYV+mtbn3f4w76ejoII62RYgW3pJ70fPDWRriQzKz7UfWMiMNqGKISHTAAAMMMK2ViWFarY15a13MEyjGqAcj8gHXQNz3zPHTu+e9v55ba9mnxML0NgzDTRXGOfcv5mvzYogohxC6ESmlcbdp1IABBpj5MRp2Av+hp1Pv+sEaAAAAAElFTkSuQmCC" ]
                []
            , img
                [ class "message", src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABoAAAAaCAYAAACpSkzOAAAAwElEQVRIie3RMQqDMBQGYC/TtXOv4OiYSRdx8jougoKDB1AUPYeIe1uSDOrkm18nC5ZqkyillPzwb3n54D3jdL7gN2poSENSEHE8zMsaKeM4ASBlHPOyRuJ4x0FRkuJWoiTdDwVhvInMCcJYHTItIoTMMS2iBn1amewKV6Gm7aSgpu3UIMq4FEQZ15CGFCHb9dF2feyHUQrqh/E5KwQdESHoeru/7QSw+GwCWH0rdaPXZkW1gLKiEpr7XWhv/w96AGid4Xck8ekWAAAAAElFTkSuQmCC" ]
                []
            , div
                [ class "icon" ]
                [ img
                    [ src "static/images/icon.png" ]
                    []
                ]
            , ul
                [ class "nav" ]
                [ li
                    []
                    [ a
                        [ href "#" ]
                        [ text "PRACTICE" ]
                    ]
                , li
                    []
                    [ a
                        [ href "#" ]
                        [ text "COMPETE" ]
                    ]
                , li
                    []
                    [ a
                        [ href "#" ]
                        [ text "CONTRIBUTE" ]
                    ]
                , li
                    []
                    [ a
                        [ href "#" ]
                        [ text "LEARN" ]
                    ]
                ]
            ]
        , div
            [ class "main" ]
            [ div
                [ class "right" ]
                []
            , div
                [ class "left" ]
                [ div
                    [ class "container" ]
                    [ div
                        [ class "con_top" ]
                        [ div
                            [ class "top_center" ]
                            [ a
                                [ href "#", class "word_1" ]
                                [ text "PRACTICE" ]
                            , span
                                []
                                [ text ">" ]
                            , div
                                [ class "word_2" ]
                                [ text "Classic Puzzle - Easy" ]
                            ]
                        ]
                    , div
                        [ class "con_center" ]
                        [ div
                            [ class "word_3" ]
                            [ text "TO START" ]
                        , div
                            [ class "picture", id "picture" ]
                            [ a
                                [ href "#" ]
                                [ img
                                    [ src "#" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "picture" ]
                            [ a
                                [ href "#" ]
                                [ img
                                    [ src "#" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "picture_2", id "picture_2" ]
                            [ a
                                [ href "#" ]
                                [ img
                                    [ src "#" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "word_3" ]
                            [ text "COMPLETED" ]
                        , div
                            [ class "picture_2", id "picture_2" ]
                            [ a
                                [ href "#" ]
                                [ img
                                    [ src "#" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div
                    [ class "bottom" ]
                    [ img
                        [ src "static/images/bottom.png" ]
                        []
                    ]
                ]
            ]
        ]
