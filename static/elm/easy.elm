import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Http exposing (..)
import Debug exposing (..)

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { loadState : StateModel
    ,puzzles:Puzzles
    }

type StateModel
    = Fail
    | Success
    | Loading
    | ParseError

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> (Model, Cmd Msg)
init _ =
      (
        {loadState=Loading
        ,puzzles=[]
        },
        getEasyPuzzles
      )

getEasyPuzzles:Cmd Msg
getEasyPuzzles =
  Http.get
    {
      url="/easypuzzles"
    , expect = Http.expectString GotText
    }

type Msg=GotText (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      GotText result ->
        case result of
          Ok fullText ->
            let res=Debug.log "parse" (Decode.decodeString puzzlesDecoder fullText)
                ful=Debug.log "fullText" fullText
            in case res of
                Ok output ->
                  Debug.log "puzzles" ({model|loadState=Success,puzzles=output},Cmd.none)
                Err _ ->
                -- 解析错误，后台返回的不是puzzle的list，而只是单纯的错误信息
                  Debug.log "error" ({model|loadState=ParseError},Cmd.none)
          Err _ ->
            Debug.log "hh" ({model|loadState=Fail},Cmd.none)

type alias Puzzle=
  {
    puzzleUuid:String,
    puzzleTitle:String
  }

type alias Puzzles=List Puzzle

puzzlesDecoder : Decode.Decoder (List Puzzle)
puzzlesDecoder =
    Decode.list puzzleDecoder

puzzleDecoder : Decode.Decoder Puzzle
puzzleDecoder =
    map2 Puzzle
        (field "puzzleUuid" string)
        (field "puzzleTitle" string)

getPanel :Model->List (Html Msg)
getPanel model=
  List.map (\x->
    div [ class "level-puzzle" ]
        [ div [ class "cg-puzzle-tile large" ]
          [ div [ class "puzzle-wrapper" ]
            [ a [ class "puzzle-content puzzle-hover", href ("/training?uuid=" ++ x.puzzleUuid) ]
              [ img [ class "tile-image", src "/static/images/thor.jpg" ]
                []
              , div [ class "background-container" ]
                [ div [ class "background" ]
                  [ div [ class "puzzle-details-container" ]
                    [ div [ class "puzzle-info" ]
                      [ div [ class "puzzle-title" ]
                        [ text "Power of Thor - Episode 1" ]
                      , div [ class "xp-points" ]
                        [ span [ class "xp-logo" ]
                          [ text "XP" ]
                        , span []
                          [ text "+50 XP" ]
                        ]
                      , div [ class "puzzle-progress-codingamers-solved" ]
                        [ span []
                          [ text "Completed by 227,075 CodinGamers" ]
                        ]
                      ]
                    ]
                  , div [ class "puzzle-progress" ]
                    [ div [ class "puzzle-progress-bar" ]
                      [ div [ class "puzzle-progress-value" ]
                        [ span [ class "puzzle-progress-score" ]
                          [ text "0" ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            , div [ class "puzzle-footer-details" ]
              [ div [ class "footer-content" ]
                [ a [ class "footer-topic", href ("/training?uuid="++x.puzzleUuid) ]
                  [ text x.puzzleTitle ]
                ]
              , div [ class "footer-rating" ]
                  [ ul [ class "footer-stars" ]
                    [ li [ class "footer-star-item" ]
                      []
                    , li [ class "footer-star-item" ]
                      []
                    , li [ class "footer-star-item" ]
                      []
                    , li [ class "footer-star-item" ]
                      []
                    , li [ class "footer-star-item rate-off" ]
                      []
                    ]
                  ]
              ]
            ]
          ]
        ]
  ) model.puzzles


view : Model -> Html Msg
view model =
  div [] [
    nav [ id "navigation" ]
      [ div [ class "navigation_nav-container" ]
        [ div [ class "navigation-desktop" ]
          [ a [ href "/home", class "navigation-logo" ]
            []
          , div [ class "navigation-desktop_nav-tabs" ]
            [ a [ href "/practice", class "navigation-desktop_nav-tab navigation-desktop_selected" ]
              [ span []
                [ text "Practice" ]
              ]
              -- fixme: 这里暂时先隐藏，暂时不需要那么多菜单
            -- , a [ href "/training", class "navigation-desktop_nav-tab" ]
            --   [ span []
            --     [ text "Compete" ]
            --   ]
            -- , a [ href "/training", class "navigation-desktop_nav-tab" ]
            --   [ span []
            --     [ text "Contribute" ]
            --   ]
            -- , a [ href "/training", class "navigation-desktop_nav-tab" ]
            --   [ span []
            --     [ text "Learn" ]
            --   ]
            ]
          , div [ class "navigation_nav-space" ]
            []
          , div [ class "navigation-notifications" ]
            [ div [ class "navigation-notifications_nav-notifications-wrapper" ]
              [ div [ class "navigation-notifications_nav-notifications-icon" ]
                []
              ]
            , div [ class "navigation-notifications_nav-notifications-panel angular-animate" ]
              [ div [ class "navigation-notifications_nav-top-overlay" ]
                []
              , div [ class "cg-notifications theme-black" ]
                [ div [ class "notifications-header" ]
                  [ h2 [ class "notifications-title" ]
                    [ text "Notifications" ]
                  ]
                , div [ class "notifications-list" ]
                  [ div [ class "notification-no-notif-container" ]
                    [ div [ class "notification-no-notif-icon-container" ]
                      [ div [ class "notification-no-notif-icon" ]
                        []
                      ]
                    , div [ class "angular-animate notification-placeholder" ]
                      [ text "No more new notifications" ]
                    ]
                  ]
                , button [ class "notifications-footer-button show-read" ]
                  [ text "Show read" ]
                ]
              ]
            ]
          , div [ class "navigation-profile" ]
            [ div [ class "navigation-profile_nav-profile-avatar" ]
              []
            , div [ class "navigation-profile_nav-profile-nickname nav-profile-nickname-add" ]
              [ text "Anonymous" ]
            , div [ class "navigation-profile_nav-profile-dropdown-container" ]
              [ div [ class "navigation-profile_nav-profile-dropdown-icon " ]
                []
              , div [ class "navigation-profile_nav-profile-dropdown angular-animate" ]
                [ div [ class "navigation-profile_nav-top-overlay" ]
                  []
                , div [ class "navigation-profile_nav-profile-dropdown-level-wrapper" ]
                  [ div [ class "navigation-profile_nav-profile-level-container" ]
                    [ div [ class "navigation-profile_nav-profile-level-icon xp-level-10" ]
                      []
                    , div [ class "navigation-profile_nav-profile-level" ]
                      [ text "Level 3" ]
                    ]
                  , div [ class "navigation-profile_nav-profile-xp-container" ]
                    [ div [ class "navigation-profile_nav-profile-xp" ]
                      [ text "1 / 80 XP" ]
                    ]
                  , div [ class "navigation-profile_nav-profile-progress-container" ]
                    [ div [ class "navigation-profile_nav-profile-progress" ]
                      [ div [ class "navigation-profile_nav-profile-progress-total-xp" ]
                        []
                      , div [ class "navigation-profile_nav-profile-progress-xp" ]
                        []
                      ]
                    ]
                  ]
                , div [ class "navigation-profile_nav-profile-dropdown-items" ]
                  [ a [ class "navigation-profile_nav-profile-dropdown-link navigation_friends" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon navigation-icon_friends" ]
                        []
                      ]
                    , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                      [ text "Friends" ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link  navigation-profile_separator" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon" ]
                        []
                      , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                        []
                      ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link", href "/home" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon navigation-icon_profile" ]
                        []
                      ]
                    , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                      [ text "Home" ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link", href "/profile" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon navigation-icon_home" ]
                        []
                      ]
                    , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                      [ text "My profile" ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link", href "/settings" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon navigation-icon_settings" ]
                        []
                      ]
                    , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                      [ text "Settings" ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link  navigation-profile_separator" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon" ]
                        []
                      , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                        []
                      ]
                    ]
                  , a [ class "navigation-profile_nav-profile-dropdown-link" ]
                    [ div [ class "navigation-profile_nav-profile-dropdown-item-icon-container" ]
                      [ div [ class "navigation-profile_nav-profile-dropdown-item-icon navigation-icon_sign_out" ]
                        []
                      ]
                    , div [ class "navigation-profile_nav-profile-dropdown-item-label" ]
                      [ text "Sign out" ]
                    ]
                  ]
                ]
              ]
            ]
          , div [ class "navigation-additional-links" ]
            [ div [ class "cg-menu-actions theme-black" ]
              [ div [ class "menu-actions" ]
                [ div [ class "icon-actions white" ]
                  []
                ]
              , div [ class "list-actions" ]
                [ a [ href "/blog" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_blog" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Blog" ]
                  ]
                , a [ href "/forum" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_forum" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Forum" ]
                  ]
                , a [ href "/about" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_about_us" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "About Us" ]
                  ]
                , a [ href "//work/?cg_referrer=%24direct" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_company" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Company" ]
                  ]
                , a [ href "/faq" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_faq" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Faq" ]
                  ]
                , a [ class "separator cg-menu-action-item-5" ]
                  [ span [ class "action-title" ]
                    []
                  ]
                , a [ href "https://www.facebook.com/CodinGame" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_facebook" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Facebook" ]
                  ]
                , a [ href "https://twitter.com/codingame" ]
                  [ span [ class "action-icon-container" ]
                    [ span [ class "action-icon navigation-icon_twitter" ]
                      []
                    ]
                  , span [ class "action-title" ]
                    [ text "Twitter" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ,div [ id "content" ]
      [ div [ class "column-contents" ]
        [ div [ class "left-column" ]
          [ div [ id "scrollable-pane" ]
            [ div [ class "contentView" ]
              [ div [ id "games" ]
                [ div [ class "games-level-header" ]
                  [ div [ class "games-level-header_content" ]
                    [ ul [ id "content-details-breadcrumb" ]
                      [ li []
                        [ a [ href "/training" ]
                          [ span []
                            [ text "Practice" ]
                          ]
                        ]
                      ]
                    , h1 [ class "games-level-header_title" ]
                      [ text "Classic Puzzle - Easy" ]
                    ]
                  ]
                , div [ class "games_wrapper" ]
                  [ div [ class "container" ]
                    [ div [ class "content large-section" ]
                      [ div [ class "cg-puzzles-section" ]
                        [ div [ class "puzzle-header" ]
                          [ div [ class "puzzle-title-header" ]
                            [ a [ class "puzzle-title" ]
                              [ text "To start" ]
                            ]
                          ]
                        , div [ class "puzzles large" ]
                            (getPanel model)
                        , div [ class "puzzles secondline" ]
                          (getPanel model)
                        ]
                      , div [ class "cg-puzzles-section" ]
                        [ div [ class "puzzle-header" ]
                          [ div [ class "puzzle-title-header" ]
                            [ a [ class "puzzle-title" ]
                              [ text "Completed" ]
                            ]
                          ]
                        , div [ class "puzzles secondline" ]
                          (getPanel model)
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
  ]