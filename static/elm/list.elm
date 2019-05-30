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
    ,epuzzles:Puzzles
    ,mpuzzles:Puzzles
    ,hpuzzles:Puzzles
    ,ppuzzles:Puzzles
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
        ,epuzzles=[]
        ,mpuzzles=[]
        ,hpuzzles=[]
        ,ppuzzles=[]
        },
        getAllPuzzles
      )

getAllPuzzles:Cmd Msg
getAllPuzzles =
  Http.get
    {
      url="/allpuzzles"
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
                  let
                    easy=List.filter (\x->x.puzzleCategory=="Easy") output
                    medium=List.filter (\x->x.puzzleCategory=="Medium") output
                    hard=List.filter (\x->x.puzzleCategory=="Hard") output
                    profession=List.filter (\x->x.puzzleCategory=="Professional") output
                  in
                    Debug.log "puzzles" ({model|loadState=Success,epuzzles=easy,mpuzzles=medium,hpuzzles=hard,ppuzzles=profession},Cmd.none)
                Err _ ->
                -- 解析错误，后台返回的不是puzzle的list，而只是单纯的错误信息
                  Debug.log "error" ({model|loadState=ParseError},Cmd.none)
          Err _ ->
            Debug.log "hh" ({model|loadState=Fail},Cmd.none)

type alias Puzzle=
  {
    puzzleUuid:String,
    puzzleTitle:String,
    puzzleCategory:String
  }

type alias Puzzles=List Puzzle

puzzlesDecoder : Decode.Decoder (List Puzzle)
puzzlesDecoder =
    Decode.list puzzleDecoder

puzzleDecoder : Decode.Decoder Puzzle
puzzleDecoder =
    map3 Puzzle
        (field "puzzleUuid" string)
        (field "puzzleTitle" string)
        (field "puzzleCategory" string)


view : Model -> Html Msg
view model =
  div []
  [
    nav [ id "navigation" ]
      [ div [ class "navigation_nav-container" ]
        [ div [ class "navigation-desktop" ]
          [ a [ href "/list", class "navigation-logo" ]
            []
          , div [ class "navigation-desktop_nav-tabs" ]
            [ a [ href "/list", class "navigation-desktop_nav-tab navigation-desktop_selected" ]
              [ span []
                [ text "Practice" ]
              ]
            ]
          , div [ class "navigation_nav-space" ]
            []
          ]
        ]
      ]
    ,div [ id "content" ]
      [ div [ class "column-contents" ]
        [ div [ class "left-column" ]
          [ div [ id "scrollable-pane" ]
            [ div [ class "contentView" ]
              [ div [ id "games" ]
                [ div [ class "cg-navigation-subtabs" ]
                  [ div [ class "cg-navigation-subtabs_content" ]
                    [ div [ class "cg-subtabs" ]
                      [ a [ href "/list", class "subtab selected" ]
                        [ span []
                          [ text "Puzzles" ]
                        , div [ class "hover-bar" ]
                          []
                        ]
                      ]
                    ]
                  ]
                , div [ class "games_wrapper" ]
                  [ div [ class "container" ]
                    [
                      div [ class "content large-section" ]
                      [
                        div [ class "cg-puzzles-section" ]
                        [
                        ]
                      , div [ class "cg-puzzles-section" ]
                        [ div [ class "puzzle-header" ]
                          [ div [ class "puzzle-title-header" ]
                            [ div []
                              [ a [ class "puzzle-title" ]
                                [ text "Classic Puzzle - Easy" ]
                              , div [ class "sub-title" ]
                                [ text "Improve your skills by solving algorithmic puzzles" ]
                              ]
                            -- , div [ class "level-progress-value" ]
                            --   [ text "4% completed" ]
                            ]
                          ]
                        , div [ class "puzzles" ]
                          (getPanel model.epuzzles)
                        -- , a [ class "sub-pane", href "/easy" ]
                        --   [ span [ class "icon_view_all" ]
                        --     []
                        --   , span [ class "sub-pane-label" ]
                        --     [ text "View all" ]
                        --   ]
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

getPanel :Puzzles->List (Html Msg)
getPanel puzzles=
  List.map (\x->
    div [ class "level-puzzle" ]
        [ div [ class "cg-puzzle-tile large" ]
          [ div [ class "puzzle-wrapper" ]
            [ a [ class "puzzle-content puzzle-hover", href ("/training?uuid=" ++ x.puzzleUuid) ]
              [ img [ class "tile-image", src "/static/images/thor.jpg" ]
                []
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
  ) puzzles
