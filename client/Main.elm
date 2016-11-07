module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)


type Msg
    = Edit PlayerId
    | Score PlayerId Int
    | Input String
    | Save
    | Cancel
    | DeletePlay


type alias Model =
    { players : List Player
    , editPlayerName : Maybe String
    , editPlayerId : Maybe Int
    , plays : List Play
    , debug :
        Maybe { lastAction : String }
    }


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : PlayerId
    , name : String
    , points : Int
    }


initialModel : Model
initialModel =
    { players =
        [ { id = 0
          , name = "Lukis"
          , points = 90
          }
        ]
    , editPlayerName = Nothing
    , editPlayerId = Nothing
    , plays = []
    , debug = Nothing
    }


actionInput : String -> Model -> Model
actionInput value model =
    { model | editPlayerName = Just value }


actionCanel : Model -> Model
actionCanel model =
    { model
        | editPlayerName = Nothing
        , editPlayerId = Nothing
    }


update : Msg -> Model -> Model
update msg inputModel =
    let
        model =
            { inputModel | debug = Just { lastAction = (toString msg) } }
    in
        case msg of
            Input val ->
                actionInput val model

            Cancel ->
                actionCanel model

            _ ->
                model


renderHeader : Model -> Html Msg
renderHeader model =
    h1 [] [ text "Score Keeper" ]


renderPlayer : Player -> Html Msg
renderPlayer player =
    div []
        [ button [ onClick (Edit player.id) ] [ text "edit" ]
        , text player.name
        , button [ onClick (Score player.id 2) ] [ text "2pt" ]
        , button [ onClick (Score player.id 3) ] [ text "3pt" ]
        , text (toString player.points)
        ]


renderPlayerSection : Model -> Html Msg
renderPlayerSection model =
    section [ class "players-section" ]
        [ div []
            [ h2 [] [ text "Players section header" ]
            , div [] (List.map renderPlayer model.players)
            ]
        ]


renderPlayerForm : Model -> Html Msg
renderPlayerForm model =
    section [ class "player-form" ]
        [ div []
            [ Html.form [ onSubmit Save ]
                [ input
                    [ placeholder "Add/Edit Player..."
                    , type' "text"
                    , onInput Input
                    , value (withDefault "" model.editPlayerName)
                    ]
                    []
                , button [ type' "submit" ] [ text "Save" ]
                , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
                ]
            ]
        ]


renderPlaysSection : Model -> Html Msg
renderPlaysSection model =
    section [ class "plays-section" ]
        [ div []
            [ h2 [] [ text "Plays section" ]
            ]
        ]


renderDebug : Model -> Html Msg
renderDebug model =
    div []
        [ hr [] []
        , h3 []
            [ text "DEBUG"
            ]
        , code []
            [ text (toString model) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ renderHeader model
        , renderPlayerSection model
        , renderPlayerForm model
        , renderPlaysSection model
        , renderDebug model
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
