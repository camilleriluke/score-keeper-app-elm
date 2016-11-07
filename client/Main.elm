module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Debug exposing (log)
import List exposing (length)
import List.Extra exposing (replaceIf, find)
import String exposing (isEmpty, trim)


type Msg
    = Edit PlayerId
    | Score PlayerId Int
    | Input String
    | Save
    | Cancel
    | DeletePlay


type alias Model =
    { lastPlayerId : PlayerId
    , players : List Player
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
    { lastPlayerId = 0
    , players = []
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


actionSave : Model -> Model
actionSave model =
    let
        nextPlayerId =
            model.lastPlayerId + 1
    in
        case ( model.editPlayerName, model.editPlayerId ) of
            ( Just name, Just id ) ->
                let
                    maybePlayer =
                        find (\p -> p.id == id) model.players
                in
                    case maybePlayer of
                        Just player ->
                            let
                                updatedPlayer =
                                    { player | name = name }
                            in
                                { model
                                    | players = (replaceIf (\player -> player.id == id) updatedPlayer model.players)
                                    , editPlayerId = Nothing
                                    , editPlayerName = Nothing
                                }

                        _ ->
                            model

            ( Just name, _ ) ->
                { model
                    | players =
                        { id = nextPlayerId
                        , name = name
                        , points = 0
                        }
                            :: model.players
                    , lastPlayerId = nextPlayerId
                    , editPlayerId = Nothing
                    , editPlayerName = Nothing
                }

            _ ->
                model


actionScore : PlayerId -> Int -> Model -> Model
actionScore playerId points model =
    let
        maybePlayer =
            find (\p -> p.id == playerId) model.players
    in
        case maybePlayer of
            Just player ->
                let
                    updatedPlayer =
                        { player
                            | points = player.points + points
                        }
                in
                    { model
                        | players = (replaceIf (\player -> player.id == playerId) updatedPlayer model.players)
                    }

            _ ->
                model


actionEdit : PlayerId -> Model -> Model
actionEdit playerId model =
    let
        maybePlayer =
            find (\p -> p.id == playerId) model.players
    in
        case maybePlayer of
            Just p ->
                { model
                    | editPlayerName = Just p.name
                    , editPlayerId = Just p.id
                }

            _ ->
                model


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

            Save ->
                if ((Maybe.withDefault "" model.editPlayerName) |> trim |> isEmpty) then
                    { model
                        | editPlayerId = Nothing
                        , editPlayerName = Nothing
                    }
                else
                    actionSave model

            Edit playerId ->
                actionEdit playerId model

            Score playerId score ->
                actionScore playerId score model

            _ ->
                model


renderHeader : Model -> Html Msg
renderHeader model =
    h1 [] [ text "Score Keeper" ]


renderPlayer : Model -> Player -> Html Msg
renderPlayer model player =
    let
        selectedClass : String
        selectedClass =
            case model.editPlayerId of
                Just id ->
                    if (player.id == id) then
                        "pure-button-selected"
                    else
                        ""

                _ ->
                    ""
    in
        li [ class selectedClass ]
            [ i [ class "fa-edit", onClick (Edit player.id) ] []
            , text player.name
            , button [ class "pure-button", onClick (Score player.id 2) ] [ text "2pt" ]
            , button [ class "pure-button", onClick (Score player.id 3) ] [ text "3pt" ]
            , text (toString player.points)
            ]


renderPlayerSection : Model -> Html Msg
renderPlayerSection model =
    section [ class "players-section" ]
        [ div []
            [ h2 [] [ text "Players section header" ]
            , ul [] (List.map (renderPlayer model) model.players)
            , div []
                [ strong []
                    [ text
                        ("Total: "
                            ++ (toString
                                    ((List.map (\p -> p.points) model.players)
                                        |> List.sum
                                    )
                               )
                        )
                    ]
                ]
            ]
        ]


renderPlayerForm : Model -> Html Msg
renderPlayerForm model =
    section [ class "player-form" ]
        [ div []
            [ Html.form [ onSubmit Save, class "pure-form" ]
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
