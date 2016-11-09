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


editPlayer : Model -> PlayerId -> String -> Model
editPlayer model id name =
    let
        maybePlayer =
            lookupPlayer model.players id
    in
        case maybePlayer of
            Just player ->
                replacePlayer (updatePlayerName player name) model

            Nothing ->
                model


addPlayer : Model -> String -> Model
addPlayer model name =
    let
        nextPlayerId =
            model.lastPlayerId + 1

        model =
            { model
                | lastPlayerId = nextPlayerId
            }

        newPlyer =
            { id = nextPlayerId
            , name = name
            , points = 0
            }
    in
        addPlayerToModel model newPlyer


actionSave : Model -> Model
actionSave model =
    case ( model.editPlayerName, model.editPlayerId ) of
        ( Just name, Just id ) ->
            editPlayer model id name

        ( Just name, _ ) ->
            addPlayer model name

        _ ->
            model


lookupPlayer : List Player -> PlayerId -> Maybe Player
lookupPlayer players playerId =
    find (\p -> p.id == playerId) players


addPlayerToModel : Model -> Player -> Model
addPlayerToModel model player =
    { model
        | players = player :: model.players
    }


fromPlayers : Model -> List Player
fromPlayers model =
    model.players


replacePlayer : Player -> Model -> Model
replacePlayer player model =
    { model
        | players = (replaceIf (\p -> p.id == player.id) player model.players)
    }


updatePlayerScore : Player -> Int -> Player
updatePlayerScore player points =
    { player
        | points = player.points + points
    }


updatePlayerName : Player -> String -> Player
updatePlayerName player name =
    { player
        | name = name
    }


actionScore : PlayerId -> Int -> Model -> Model
actionScore id points model =
    let
        maybePlayer =
            lookupPlayer model.players id
    in
        case maybePlayer of
            Just player ->
                replacePlayer (updatePlayerScore player points) model

            Nothing ->
                model


setEditingForPlayer : Model -> Player -> Model
setEditingForPlayer model player =
    { model
        | editPlayerName = Just player.name
        , editPlayerId = Just player.id
    }


actionEdit : PlayerId -> Model -> Model
actionEdit id model =
    let
        maybePlayer =
            lookupPlayer model.players id
    in
        case maybePlayer of
            Just player ->
                setEditingForPlayer model player

            Nothing ->
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
                let
                    editName =
                        (Maybe.withDefault "" model.editPlayerName)
                            |> trim

                    model =
                        if (not (isEmpty editName)) then
                            actionSave model
                        else
                            model
                in
                    model
                        -- This is to clear editing / saving state
                        |>
                            actionCanel

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


renderTotal : Int -> Html Msg
renderTotal total =
    text ("Total: " ++ (toString total))


sumTotal : List Player -> Int
sumTotal players =
    (List.map (\p -> p.points) players)
        |> List.sum


renderPlayerSection : Model -> Html Msg
renderPlayerSection model =
    section [ class "players-section" ]
        [ div []
            [ h2 [] [ text "Players section header" ]
            , ul [] (List.map (renderPlayer model) model.players)
            , div []
                [ strong []
                    [ renderTotal (sumTotal model.players) ]
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
