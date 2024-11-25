module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


initPlayer : Int -> Player
initPlayer id =
    Player id "" False

type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    }


type Msg
    = SetName String
    | AddPlayer
    | ModifyPlayer Int Bool
    | DeletePlayer Int


init : Model
init =
    { players = []
    , newPlayer = initPlayer 0
    }


updatePlayerName : String -> Player -> Player
updatePlayerName newName player =
    { player | name = newName }

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName newName ->
            let
                updatedNewPlayer = updatePlayerName newName model.newPlayer
            in
            { model | newPlayer = updatedNewPlayer }

        AddPlayer ->
            let
                newId = 1 + Maybe.withDefault 0 (List.maximum (List.map .id model.players))
                newPlayer = { id = newId, name = model.newPlayer.name, isActive = False }
                updatedPlayers = model.players ++ [newPlayer]
            in
            -- Clear the input field by resetting newPlayer and update the players list.
            { model | players = updatedPlayers, newPlayer = initPlayer (newId + 1) }

        ModifyPlayer playerId newStatus ->
            let
                updatePlayerStatus player =
                    if player.id == playerId then
                        { player | isActive = newStatus }
                    else
                        player
            in
            { model | players = List.map updatePlayerStatus model.players }

        DeletePlayer playerId ->
            { model | players = List.filter (\player -> player.id /= playerId) model.players }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Elm Exercise: Players CRUD" ]
        , Html.form [ id "submit-player", onSubmit AddPlayer ]
            [ input [ type_ "text", id "input-player", placeholder "Enter player's name", value model.newPlayer.name, onInput SetName ] []
            , button [ type_ "submit", id "btn-add" ] [ text "Add" ]
            ]
        , ol [ id "players-list" ] -- Changed from <ul> to <ol>
            (List.map viewPlayer model.players)
        ]

viewPlayer : Player -> Html Msg
viewPlayer player =
    li [ id ("player-" ++ String.fromInt player.id) ]
        [ div [ class "player-name" ] [ text player.name ]
        , label [ class "player-status" ]
            [ input [ type_ "checkbox", class "player-status", checked player.isActive, onCheck (\_ -> ModifyPlayer player.id (not player.isActive)) ] []
            , text "Active"
            ]
        , button [ class "btn-delete", onClick (DeletePlayer player.id) ] [ text "Delete" ]
        ]





main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
