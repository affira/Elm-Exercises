-- Fetch players from end point on load
-- Update the id from the fetched players
-- Add player to the end of the list


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id, type_, class, checked, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3)
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    , reqStatus : String
    }


type Msg
    = SetName String
    | ModifyPlayer Int Bool
    | AddPlayer
    | DeletePlayer Int
    | FetchPlayers (Result Http.Error (List Player))


playerDecoder : Decoder Player
playerDecoder =
    map3 Player (field "id" Decode.int) (field "name" Decode.string) (field "isActive" Decode.bool)


playersDecoder : Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


fetchPlayers : String -> Cmd Msg
fetchPlayers url =
    Http.get
        { url = url
        , expect = Http.expectJson FetchPlayers playersDecoder
        }




listLast : List a -> Maybe a
listLast list =
    List.head <| List.reverse list


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


init : () -> (Model, Cmd Msg)
init _ =
    ( { players = []
      , newPlayer = initPlayer 0
      , reqStatus = "Loading..."
      }
    , fetchPlayers "http://localhost:3001/api/players/"
    )

updatePlayerName : String -> Player -> Player
updatePlayerName newName player =
    { player | name = newName }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetName newName ->
            let
                updatedNewPlayer = updatePlayerName newName model.newPlayer
            in
            ({ model | newPlayer = updatedNewPlayer }, Cmd.none)

        AddPlayer ->
            let
                newId = 1 + Maybe.withDefault 0 (List.maximum (List.map .id model.players))
                newPlayer = { id = newId, name = model.newPlayer.name, isActive = False }
                updatedPlayers = model.players ++ [newPlayer]
            in
            -- Clear the input field by resetting newPlayer and update the players list.
            ({ model | players = updatedPlayers, newPlayer = initPlayer (newId + 1) }, Cmd.none)


        DeletePlayer playerId ->
            ({ model | players = List.filter (\player -> player.id /= playerId) model.players }, Cmd.none)


        ModifyPlayer playerId newStatus ->
           let
                updatePlayerStatus player =
                    if player.id == playerId then
                        { player | isActive = newStatus }
                    else
                        player
            in
            ({ model | players = List.map updatePlayerStatus model.players }, Cmd.none)

        FetchPlayers (Ok players) ->
            ( { model | players = players, reqStatus = "" }, Cmd.none )

        FetchPlayers (Err _) ->
            ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )



view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Exercise: Players CRUD" ]
        , form [ id "submit-player", onSubmit AddPlayer ]
            [ input [ id "input-player", type_ "text", onInput SetName, value model.newPlayer.name ] []
            , button [ id "btn-add", type_ "submit" ] [ text "Add Player" ]
            ]
        , ol [ id "players-list" ] (List.map viewPlayer model.players)
        , div [ id "request-status" ] [ text model.reqStatus ]
    ]

viewPlayer : Player -> Html Msg
viewPlayer player =
    li [ id <| "player-" ++ String.fromInt player.id ]
        [ div [ class "player-name" ] [ text player.name ]
        , label [ class "player-status" ]
            [ input [ type_ "checkbox", class "player-status", checked player.isActive, onInput (\_ -> ModifyPlayer player.id True) ] []
            , text "Active"
            ]
        , button [ class "btn-delete", onClick (DeletePlayer player.id) ] [ text "Delete" ]
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
