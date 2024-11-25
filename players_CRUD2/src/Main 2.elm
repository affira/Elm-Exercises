-- Fetch players from backend on load
-- Delete player from backend first then delete player from frontend on success
-- modify player from backend first then modify player from frontend on success
-- modify player from backend first then modify player from frontend on success
-- add player to backend first then add player to frontend on success


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3)
import Json.Encode as Encode


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    , baseUrl : String
    , reqStatus : String
    }


type Msg
    = SetName String
    | FetchPlayers (Result Http.Error (List Player))
    | PutPlayerReq Int Bool
    | ModifyPlayer (Result Http.Error Player)
    | PostPlayerReq
    | AddPlayer (Result Http.Error Player)
    | DeletePlayerReq Int
    | DeletePlayer Int (Result Http.Error ())


playerEncoder : Player -> Encode.Value
playerEncoder player =
    Encode.object
        [ ( "id", Encode.int player.id )
        , ( "name", Encode.string player.name )
        , ( "isActive", Encode.bool player.isActive )
        ]


playerDecoder : Decoder Player
playerDecoder =
    map3 Player (field "id" Decode.int) (field "name" Decode.string) (field "isActive" Decode.bool)


playersDecoder : Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


-- HTTP request functions
-- HTTP request functions
fetchPlayers : String -> Cmd Msg
fetchPlayers url =
    Http.get
        { url = url
        , expect = Http.expectJson FetchPlayers playersDecoder
        }

postPlayerReq : String -> Player -> Cmd Msg
postPlayerReq url player =
    Http.post
        { url = url
        , body = Http.jsonBody (playerEncoder player)
        , expect = Http.expectJson AddPlayer playerDecoder
        }

deletePlayerReq : String -> Int -> Cmd Msg
deletePlayerReq url id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (\_ -> DeletePlayer id (Ok ()))
        , timeout = Nothing
        , tracker = Nothing
        }

putPlayerReq : String -> Player -> Cmd Msg
putPlayerReq url player =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url ++ String.fromInt player.id
        , body = Http.jsonBody (playerEncoder player)
        , expect = Http.expectJson ModifyPlayer playerDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


listLast : List a -> Maybe a
listLast list =
    List.head <| List.reverse list


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


initModel : Model
initModel =
    { players = []
    , newPlayer = initPlayer 0
    , baseUrl = "http://localhost:3001/api/players/"
    , reqStatus = "Loading..."
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , fetchPlayers initModel.baseUrl
    )

-- Update player name function
updatePlayerName : String -> Player -> Player
updatePlayerName newName player =
    { player | name = newName }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName newName ->
            ( { model | newPlayer = updatePlayerName newName model.newPlayer }, Cmd.none )

        FetchPlayers (Ok newPlayers) ->
            let
                maxId = List.maximum (List.map .id newPlayers)
                nextId = Maybe.withDefault 0 maxId + 1
            in
            ( { model | players = newPlayers, newPlayer = initPlayer nextId, reqStatus = "" }, Cmd.none )

        FetchPlayers (Err _) ->
            ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )

        PostPlayerReq ->
            -- Call postPlayerReq function to send a new player data
            ( model, postPlayerReq model.baseUrl model.newPlayer )

        AddPlayer data ->
            -- Handle the result of adding a player
            case data of
                Ok newPlayer ->
                    ( { model | players = model.players ++ [newPlayer], newPlayer = initPlayer (newPlayer.id + 1) }, Cmd.none )
                Err _ ->
                    ( { model | reqStatus = "Failed to add player" }, Cmd.none )

        PutPlayerReq id status ->
            -- Find the player and call putPlayerReq to update their status
            case List.head (List.filter (\p -> p.id == id) model.players) of
                Just player ->
                    ( model, putPlayerReq model.baseUrl { player | isActive = status } )
                Nothing ->
                    ( model, Cmd.none )

        ModifyPlayer data ->
            -- Handle the result of modifying a player
            case data of
                Ok updatedPlayer ->
                    let updatedPlayers = List.map (\p -> if p.id == updatedPlayer.id then updatedPlayer else p) model.players
                    in ( { model | players = updatedPlayers }, Cmd.none )
                Err _ ->
                    ( { model | reqStatus = "Failed to update player" }, Cmd.none )

        DeletePlayerReq id ->
            -- Call deletePlayerReq to initiate player deletion
            ( model, deletePlayerReq model.baseUrl id )

        DeletePlayer id data ->
            -- Handle the result of deleting a player
            case data of
                Ok _ ->
                    let updatedPlayers = List.filter (\p -> p.id /= id) model.players
                    in ( { model | players = updatedPlayers }, Cmd.none )
                Err _ ->
                    ( { model | reqStatus = "Failed to delete player" }, Cmd.none )


-- View function
view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Elm Exercise: Players CRUD2" ]
        , Html.form [ id "submit-player", onSubmit PostPlayerReq ]
            [ input [ type_ "text", id "input-player", placeholder "Enter player's name", value model.newPlayer.name, onInput SetName ] []
            , button [ type_ "submit", id "btn-add" ] [ text "Add" ]
            ]
        , ol [ id "players-list" ]
            (List.map viewPlayer model.players)
        , div [ id "request-status" ]
            [ text model.reqStatus ]
        ]

viewPlayer : Player -> Html Msg
viewPlayer player =
    li [ id ("player-" ++ String.fromInt player.id) ]
        [ div [ class "player-name" ] [ text player.name ]
        , label [ class "player-status" ]
            [ input [ type_ "checkbox", class "player-status", checked player.isActive, onCheck (\_ -> PutPlayerReq player.id (not player.isActive)) ] []
            , text (if player.isActive then "Active" else "Inactive")
            ]
        , button [ class "btn-delete", onClick (DeletePlayerReq player.id) ] [ text "Delete" ]
        ]
        

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
