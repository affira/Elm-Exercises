module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Helper exposing (..)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "Player CRUD Tests"
        [ describe "Add Player"
            [ fuzz string
                "Should set new player value"
                (\playerName ->
                    let
                        newModel =
                            Main.update (Main.SetName playerName) Helper.testModel
                    in
                    Expect.equal playerName newModel.newPlayer.name
                )
            , fuzz string
                "Should add a new player correctly"
                (\playerName ->
                    let
                        expectPlayer =
                            { name = playerName, isActive = False }

                        newModel1 =
                            Main.update (Main.SetName playerName) Helper.testModel

                        newModel2 =
                            Main.update Main.AddPlayer newModel1
                    in
                    case List.head newModel2.players of
                        Just player ->
                            let
                                testPlayer =
                                    { name = player.name, isActive = player.isActive }
                            in
                            Expect.equal expectPlayer testPlayer

                        Nothing ->
                            Expect.fail "Player was not added"
                )
            , test "Should add several players"
                (\_ ->
                    let
                        expectPlayers =
                            List.map (\player -> { name = player.name, isActive = player.isActive }) Helper.testFalsePlayers

                        newModel =
                            List.foldl (\player model -> Main.update Main.AddPlayer <| Main.update (Main.SetName player.name) model) Helper.testModel expectPlayers

                        testPlayers =
                            List.map (\player -> { name = player.name, isActive = player.isActive }) newModel.players
                    in
                    if expectPlayers == testPlayers then
                        Expect.pass

                    else
                        Expect.fail "The added players did not match what was expected."
                )
            ]
        , describe "Modify player"
            [ test "Should modify single players"
                (\_ ->
                    let
                        expectPlayer =
                            { name = testTruePlayer.name, isActive = testTruePlayer.isActive }

                        newModel1 =
                            Main.update Main.AddPlayer <| Main.update (Main.SetName testTruePlayer.name) Helper.testModel
                    in
                    case List.head newModel1.players of
                        Just player ->
                            let
                                newModel2 =
                                    Main.update (Main.ModifyPlayer player.id (not player.isActive)) newModel1
                            in
                            case List.head newModel2.players of
                                Just modifiedPlayer ->
                                    Expect.equal expectPlayer { name = modifiedPlayer.name, isActive = modifiedPlayer.isActive }

                                Nothing ->
                                    Expect.fail "Player modification failed"

                        Nothing ->
                            Expect.fail "Player was not modified"
                )
            ]
        ]
