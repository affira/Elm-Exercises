module Helper exposing (..)


add : number -> number -> number
add =
    (+)


pr : { name : a, id : b } -> a
pr a =
    a.name


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type Msg
    = SetName String
    | AddPlayer
    | ModifyPlayer Int Bool
    | DeletePlayer Int


testModel : { players : List Player, newPlayer : Player }
testModel =
    { players = []
    , newPlayer = Player 0 "" False
    }


testFalsePlayer : Player
testFalsePlayer =
    Player 0 "Paul walker" False


testTruePlayer : Player
testTruePlayer =
    Player 1 "James Runn" True


testFalsePlayers : List Player
testFalsePlayers =
    [ Player 0 "Paul walker" False, Player 1 "Jean Adama" False, Player 2 "Coasta Pettri" False, Player 3 "Simeone Ignite" False ]
