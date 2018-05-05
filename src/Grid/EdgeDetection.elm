module Grid.EdgeDetection exposing (..)

import Set exposing (Set)


-- Type(s)


type alias Coords =
    ( Int, Int )


type alias Area =
    Set Coords


type alias EdgeSet a =
    { top : a
    , bottom : a
    , left : a
    , right : a
    }



-- Functions


edges : Coords -> Area -> EdgeSet Bool
edges ( x, y ) area =
    { top = not (Set.member ( x, y + 1 ) area)
    , bottom = not (Set.member ( x, y - 1 ) area)
    , left = not (Set.member ( x - 1, y ) area)
    , right = not (Set.member ( x + 1, y ) area)
    }
