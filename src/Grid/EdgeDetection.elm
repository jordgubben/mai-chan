module Grid.EdgeDetection exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)


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


type TerrainPalette t
    = TerrainPalette (Dict String t)



-- # Terrain sculpting


emptyPalette : TerrainPalette a
emptyPalette =
    TerrainPalette Dict.empty


addEdgeTile : EdgeSet Bool -> t -> TerrainPalette t -> TerrainPalette t
addEdgeTile edgeSet tile (TerrainPalette dict) =
    TerrainPalette (Dict.insert (toString edgeSet) tile dict)


getEdgeTile : EdgeSet Bool -> TerrainPalette t -> Maybe t
getEdgeTile edgeSet (TerrainPalette dict) =
    Dict.get (toString edgeSet) dict



-- # Edge detection


edges : Coords -> Area -> EdgeSet Bool
edges ( x, y ) area =
    { top = not (Set.member ( x, y + 1 ) area)
    , bottom = not (Set.member ( x, y - 1 ) area)
    , left = not (Set.member ( x - 1, y ) area)
    , right = not (Set.member ( x + 1, y ) area)
    }
