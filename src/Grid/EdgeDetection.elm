module Grid.EdgeDetection exposing (Area, Coords, EdgeSet, TerrainPalette(..), addEdgeTile, bakeKey, edges, emptyPalette, getEdgeTile)

import Dict exposing (Dict)
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


type TerrainPalette t
    = TerrainPalette (Dict Int t)



-- # Terrain sculpting


emptyPalette : TerrainPalette a
emptyPalette =
    TerrainPalette Dict.empty


addEdgeTile : EdgeSet Bool -> t -> TerrainPalette t -> TerrainPalette t
addEdgeTile edgeSet tile (TerrainPalette dict) =
    TerrainPalette (Dict.insert (bakeKey edgeSet) tile dict)


getEdgeTile : EdgeSet Bool -> TerrainPalette t -> Maybe t
getEdgeTile edgeSet (TerrainPalette dict) =
    Dict.get (bakeKey edgeSet) dict


{-| Create a comparable out of an edge set, so that we can use it as a Dict key
-}
bakeKey : EdgeSet Bool -> Int
bakeKey edgeSet =
    let
        edgeValues =
            [ ( .top, 1 ), ( .bottom, 2 ), ( .left, 4 ), ( .right, 8 ) ]

        check ( hasEdge, val ) acc =
            if hasEdge edgeSet then
                val + acc

            else
                acc
    in
    edgeValues |> List.foldl check 0



-- # Edge detection


edges : Coords -> Area -> EdgeSet Bool
edges ( x, y ) area =
    { top = not (Set.member ( x, y + 1 ) area)
    , bottom = not (Set.member ( x, y - 1 ) area)
    , left = not (Set.member ( x - 1, y ) area)
    , right = not (Set.member ( x + 1, y ) area)
    }
