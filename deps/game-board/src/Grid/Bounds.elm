module Grid.Bounds exposing (Coords, Grid, maxX, maxY, minX, minY, numCols, numRows)

import Dict exposing (Dict)



-- # Types


type alias Coords =
    ( Int, Int )


type alias Grid a =
    Dict Coords a



-- # Functions


numRows : Grid a -> Int
numRows grid =
    if Dict.isEmpty grid then
        0

    else
        maxY grid - minY grid + 1


numCols : Grid a -> Int
numCols grid =
    if Dict.isEmpty grid then
        0

    else
        maxX grid - minX grid + 1


minX : Grid a -> Int
minX grid =
    grid |> Dict.keys |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0


maxX : Grid a -> Int
maxX grid =
    grid |> Dict.keys |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0


minY : Grid a -> Int
minY grid =
    grid |> Dict.keys |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0


maxY : Grid a -> Int
maxY grid =
    grid |> Dict.keys |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0
