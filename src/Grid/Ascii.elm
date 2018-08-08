module Grid.Ascii exposing (fromAscii)

import Dict exposing (Dict)
import Grid exposing (Grid)


type alias AsciiGrid =
    Dict ( Int, Int ) Char


fromAscii : ( Grid.Coords, String ) -> Grid Char
fromAscii ( ( offsetX, offsetY ), ascii ) =
    let
        strRows =
            String.split "\n" ascii
                |> List.map String.trim
                |> List.filter (String.isEmpty >> not)

        charCells : List (List Char)
        charCells =
            List.map String.toList strRows

        possitionedCells : List ( Grid.Coords, Char )
        possitionedCells =
            (List.map2
                (\y cells ->
                    List.map2 (\x cell -> ( ( offsetX + x, offsetY - y ), cell )) (List.range 0 (List.length cells)) cells
                )
                (List.range 0 (List.length charCells))
                charCells
            )
                |> List.concat
    in
        Grid.fromList possitionedCells
