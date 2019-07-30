module Grid.BoundsTest exposing (boundsSuite)

import Expect exposing (Expectation)
import Grid exposing (Grid)
import Grid.Bounds as Bounds
import Test exposing (..)


boundsSuite : Test
boundsSuite =
    describe "Bounds"
        [ describe "Grid.numCols"
            [ test "Can determine number of columns in the grid grid" <|
                \() -> Bounds.numCols rectangularExampleGrid |> Expect.equal 301
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.numCols emptyGrid
                        |> Expect.equal 0
                )
            ]
        , describe "Bounds.numRows"
            [ test "Can determine number of rows in the grid" <|
                \() -> Bounds.numRows rectangularExampleGrid |> Expect.equal 31
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.numRows emptyGrid
                        |> Expect.equal 0
                )
            ]
        , describe
            "Bounds.minX"
            [ test "Can determine the left most column of the grid" <|
                \() -> Bounds.minX rectangularExampleGrid |> Expect.equal -100
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.minX emptyGrid
                        |> Expect.equal 0
                )
            ]
        , describe "Bounds.maxX"
            [ test "Can determine the right most column of the grid" <|
                \() -> Bounds.maxX rectangularExampleGrid |> Expect.equal 200
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.maxX emptyGrid
                        |> Expect.equal 0
                )
            ]
        , describe "Bounds.minY"
            [ test "Can determine the lowest row of the grid" <|
                \() -> Bounds.minY rectangularExampleGrid |> Expect.equal -10
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.minY emptyGrid
                        |> Expect.equal 0
                )
            ]
        , describe "Bounds.maxY"
            [ test "Can determine the gighest row of the grid" <|
                \() -> Bounds.maxY rectangularExampleGrid |> Expect.equal 20
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Bounds.maxY emptyGrid
                        |> Expect.equal 0
                )
            ]
        ]



-- # Example data


emptyGrid : Grid a
emptyGrid =
    Grid.empty


rectangularExampleGrid : Grid String
rectangularExampleGrid =
    Grid.empty
        |> Grid.put ( 0, 0 ) "a"
        |> Grid.put ( 0, -10 ) "down"
        |> Grid.put ( 0, 20 ) "up"
        |> Grid.put ( -100, 0 ) "left"
        |> Grid.put ( 200, 0 ) "right"
