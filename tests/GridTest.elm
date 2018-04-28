module GridTest exposing (..)

import Html
import Grid exposing (Grid)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag)


-- # Test: Bounds


boundsSuite : Test
boundsSuite =
    describe "Bounds"
        [ describe "Grid.numCols"
            [ test "Can determine number of columns in the grid grid" <|
                (\() -> (Grid.numCols rectangularExampleGrid) |> Expect.equal 301)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.numCols Grid.empty
                        |> Expect.equal 0
                )
            ]
        , describe "Grid.numRows"
            [ test "Can determine number of rows in the grid" <|
                (\() -> (Grid.numRows rectangularExampleGrid) |> Expect.equal 31)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.numRows Grid.empty
                        |> Expect.equal 0
                )
            ]
        , describe
            "Grid.minX"
            [ test "Can determine the left most column of the grid" <|
                (\() -> (Grid.minX rectangularExampleGrid) |> Expect.equal -100)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.minX Grid.empty
                        |> Expect.equal 0
                )
            ]
        , describe "Grid.maxX"
            [ test "Can determine the right most column of the grid" <|
                (\() -> (Grid.maxX rectangularExampleGrid) |> Expect.equal 200)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.maxX Grid.empty
                        |> Expect.equal 0
                )
            ]
        , describe "Grid.minY"
            [ test "Can determine the lowest row of the grid" <|
                (\() -> (Grid.minY rectangularExampleGrid) |> Expect.equal -10)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.minY Grid.empty
                        |> Expect.equal 0
                )
            ]
        , describe "Grid.maxY"
            [ test "Can determine the gighest row of the grid" <|
                (\() -> (Grid.maxY rectangularExampleGrid) |> Expect.equal 20)
            , test "Defaults to 0 on an empty grid"
                (\() ->
                    Grid.maxY Grid.empty
                        |> Expect.equal 0
                )
            ]
        ]



-- # Test: Rendering


renderingSuite : Test
renderingSuite =
    describe "Grid rendering"
        [ describe "Grid.toHtmlTable"
            [ test "Creates a table spanning all the cells" <|
                \() ->
                    rectangularExampleGrid
                        |> Grid.toHtmlTable (\_ _ -> (Html.text ""))
                        |> expectCellCountEquals
                            ((Grid.numRows rectangularExampleGrid) * (Grid.numCols rectangularExampleGrid))
            , test "If the grid is empty, then the table is empty" <|
                \() ->
                    Grid.empty
                        |> Grid.toHtmlTable (\_ _ -> (Html.text ""))
                        |> expectCellCountEquals 0
            ]
        ]



-- # Helpers


rectangularExampleGrid : Grid String
rectangularExampleGrid =
    Grid.empty
        |> Grid.put ( 0, 0 ) "a"
        |> Grid.put ( 0, -10 ) "down"
        |> Grid.put ( 0, 20 ) "up"
        |> Grid.put ( -100, 0 ) "left"
        |> Grid.put ( 200, 0 ) "right"


expectCellCountEquals : Int -> Html.Html msg -> Expectation
expectCellCountEquals expNumCells html =
    html
        |> Query.fromHtml
        |> Query.findAll [ tag "td" ]
        |> Query.count
            (\actualNumCells ->
                Expect.equal expNumCells actualNumCells
            )
