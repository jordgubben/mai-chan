module GridTest exposing (..)

import Dict
import Html
import Grid exposing (Grid)
import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, class, style, all)


-- # Test: Rendering


renderingSuite : Test
renderingSuite =
    describe "Grid rendering"
        [ describe "Grid.toHtmlTable"
            [ test "Creates a table spanning all the cells" <|
                \() ->
                    exampleGrid
                        |> Grid.toHtmlTable (\_ _ -> (Html.text ""))
                        |> expectCellCountEquals
                            ((Grid.numRows exampleGrid) * (Grid.numCols exampleGrid))
            , test "If the grid is empty, then the table is empty" <|
                \() ->
                    Grid.empty
                        |> Grid.toHtmlTable (\_ _ -> (Html.text ""))
                        |> expectCellCountEquals 0
            ]
        , describe "Grid.toHtmlDiv"
            [ test "Creates a div containing a div for each grid tile " <|
                \() ->
                    exampleGrid
                        |> Grid.toHtmlDiv ( 16, 16 ) (\_ _ -> (Html.text ""))
                        |> expectTileDivCountEquals (Dict.size exampleGrid)
            , test "Creates an outer div large anough to house all tiles" <|
                \() ->
                    exampleGrid
                        |> Grid.toHtmlDiv ( 16, 16 ) (\_ _ -> (Html.text ""))
                        |> expectTileDivSize
                            ( 16 * Grid.numCols exampleGrid, 16 * Grid.numRows exampleGrid )
            , test "If the grid is empty, then there are no inner divs" <|
                \() ->
                    Grid.empty
                        |> Grid.toHtmlDiv ( 16, 16 ) (\_ _ -> (Html.text ""))
                        |> expectTileDivCountEquals 0
            ]
        ]



-- # Helpers


exampleGrid : Grid String
exampleGrid =
    Grid.empty
        |> Grid.put ( 0, 0 ) "a"
        |> Grid.put ( 0, -1 ) "down"
        |> Grid.put ( 0, 2 ) "up"
        |> Grid.put ( -3, 0 ) "left"
        |> Grid.put ( 4, 0 ) "right"


expectCellCountEquals : Int -> Html.Html msg -> Expectation
expectCellCountEquals expNumCells html =
    html
        |> Query.fromHtml
        |> Query.findAll [ tag "td" ]
        |> Query.count
            (\actualNumCells ->
                Expect.equal expNumCells actualNumCells
            )


expectTileDivCountEquals : Int -> Html.Html msg -> Expectation
expectTileDivCountEquals expNumCells html =
    html
        |> Query.fromHtml
        |> Query.children [ tag "div", class "grid-cell" ]
        |> Query.count
            (\actualNumCells ->
                Expect.equal expNumCells actualNumCells
            )


expectTileDivSize : ( Int, Int ) -> Html.Html msg -> Expectation
expectTileDivSize ( expWidth, expHeight ) html =
    html
        |> Query.fromHtml
        |> Query.has
            [ style
                [ ( "width", toString expWidth ++ "px" )
                , ( "height", toString expHeight ++ "px" )
                ]
            ]
