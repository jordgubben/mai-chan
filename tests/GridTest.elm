module GridTest exposing (..)

import Html
import Grid exposing (Grid)
import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag)


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
