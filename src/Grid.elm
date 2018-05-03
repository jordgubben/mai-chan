module Grid exposing (Grid, Coords, empty, put, get, numRows, numCols, toHtmlTable)

{-| Tile grid for (board game like) strategy games.


# Types

@docs Coords, Grid


# Creation

@docs empty, put


# Analysis

@docs get, numRows, numCols


# Rendering

@docs toHtmlTable

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Grid.Bounds as Bounds exposing (minY, minX, maxY, maxX)


-- Example usage


main : Html msg
main =
    let
        grid =
            empty
                |> put ( 0, 0 ) "Origo"
                |> put ( 5, 3 ) "Upper right"
                |> put ( -2, 3 ) "Upper left"
                |> put ( 5, -4 ) "Lower right"
                |> put ( -2, -4 ) "Lower left"

        cell coords content =
            Html.div
                [ style
                    [ ( "font-size", "10px" )
                    , ( "background-color", "lightblue" )
                    ]
                ]
                [ (Html.em [] [ Html.text (toString coords) ])
                , (Html.p [] [ Html.text content ])
                ]
    in
        Html.div []
            [ (grid |> toHtmlTable cell)
            , Html.p [] [ Html.text "---" ]
            , (grid |> toHtmlDiv ( 32, 32 ) cell)
            ]



-- # Types


{-| Two-dimentional coordinates on the grid.
-}
type alias Coords =
    ( Int, Int )


{-| A Grid of tiles.
-}
type alias Grid a =
    Dict Coords a



-- # Editing


{-| An empty Grid to start from.
-}
empty : Grid a
empty =
    Dict.empty


{-| Place a tile in the Grid.
-}
put : Coords -> a -> Grid a -> Grid a
put ( x, y ) cell =
    Dict.insert ( x, y ) cell



-- # Retrieval


{-| Get a tile from a grid by position.
-}
get : Coords -> Grid a -> Maybe a
get =
    Dict.get



-- # Bounds


{-| Number of grid rows.
-}
numRows : Grid a -> Int
numRows =
    Bounds.numRows


{-| Number of grid columns.
-}
numCols : Grid a -> Int
numCols =
    Bounds.numCols



-- # Render


{-| Render grid as a HTML table.
-}
toHtmlTable : (( Int, Int ) -> a -> Html msg) -> Grid a -> Html msg
toHtmlTable viewCell grid =
    if Dict.isEmpty grid then
        Html.table [ class "grid" ] []
    else
        Html.table [ class "grid", gridTableStyle ]
            (List.range (minY grid) (maxY grid)
                |> List.reverse
                |> List.map (\r -> toHtmlRow viewCell r grid)
            )


toHtmlRow : (( Int, Int ) -> a -> Html msg) -> Int -> Grid a -> Html msg
toHtmlRow viewCell row grid =
    Html.tr [ class "grid-row" ]
        (List.range (minX grid) (maxX grid)
            |> List.map
                (\column ->
                    Html.td [ cellTdStyle, class "grid-cell" ]
                        (let
                            coords =
                                ( column, row )

                            possibleCell =
                                Dict.get coords grid
                         in
                            possibleCell
                                |> Maybe.map (\cell -> [ viewCell coords cell ])
                                |> Maybe.withDefault []
                        )
                )
        )


gridTableStyle : Html.Attribute msg
gridTableStyle =
    style
        [ ( "border-collapse", "collapse" )
        ]


cellTdStyle : Html.Attribute msg
cellTdStyle =
    style
        [ ( "width", "32px" )
        , ( "height", "32px" )
        , ( "padding", "0" )
        , ( "border", "1px solid black" )
        ]


{-| Render grid using HTML divs.

Parameters are:

  - Tile size
  - Content render func
  - Grid to render

-}
toHtmlDiv : ( Int, Int ) -> (( Int, Int ) -> a -> Html msg) -> Grid a -> Html msg
toHtmlDiv ( tileWidth, tileHeight ) viewTile grid =
    let
        gridWidth =
            Bounds.numCols grid * tileWidth

        gridHeight =
            Bounds.numRows grid * tileHeight
    in
        Html.div
            [ class "grid"
            , style
                [ ( "position", "relative" )
                , "width" % gridWidth
                , "height" % gridHeight
                ]
            ]
            (Dict.toList grid
                |> List.map
                    (\( ( x, y ), content ) ->
                        let
                            tileLeft =
                                (x - Bounds.minX grid) * tileWidth

                            tileBottom =
                                (y - Bounds.minY grid) * tileHeight
                        in
                            Html.div
                                [ class "grid-cell"
                                , style
                                    [ ( "position", "absolute" )
                                    , "width" % tileWidth
                                    , "height" % tileHeight
                                    , "bottom" % tileBottom
                                    , "left" % tileLeft
                                    , ( "overflow", "hidden" )
                                    ]
                                ]
                                [ viewTile ( x, y ) content ]
                    )
            )


{-|

    Local operator for css sizes and offsets in pixels
-}
(%) : String -> number -> ( String, String )
(%) magnitude pixels =
    ( magnitude, (toString pixels) ++ "px" )
