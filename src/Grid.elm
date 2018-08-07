module Grid exposing (Grid, Coords, empty, fromList, put, drawBox, lineRect, get, numRows, numCols, translate, rotCv, rotCcv, toHtmlTable, toHtmlDiv)

{-| Tile grid for (board game like) strategy games.


# Types

@docs Coords, Grid


# Creation

@docs empty, fromList


# Drawing

@docs put, drawBox, lineRect


# Analysis

@docs get, numRows, numCols


# Transformation

A Grid can be transformed in various ways.

@docs translate, rotCv, rotCcv


# Rendering

@docs toHtmlTable, toHtmlDiv

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


type alias Size n =
    { width : n
    , height : n
    }


{-| A Grid of tiles.
-}
type alias Grid a =
    Dict Coords a



-- # Creation


{-| An empty Grid to start from.
-}
empty : Grid a
empty =
    Dict.empty


{-| Create a grid from a list of coordnates+tile pairs
-}
fromList : List ( Coords, a ) -> Grid a
fromList =
    Dict.fromList



-- Drawing


{-| Draw a filled box that expands on positive axises from the origo (0,0).
-}
drawBox : t -> Size Int -> Grid t
drawBox tile { width, height } =
    List.range 0 (width - 1)
        |> List.map (\x -> List.range 0 (height - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
        |> List.map (\c -> ( c, tile ))
        |> fromList


{-| Draw edges of a box that expands on positive axises from the origo (0,0).
-}
lineRect : t -> Size Int -> Grid t
lineRect tile { width, height } =
    let
        leftSide =
            List.range 0 (height - 1) |> List.map (\y -> ( 0, y ))

        rightSide =
            List.range 0 (height - 1) |> List.map (\y -> ( width - 1, y ))

        topSide =
            List.range 0 (width - 1) |> List.map (\x -> ( x, height - 1 ))

        bottomSide =
            List.range 0 (width - 1) |> List.map (\x -> ( x, 0 ))
    in
        List.concat [ leftSide, rightSide, topSide, bottomSide ]
            |> List.map (\coords -> ( coords, tile ))
            |> fromList



-- # Transformations


{-| Translate (move) all cells in a grid in the given direction.
-}
translate : ( Int, Int ) -> Grid a -> Grid a
translate ( dx, dy ) grid =
    Dict.toList grid
        |> List.map (Tuple.mapFirst (\( x, y ) -> ( x + dx, y + dy )))
        |> Dict.fromList


{-| Rotate grid 90 degrees clockwise.
-}
rotCv : Grid a -> Grid a
rotCv grid =
    Dict.toList grid
        |> List.map (Tuple.mapFirst (\( x, y ) -> ( y, -x )))
        |> Dict.fromList


{-| Rotate grid 90 degrees counter clockwise.
-}
rotCcv : Grid a -> Grid a
rotCcv grid =
    Dict.toList grid
        |> List.map (Tuple.mapFirst (\( x, y ) -> ( -y, x )))
        |> Dict.fromList



-- # Editing


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
        -- Outer div properties
        gridWidth =
            Bounds.numCols grid * tileWidth

        gridHeight =
            Bounds.numRows grid * tileHeight

        gridStyle =
            style
                [ ( "position", "relative" )
                , "width" % gridWidth
                , "height" % gridHeight
                ]

        -- Create inner div(s) for every tile in grid
        tileDiv ( ( x, y ), content ) =
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
    in
        -- Wrap tiles in a common outer div
        Html.div
            [ class "grid", gridStyle ]
            (Dict.toList grid |> List.map tileDiv)


{-|

    Local operator for css sizes and offsets in pixels
-}
(%) : String -> number -> ( String, String )
(%) magnitude pixels =
    ( magnitude, (toString pixels) ++ "px" )
