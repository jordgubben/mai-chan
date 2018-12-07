module Grid exposing (Grid, Coords, Size, empty, fromList, put, drawBox, lineRect, get, pickRect, numRows, numCols, translate, rotCv, rotCcv, swap, toHtmlTable, toHtmlDiv, toSvgGroup)

{-| Tile grid for (board game like) strategy games.


# Types

@docs Grid, Coords, Size


# Creation

@docs empty, fromList


# Drawing

@docs put, drawBox, lineRect


# Retrieval

@docs get, pickRect


# Analysis

@docs get, numRows, numCols


# Transformation

A Grid can be transformed in various ways.

@docs translate, rotCv, rotCcv, swap


# Rendering

@docs toHtmlTable, toHtmlDiv, toSvgGroup

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Svg
import Svg.Attributes as SvgAt
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

        cellStyle =
            style
                [ ( "font-size", "10px" )
                , ( "background-color", "lightblue" )
                ]

        outerDivBlockStyle =
            style
                [ ( "display", "block" )
                , ( "width", 300 ) |> px
                , ( "height", 400 ) |> px
                , ( "margin", 15 ) |> px
                , ( "border", "5px solid darkgray" )
                ]

        cellToHtml coords content =
            Html.div
                [ cellStyle
                ]
                [ (Html.em [] [ Html.text (toString coords) ])
                , (Html.p [] [ Html.text content ])
                ]

        cellToSvg coords content =
            Svg.g []
                [ Svg.circle [ SvgAt.cx "16", SvgAt.cy "16", SvgAt.r "16" ] []
                , Svg.text_ [] [ Svg.text ((toString coords) ++ content) ]
                ]
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Game board render test" ]
            , Html.div [ outerDivBlockStyle ]
                [ Html.h2 [] [ Html.text "Using <table>" ]
                , (grid |> toHtmlTable cellToHtml)
                ]
            , Html.div [ outerDivBlockStyle ]
                [ Html.h2 [] [ Html.text "Using <div>" ]
                , (grid |> toHtmlDiv ( 32, 32 ) cellToHtml)
                ]
            , Html.div [ outerDivBlockStyle ]
                [ Html.h2 [] [ Html.text "Using <svg>" ]
                , Svg.svg [ SvgAt.width "256", SvgAt.height "256", SvgAt.viewBox "-64 -96 256 256" ]
                    [ (grid |> toSvgGroup ( 32, 32 ) cellToSvg) ]
                ]
            ]



-- # Types


{-| Two-dimentional coordinates on the grid.
-}
type alias Coords =
    ( Int, Int )


{-|

    The shape of a rectangle.
-}
type alias Size n =
    { width : n
    , height : n
    }


{-| A Grid of cells.
-}
type alias Grid a =
    Dict Coords a



-- # Creation


{-| An empty Grid to start from.
-}
empty : Grid a
empty =
    Dict.empty


{-| Create a grid from a list of coordnates+content pairs
-}
fromList : List ( Coords, a ) -> Grid a
fromList =
    Dict.fromList



-- Drawing


{-| Draw a filled box that expands on positive axises from the origo (0,0).
-}
drawBox : t -> Size Int -> Grid t
drawBox content { width, height } =
    List.range 0 (width - 1)
        |> List.map (\x -> List.range 0 (height - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
        |> List.map (\c -> ( c, content ))
        |> fromList


{-| Draw edges of a box that expands on positive axises from the origo (0,0).
-}
lineRect : t -> Size Int -> Grid t
lineRect content { width, height } =
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
            |> List.map (\coords -> ( coords, content ))
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


{-| Make the content of two grid cells trade places
-}
swap : Coords -> Coords -> Grid a -> Grid a
swap c1 c2 grid =
    grid
        |> Dict.update c1 (always <| Dict.get c2 grid)
        |> Dict.update c2 (always <| Dict.get c1 grid)



-- # Editing


{-| Put content in a Grid cell at the given Coords.
-}
put : Coords -> a -> Grid a -> Grid a
put ( x, y ) cell =
    Dict.insert ( x, y ) cell



-- # Retrieval


{-| Ask for the content of a grid cell by position.
-}
get : Coords -> Grid a -> Maybe a
get =
    Dict.get


{-| Get a rectangle of cells expanding positive axises (up and right)
of from given coords.
-}
pickRect : Size Int -> Coords -> Grid a -> Grid a
pickRect { width, height } ( left, bottom ) srcGrid =
    let
        top =
            bottom + height - 1

        right =
            left + width - 1
    in
        Dict.filter
            (\( x, y ) _ ->
                (left <= x && x <= right) && (bottom <= y && y <= top)
            )
            srcGrid



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

Arguments are:

  - Grid cell size
  - Content render func
  - Grid to render

-}
toHtmlDiv : ( Int, Int ) -> (( Int, Int ) -> a -> Html msg) -> Grid a -> Html msg
toHtmlDiv ( cellWidth, cellHeight ) viewContent grid =
    let
        -- Outer div properties
        gridWidth =
            Bounds.numCols grid * cellWidth

        gridHeight =
            Bounds.numRows grid * cellHeight

        gridStyle =
            style
                [ ( "position", "relative" )
                , ( "width", gridWidth ) |> px
                , ( "height", gridHeight ) |> px
                ]

        -- Create inner div(s) for every occupied cell in grid
        cellDiv ( ( x, y ), content ) =
            let
                cellLeft =
                    (x - Bounds.minX grid) * cellWidth

                cellBottom =
                    (y - Bounds.minY grid) * cellHeight

                cellStyle =
                    style
                        [ ( "position", "absolute" )
                        , ( "width", cellWidth ) |> px
                        , ( "height", cellHeight ) |> px
                        , ( "bottom", cellBottom ) |> px
                        , ( "left", cellLeft ) |> px
                        , ( "overflow", "hidden" )
                        ]
            in
                Html.div
                    [ class "grid-cell"
                    , cellStyle
                    ]
                    [ viewContent ( x, y ) content ]
    in
        -- Wrap cells in a common outer div
        Html.div
            [ class "grid", gridStyle ]
            (Dict.toList grid |> List.map cellDiv)


{-| Render grid using SVG groups.

Arguments are:

  - Grid cell size
  - Content render func
  - Grid to render

(Note: Positive y-axis is up in this package, but down in SVG coordinates.
This compromise to keep the behaviour of the function as close as possible
to it's HTML siblings, while still maitaining unsuprising SVG behaviour
inside content rendering functions.)

-}
toSvgGroup : ( Int, Int ) -> (Coords -> a -> Svg.Svg msg) -> Grid a -> Svg.Svg msg
toSvgGroup ( cellWidth, cellHeight ) viewContent grid =
    let
        cellGroup : ( Coords, a ) -> Svg.Svg msg
        cellGroup ( ( x, y ), content ) =
            Svg.g
                [ SvgAt.transform ("translate(" ++ (toString (x * cellWidth)) ++ " " ++ (toString (-y * cellHeight)) ++ ")")
                ]
                [ viewContent ( x, y ) content ]
    in
        Svg.g [] (Dict.toList grid |> List.map cellGroup)


{-| Helper for css-style sizes and offsets in pixels.
-}
px : ( String, number ) -> ( String, String )
px ( magnitude, pixels ) =
    ( magnitude, (toString pixels) ++ "px" )
