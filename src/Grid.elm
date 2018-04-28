module Grid exposing (Grid, Coords, empty, put, get, numRows, numCols, toHtmlTable)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Grid.Bounds as Bounds exposing (minY, minX, maxY, maxX)


-- Example usage


main : Html msg
main =
    empty
        |> put ( 0, 0 ) "Origo"
        |> put ( 5, 3 ) "Upper right"
        |> put ( -2, 3 ) "Upper left"
        |> put ( 5, -4 ) "Lower left"
        |> put ( -2, -4 ) "Lower right"
        |> toHtmlTable
            (\coords content ->
                Html.div [ style [ ( "width", "64px" ), ( "height", "64px" ), ( "overflow", "hidden" ) ] ]
                    [ (Html.em [ style [ ( "font-size", "10px" ) ] ] [ Html.text (toString coords) ])
                    , (Html.p [] [ Html.text content ])
                    ]
            )



-- Types


type alias Coords =
    ( Int, Int )


type alias Grid a =
    Dict Coords a



-- # Editing


empty : Grid a
empty =
    Dict.empty


put : Coords -> a -> Grid a -> Grid a
put ( x, y ) cell =
    Dict.insert ( x, y ) cell



-- # Retrieval


get : Coords -> Grid a -> Maybe a
get =
    Dict.get



-- # Bounds


numRows : Grid a -> Int
numRows =
    Bounds.numRows


numCols : Grid a -> Int
numCols =
    Bounds.numCols



-- # Render


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
