module Grid exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style, class)


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
numRows grid =
    if Dict.isEmpty grid then
        0
    else
        (maxY grid) - (minY grid) + 1


numCols : Grid a -> Int
numCols grid =
    if Dict.isEmpty grid then
        0
    else
        (maxX grid) - (minX grid) + 1


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



-- # Render


toHtmlTable : (( Int, Int ) -> a -> Html msg) -> Grid a -> Html msg
toHtmlTable viewCell grid =
    if Dict.isEmpty grid then
        Html.table [ class "grid" ] []
    else
        Html.table [ class "grid" ]
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


cellTdStyle : Html.Attribute msg
cellTdStyle =
    style
        [ ( "width", "64px" )
        , ( "height", "64px" )
        , ( "border", "1px solid black" )
        ]
