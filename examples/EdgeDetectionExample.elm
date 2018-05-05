module EdgeDetectionExample exposing (..)

{-| Illustrate how edge detection can be used to outline areas on a grid
-}

import Set exposing (Set)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Grid
import Grid.EdgeDetection as EdgeDetection exposing (EdgeSet)


main : Html msg
main =
    let
        cross =
            [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( -1, 0 ), ( -2, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, -1 ), ( 0, -2 ) ]
                |> List.map (\c -> ( c, True ))
                |> Grid.fromList

        crossArea =
            Dict.keys cross |> Set.fromList

        crossWithEdedges =
            Dict.map (\coords _ -> EdgeDetection.edges coords crossArea) cross
    in
        Html.div []
            (List.map outerBox
                [ Grid.toHtmlDiv ( 32, 32 ) (\_ _ -> filler "darkblue") cross
                , Grid.toHtmlDiv ( 32, 32 )
                    (\_ edges -> edgeBox "darkgreen" "lightgreen" edges)
                    crossWithEdedges
                ]
            )


outerBox : Html msg -> Html msg
outerBox content =
    Html.div
        [ style
            [ ( "display", "inline-block" )
            , ( "margin", "20px" )
            , ( "border", "1px solid black" )
            ]
        ]
        [ content ]


filler : String -> Html msg
filler color =
    Html.div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", color )
            ]
        ]
        []


edgeBox : String -> String -> EdgeSet Bool -> Html msg
edgeBox edgeColor fillColor edges =
    let
        borderStyle edgeGetter =
            (if edgeGetter edges then
                edgeColor
             else
                fillColor
            )
    in
        Html.div
            [ style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                , ( "background-color", fillColor )
                , ( "box-sizing", "border-box" )
                , ( "border", "5px solid " ++ fillColor )
                , ( "border-left-color", borderStyle .left )
                , ( "border-right-color", borderStyle .right )
                , ( "border-top-color", borderStyle .top )
                , ( "border-bottom-color", borderStyle .bottom )
                ]
            ]
            []
