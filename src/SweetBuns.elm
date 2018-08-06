module SweetBuns exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Grid exposing (..)


type alias Bun =
    String


type alias FullTile =
    String


main : Html msg
main =
    Html.div []
        [ Html.text "Hello"
        , Grid.toHtmlDiv ( tileSide, tileSide ) renderTile all
        ]


renderTile : a -> FullTile -> Html msg
renderTile coords tile =
    Html.div
        [ style
            [ ( "width", toString (tileSide) ++ "px" )
            , ( "height", toString (tileSide) ++ "px" )
            , ( "background-color", "lightgray" )
            , ( "border", "1px solid darkgray" )
            ]
        ]
        [ Html.text tile
        ]


tileSide : Int
tileSide =
    32


buns : Grid Bun
buns =
    Grid.fromList
        [ ( ( 0, 0 ), "🍪" )
        , ( ( 1, 0 ), "🍪" )
        , ( ( 2, 0 ), "🍪" )
        , ( ( 3, 0 ), "🍪" )
        , ( ( 4, 0 ), "🍪" )
        , ( ( 5, 0 ), "🍪" )
        ]


back : Grid FullTile
back =
    Grid.drawBox " " { width = 6, height = 12 }
        |> Grid.translate ( 0, -12 )


all : Grid FullTile
all =
    Dict.union buns back
