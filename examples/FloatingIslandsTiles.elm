module FloatingIslandsTiles exposing (..)

{-| Example displaying a tilemap

Tiles by: [Buch](https://opengameart.org/users/buch)
Tile source:
<https://opengameart.org/content/the-field-of-the-floating-islands>

-}

import Array
import Html exposing (Html)
import Html.Attributes exposing (style)
import Sprite
import Grid


main : Html msg
main =
    Html.div [ style [ ( "background-color", "black" ) ] ]
        [ Grid.toHtmlDiv ( 16, 16 ) renderTile grid
        ]



-- Tile rendering


type alias Tile =
    Sprite.Sprite {}


renderTile : a -> Tile -> Html msg
renderTile _ tile =
    Html.span [ style <| Sprite.sprite tile ] []



-- Example Grid


grid : Grid.Grid Tile
grid =
    Grid.empty
        |> Grid.put ( 0, 0 ) turfTile
        |> Grid.put ( 2, 4 ) (staticTile ( 0, 0 ))
        |> Grid.put ( 2, 3 ) (staticTile ( 0, 4 ))
        |> Grid.put ( 3, 4 ) (staticTile ( 5, 0 ))
        |> Grid.put ( 3, 3 ) (staticTile ( 5, 4 ))



-- Tile(s)


turfTile : Tile
turfTile =
    staticTile ( 2, 2 )


staticTile : ( Int, Int ) -> Tile
staticTile tileLoc =
    { baseTile
        | frame = 0
        , dope = Array.fromList [ tileLoc ]
    }


baseTile : Tile
baseTile =
    { sheet = "FloatingIslandsTiles.png"
    , rows = 8
    , columns = 23
    , size = ( 368, 128 )
    , frame = 0
    , dope = Array.empty
    }
