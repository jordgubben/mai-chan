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
        -- Solo turf
        |> Grid.put ( 0, 0 ) turfSoloTile
        -- 3x3 block
        |> Grid.put ( 2, 5 ) turfEdgeTopLeft
        |> Grid.put ( 2, 4 ) turfEdgeLeft
        |> Grid.put ( 2, 3 ) turfEdgeBottomLeft
        |> Grid.put ( 3, 5 ) turfEdgeTop
        |> Grid.put ( 3, 4 ) turfNoEdges
        |> Grid.put ( 3, 3 ) turfEdgeBottom
        |> Grid.put ( 4, 5 ) turfEdgeTopRight
        |> Grid.put ( 4, 4 ) turfEdgeRight
        |> Grid.put ( 4, 3 ) turfEdgeBottomRight



-- # Turf tile


turfSoloTile : Tile
turfSoloTile =
    staticTile ( 2, 2 )


turfEdgeTopLeft : Tile
turfEdgeTopLeft =
    staticTile ( 0, 0 )


turfEdgeLeft : Tile
turfEdgeLeft =
    staticTile ( 0, 2 )


turfEdgeBottomLeft : Tile
turfEdgeBottomLeft =
    staticTile ( 0, 4 )


turfEdgeTop : Tile
turfEdgeTop =
    staticTile ( 4, 0 )


turfNoEdges : Tile
turfNoEdges =
    staticTile ( 4, 2 )


turfEdgeBottom : Tile
turfEdgeBottom =
    staticTile ( 4, 4 )


turfEdgeTopRight : Tile
turfEdgeTopRight =
    staticTile ( 5, 0 )


turfEdgeRight : Tile
turfEdgeRight =
    staticTile ( 5, 2 )


turfEdgeBottomRight : Tile
turfEdgeBottomRight =
    staticTile ( 5, 4 )



-- Pipes


turfEdgeTopBottom : Tile
turfEdgeTopBottom =
    staticTile ( 7, 0 )


turfEdgeLeftRight : Tile
turfEdgeLeftRight =
    staticTile ( 6, 1 )



-- Ends


turfEdgeTopBottomLeft : Tile
turfEdgeTopBottomLeft =
    staticTile ( 6, 5 )


turfEdgeTopBottomRight : Tile
turfEdgeTopBottomRight =
    staticTile ( 9, 6 )


turfEdgeTopLeftRight : Tile
turfEdgeTopLeftRight =
    staticTile ( 8, 4 )


turfEdgeBottomLeftRight : Tile
turfEdgeBottomLeftRight =
    staticTile ( 7, 7 )


{-| Pick one static tile from the tile map.
-}
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
