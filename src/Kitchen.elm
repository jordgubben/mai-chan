module Kitchen exposing (level)

import Dict
import FloorTile exposing (FloorTile(..))
import Grid exposing (Grid, Size)
import Thingy exposing (Flavour(..), Thingy(..))


level =
    { floor = kitchenLevel
    , things = initialThings
    , size = boardSize
    }


initialThings : Grid Thingy
initialThings =
    Grid.empty


kitchenLevel : Grid FloorTile
kitchenLevel =
    kitchenFloor
        |> Dict.union kitchenSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


kitchenSpawners : Grid FloorTile
kitchenSpawners =
    Grid.drawBox (Spawner spawnables) { width = 6, height = 1 }
        |> Grid.translate ( 0, 0 )


{-| Spawnable things.
Weighted so that Flour, Water and Flavourings spawn 1/3 of the time each.
(Thus preventing an abundance of Flavourings)
-}
spawnables : List Thingy
spawnables =
    List.concat
        [ spawnableFlavours
        , List.repeat (List.length spawnableFlavours) (Water Nothing)
        , List.repeat (List.length spawnableFlavours) (Flour Nothing)
        ]


spawnableFlavours : List Thingy
spawnableFlavours =
    [ Flavouring { flavour = Sugar, packaged = False }
    , Flavouring { flavour = Chilli, packaged = False }
    , Flavouring { flavour = Chocolate, packaged = False }
    ]


kitchenCollectors : Grid FloorTile
kitchenCollectors =
    Grid.drawBox BunCollector { boardSize | height = 1 }
        |> Grid.translate ( 0, -5 )


kitchenFloor : Grid FloorTile
kitchenFloor =
    Grid.drawBox PlainTile { width = 6, height = 6 }
        |> Grid.translate ( 0, -5 )


kitchenWalls : Grid FloorTile
kitchenWalls =
    Grid.lineRect WallTile { width = 8, height = 8 }
        |> Grid.translate ( -1, -6 )


boardSize : Size Int
boardSize =
    Size 6 6
