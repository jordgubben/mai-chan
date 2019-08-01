module FloorTile exposing
    ( FloorTile(..)
    , isBunCollector
    , isObstacleTile
    , obstacleTileArea
    , spawnPoints
    )

import Dict
import Grid exposing (Coords, Grid)
import Set exposing (Set)
import Thingy exposing (Thingy)


{-| Background tile

The set of backgromund tiles are intended to vary fron level to level.

-}
type FloorTile
    = PlainTile
    | Spawner (List Thingy)
    | BunCollector
    | WallTile


spawnPoints : Grid FloorTile -> List ( Coords, List Thingy )
spawnPoints level =
    level
        |> Dict.toList
        |> List.filterMap
            (\( coords, tile ) ->
                case tile of
                    Spawner thing ->
                        Just ( coords, thing )

                    _ ->
                        Nothing
            )


{-| Filter out the Area where FloorTile are obstacles
-}
obstacleTileArea : Grid FloorTile -> Set Coords
obstacleTileArea =
    Dict.filter (always isObstacleTile) >> Dict.keys >> Set.fromList


isObstacleTile : FloorTile -> Bool
isObstacleTile tile =
    case tile of
        WallTile ->
            True

        _ ->
            False


isBunCollector : FloorTile -> Bool
isBunCollector floorTile =
    case floorTile of
        BunCollector ->
            True

        _ ->
            False
