module Grid.MovementRange exposing (Coords, chart)

import Set exposing (Set)



-- # Types


type alias Coords =
    ( Int, Int )



-- # Charting


{-|

    Chart out all coordinates reachable
    from the given starting point
    in the given number of steps.

-}
chart : Set Coords -> Coords -> Int -> Set Coords
chart obstacles startingPoint steps =
    Set.diff
        (if steps <= 1 then
            Set.insert startingPoint (neighbours startingPoint)

         else
            chart obstacles startingPoint (steps - 1)
                |> Set.toList
                |> List.map (neighbours >> Set.toList)
                |> List.concat
                |> Set.fromList
        )
        obstacles


neighbours : Coords -> Set Coords
neighbours ( x, y ) =
    Set.fromList
        [ ( x + 1, y )
        , ( x - 1, y )
        , ( x, y + 1 )
        , ( x, y - 1 )
        ]
