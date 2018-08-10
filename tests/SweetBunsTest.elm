module SweetBunsTest exposing (..)

import Random exposing (Seed)
import Set
import Dict
import Grid
import SweetBuns exposing (Thingy(..), FloorTile(..))
import Test exposing (..)
import Fuzz exposing (..)
import Expect exposing (Expectation, equal, fail)


spawnSuite : Test
spawnSuite =
    describe "Spawning pattern"
        [ test "Buns can spawn from all  spawn tiles" <|
            \() ->
                let
                    -- Given a kitchen with one spawn poin
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), (Spawner bun) )
                            ]

                    -- When spawning
                    spawnedThings : Grid.Grid Thingy
                    spawnedThings =
                        SweetBuns.spawnThingEverywhere initialKitchen

                    -- Bun appears on spawn tile
                    expectedThings : Grid.Grid Thingy
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), bun )
                            ]
                in
                    equal spawnedThings
                        expectedThings
        , fuzz int "Buns can spawn from a random spawner" <|
            \s ->
                let
                    seed =
                        Random.initialSeed s

                    -- Given a kitchen with several spawn points
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), (Spawner bun) )
                            , ( ( 0, 2 ), (Spawner bun) )
                            , ( ( 0, 3 ), (Spawner bun) )
                            ]

                    -- When spawning
                    ( spawnedThings, _ ) =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen
                in
                    spawnedThings
                        |> Dict.keys
                        |> List.filter
                            (\c ->
                                (Grid.get c initialKitchen) == Just (Spawner bun)
                            )
                        |> List.length
                        |> equal 1
        , fuzz int "Buns can their defined thing" <|
            \s ->
                let
                    seed =
                        Random.initialSeed s

                    -- Given a kitchen with several spawn points spawning different things
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), (Spawner Water) )
                            , ( ( 0, 2 ), (Spawner Flour) )
                            , ( ( 0, 3 ), (Spawner bun) )
                            ]

                    -- When spawning
                    ( spawnedThings, _ ) =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen
                in
                    -- Then spawns the selected Sparners type of thing
                    spawnedThings
                        |> Dict.toList
                        |> List.head
                        |> Maybe.map
                            (\( coords, thing ) ->
                                equal (Just thing)
                                    (Grid.get coords initialKitchen
                                        |> Maybe.withDefault PlainTile
                                        |> (\tile ->
                                                case tile of
                                                    Spawner thing ->
                                                        Just thing

                                                    _ ->
                                                        Nothing
                                           )
                                    )
                            )
                        |> Maybe.withDefault (fail "Nothing spawned")
        ]


collectSuite : Test
collectSuite =
    describe "Collecting buns"
        [ test "Buns are collected at spawn tiles" <|
            \() ->
                let
                    -- Given a level with one collector
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), BunCollector )
                            ]

                    -- And a bun on every tile
                    initialThings =
                        Grid.fromList
                            [ ( ( 0, 0 ), bun )
                            , ( ( 0, 1 ), bun )
                            ]

                    -- When collecting
                    remainingThings =
                        SweetBuns.collectThings initialKitchen initialThings

                    -- THe bun on the collector tile is removed
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 0 ), bun )
                            ]
                in
                    equal remainingThings expectedThings
        , test "Only ready Buns are collected (not ingredients" <|
            \() ->
                let
                    -- Given a level with one collector
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 1 ), BunCollector )
                            , ( ( 0, 2 ), BunCollector )
                            , ( ( 0, 3 ), BunCollector )
                            ]

                    -- And a something on every tile
                    initialThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), Flour )
                            , ( ( 0, 2 ), Water )
                            , ( ( 0, 3 ), Bun )
                            ]

                    -- When collecting
                    remainingThings =
                        SweetBuns.collectThings initialKitchen initialThings

                    -- Only the bun is collected
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), Flour )
                            , ( ( 0, 2 ), Water )
                            ]
                in
                    equal remainingThings expectedThings
        ]


movementSuite : Test
movementSuite =
    describe "Bun movement pattern"
        [ test "Move down" <|
            (\_ ->
                let
                    -- Given a single bun (free of obstacles)
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.moveAll Set.empty initialState

                    -- Then it moves down
                    expectedState =
                        (Grid.fromList [ ( ( 2, 0 ), bun ) ])
                in
                    equal movedState expectedState
            )
        , test "Obstacles prevent downward movement" <|
            (\_ ->
                let
                    -- Given a single bun surrounded by terrain
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 1, 1 ), ( 2, 0 ), ( 3, 1 ) ]
                in
                    -- When progressing movement
                    -- Then they stay in the same place
                    expectNoMovemenemt terrain initialState
            )
        , test "Other buns prevent downward movement" <|
            (\_ ->
                let
                    -- Given two buns stacked on to of each other
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ), ( ( 2, 2 ), bun ) ]

                    -- And surrounded by terrain
                    terrain =
                        Set.fromList [ ( 1, 2 ), ( 1, 1 ), ( 2, 0 ), ( 3, 2 ), ( 3, 1 ) ]
                in
                    -- When progressing movement
                    -- Then they stay in the same place
                    expectNoMovemenemt terrain initialState
            )
        , test "Moves right if only option" <|
            (\_ ->
                let
                    -- Given a bun blocked from moving down or to the left
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 1, 1 ), ( 2, 0 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.moveAll terrain initialState

                    -- Then is moves to the right
                    expectedState =
                        Grid.fromList [ ( ( 3, 1 ), bun ) ]
                in
                    equal movedState expectedState
            )
        , test "Moves left if only option" <|
            (\_ ->
                let
                    -- Given a bun bocked from moving down and to the right
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 3, 1 ), ( 2, 0 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.moveAll terrain initialState

                    -- Then it moves to the left
                    expectedState =
                        Grid.fromList [ ( ( 1, 1 ), bun ) ]
                in
                    equal movedState expectedState
            )
        , test "Do not overwrite each other" <|
            (\_ ->
                let
                    -- Given two buns destined to end up tn the same place
                    initialState =
                        Grid.fromList
                            [ ( ( 1, 2 ), bun )
                            , ( ( 1, 1 ), bun )
                            , ( ( 2, 1 ), bun )
                            ]

                    terrain =
                        Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.moveAll terrain initialState

                    -- Then all buns still are there
                    -- (Exact placement is not relevant)
                in
                    equal (Dict.size movedState) (Dict.size initialState)
            )
        , test "Mixable ingredients can be moved onto each other, combining into a new Thing" <|
            \() ->
                let
                    -- Given Water that is on top of Flour
                    initialState =
                        Grid.fromList [ ( ( 0, 1 ), Water ), ( ( 0, 0 ), Flour ) ]

                    -- When the Water moves (down)
                    movedState =
                        SweetBuns.moveSingle Set.empty ( 0, 1 ) initialState

                    -- Then a Bun is created
                    expectedState =
                        Grid.fromList [ ( ( 0, 0 ), bun ) ]
                in
                    equal movedState expectedState
        ]


mixingSuite : Test
mixingSuite =
    describe "Mixing ingredients"
        [ test "Mixing Flour with Water produces a Bun" <|
            \() ->
                SweetBuns.mixIngredients Flour Water |> equal (Just Bun)
        , test "Mixing Water with Flour produces a Bun" <|
            \() ->
                SweetBuns.mixIngredients Water Flour |> equal (Just Bun)
        ]



-- # HELPERS


bun : Thingy
bun =
    Bun


{-| Verrify that buns do not move
-}
expectNoMovemenemt : Set.Set Grid.Coords -> Grid.Grid SweetBuns.Thingy -> Expectation
expectNoMovemenemt terrain buns =
    SweetBuns.moveAll terrain buns |> equal buns
