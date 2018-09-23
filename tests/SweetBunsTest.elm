module SweetBunsTest exposing (..)

import Random exposing (Seed)
import Set
import Dict
import Grid
import Thingy exposing (Thingy(..), Flavour(..))
import SweetBuns exposing (FloorTile(..))
import Test exposing (..)
import Fuzz exposing (..)
import Expect exposing (Expectation, equal, fail)


spawnSuite : Test
spawnSuite =
    describe "Spawning pattern"
        [ fuzz int "Buns can spawn from a random spawner" <|
            \s ->
                let
                    seed =
                        Random.initialSeed s

                    -- Given a kitchen with several spawn points
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), (Spawner [ bun ]) )
                            , ( ( 0, 2 ), (Spawner [ bun ]) )
                            , ( ( 0, 3 ), (Spawner [ bun ]) )
                            ]

                    -- When spawning
                    ( spawnedThing, _ ) =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen Set.empty
                in
                    spawnedThing
                        |> Maybe.map
                            (\( coords, thing ) ->
                                (Grid.get coords initialKitchen) |> equal (Just (Spawner [ bun ]))
                            )
                        |> Maybe.withDefault (Expect.fail "Supposed to spawn something")
        , fuzz int "Spawners spawn their defined thing" <|
            \s ->
                let
                    seed =
                        Random.initialSeed s

                    -- Given a kitchen with several spawn points spawning different things
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 0 ), PlainTile )
                            , ( ( 0, 1 ), (Spawner [ water ]) )
                            , ( ( 0, 2 ), (Spawner [ flour ]) )
                            , ( ( 0, 3 ), (Spawner [ bun ]) )
                            ]

                    -- When spawning
                    spawnedThing : Maybe ( Grid.Coords, Thingy )
                    spawnedThing =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen Set.empty
                            |> Tuple.first
                in
                    -- Then spawns the selected Sparners type of thing
                    spawnedThing
                        |> Maybe.map
                            (\( coords, thing ) ->
                                case (Grid.get coords initialKitchen) of
                                    Just (Spawner things) ->
                                        equal (Just thing) (things |> List.head)

                                    _ ->
                                        Expect.fail ("Expected a spawner at " ++ toString coords)
                            )
                        |> Maybe.withDefault (Expect.fail "Should spawn something")
        , fuzz int "Spawners can spawn one of several things, but nothing else than defined" <|
            \s ->
                let
                    seed =
                        Random.initialSeed s

                    -- Given a kitchen with a single spawner
                    spawnerCoords =
                        ( 0, 0 )

                    spawnables =
                        [ flour, water ]

                    initialKitchen =
                        Grid.fromList
                            [ ( spawnerCoords, (Spawner spawnables) )
                            ]

                    -- When spawning
                    spawnedThing : Maybe ( Grid.Coords, Thingy )
                    spawnedThing =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen Set.empty
                            |> Tuple.first
                in
                    -- Then spawns the selected Sparners type of thing
                    spawnedThing
                        |> Maybe.map
                            (\( _, thing ) ->
                                Expect.true
                                    ("Should be one of " ++ (toString spawnables))
                                    (List.member thing spawnables)
                            )
                        |> Maybe.withDefault (Expect.fail "Should spawn something")
        , test "Only spawn on unoccupied spawners" <|
            \() ->
                let
                    -- Given a kitchen where all floor tiles are occupied
                    floor =
                        Grid.fromList [ ( ( 0, 0 ), Spawner [ water ] ) ]

                    obstacles =
                        Set.singleton ( 0, 0 )

                    -- When spawning
                    ( spawnedThing, _ ) =
                        SweetBuns.spawnSingelThingRnd (Random.initialSeed 0) floor obstacles
                in
                    -- Nothing is spawned
                    spawnedThing |> equal Nothing
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
                    ( remainingThings, _ ) =
                        SweetBuns.collectThings initialKitchen initialThings

                    -- THe bun on the collector tile is removed
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 0 ), bun )
                            ]
                in
                    equal remainingThings expectedThings
        , test "Only ready Buns are collected (not ingredients)" <|
            \() ->
                let
                    -- Given a level with three collectors
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 1 ), BunCollector )
                            , ( ( 0, 2 ), BunCollector )
                            , ( ( 0, 3 ), BunCollector )
                            ]

                    -- And a something on every tile
                    initialThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), flour )
                            , ( ( 0, 2 ), water )
                            , ( ( 0, 3 ), bun )
                            ]

                    -- When collecting
                    ( remainingThings, _ ) =
                        SweetBuns.collectThings initialKitchen initialThings

                    -- Only the bun is collected
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), flour )
                            , ( ( 0, 2 ), water )
                            ]
                in
                    equal remainingThings expectedThings
        , test "Colected Buns generate points" <|
            \() ->
                let
                    -- Given a level with one collector
                    initialKitchen =
                        Grid.fromList
                            [ ( ( 0, 1 ), BunCollector )
                            ]

                    -- And a something on every tile
                    initialThings =
                        Grid.fromList
                            [ ( ( 0, 1 ), bun )
                            ]

                    -- When collecting
                    ( _, score ) =
                        SweetBuns.collectThings initialKitchen initialThings
                in
                    equal score 100
        ]


fallingSuite : Test
fallingSuite =
    describe "Falling things fall downward"
        [ test "Move down" <|
            (\_ ->
                let
                    -- Given a single bun (free of obstacles)
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.applyGravity Grid.empty initialState

                    -- Then it moves down
                    expectedState =
                        (Grid.fromList [ ( ( 2, 0 ), bun ) ])
                in
                    equal movedState expectedState
            )
        , test "Is stable if nothing is going to fall"
            (\_ ->
                let
                    -- Given a single grounded bun
                    initialState =
                        Grid.fromList [ ( ( 0, 1 ), bun ) ]

                    terrain =
                        Grid.drawBox WallTile (Grid.Size 1 1)

                    -- When checking if stable
                    stability =
                        SweetBuns.isStable terrain initialState
                in
                    -- Then is stable
                    Expect.true "Should be stable" stability
            )
        , test "Is unstable if something is going to fall"
            (\_ ->
                let
                    -- Given a single slightly elevated  bun
                    initialState =
                        Grid.fromList [ ( ( 0, 2 ), bun ) ]

                    terrain =
                        Grid.drawBox WallTile (Grid.Size 1 1)

                    -- When checking if stable
                    stability =
                        SweetBuns.isStable terrain initialState
                in
                    -- Then is not stable
                    Expect.false "Should not be stable" stability
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
        , test "Stays still, even though moving to the right is possible" <|
            (\_ ->
                let
                    -- Given a bun blocked from moving down or to the left
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 1, 1 ), ( 2, 0 ) ]

                    -- When progressing movement
                    -- Then is stays in place
                in
                    expectNoMovemenemt terrain initialState
            )
        , test "Stays still, even though moving to the left is possible" <|
            (\_ ->
                let
                    -- Given a bun bocked from moving down and to the right
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 3, 1 ), ( 2, 0 ) ]
                in
                    -- When progressing movement
                    -- Then it stays is place
                    expectNoMovemenemt terrain initialState
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
                        [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ) ]
                            |> List.map (\coords -> ( coords, WallTile ))
                            |> Grid.fromList

                    -- When progressing movement
                    movedState =
                        SweetBuns.applyGravity terrain initialState

                    -- Then all buns still are there
                    -- (Exact placement is not relevant)
                in
                    equal (Dict.size movedState) (Dict.size initialState)
            )
        , test "Mixable ingredients can be moved onto each other, combining into a new Thing" <|
            \() ->
                let
                    -- Given Water that is on top of Flour
                    -- And Flour in turn sitting on top of an obstacle (that does not fall)
                    initialState =
                        Grid.fromList
                            [ ( ( 0, 1 ), water )
                            , ( ( 0, 0 ), flour )
                            , ( ( 0, -1 ), Obstacle )
                            ]

                    -- When the Water moves (down)
                    movedState =
                        SweetBuns.applyGravity Grid.empty initialState

                    -- Then a Bun is created
                    expectedState =
                        Grid.fromList [ ( ( 0, 0 ), bun ), ( ( 0, -1 ), Obstacle ) ]
                in
                    equal movedState expectedState
        ]


consciousMovementSuite : Test
consciousMovementSuite =
    describe "Conscious movement initiated by the player"
        [ test "Can move to an empty adjacens tile" <|
            \() ->
                let
                    -- Given a bun with no neighbours
                    initialThings =
                        Grid.fromList [ ( ( 10, 20 ), bun ) ]

                    -- When moving
                    movedThings =
                        SweetBuns.attemptMove Set.empty ( 10, 20 ) ( 10, 21 ) initialThings

                    -- Then it is moved
                    expectedThings =
                        Grid.fromList [ ( ( 10, 21 ), bun ) ]
                in
                    movedThings |> equal expectedThings
        , test "Can not move to an occupied tile" <|
            \() ->
                let
                    -- Given a bun with a neighbour
                    initialThings =
                        Grid.fromList [ ( ( 14, 10 ), bun ), ( ( 15, 10 ), bun ) ]

                    -- When moving on onto neighbour
                    movedThings =
                        SweetBuns.attemptMove Set.empty ( 14, 10 ) ( 15, 10 ) initialThings
                in
                    -- Then it is not moved
                    movedThings |> equal initialThings
        , test "Can not move to an terrain tile" <|
            \() ->
                let
                    -- Given a single bun
                    initialThings =
                        Grid.fromList [ ( ( 0, 0 ), bun ) ]

                    -- And some terrain
                    terrain =
                        Set.fromList [ ( 0, -1 ) ]

                    -- When moving on onto terrain
                    movedThings =
                        SweetBuns.attemptMove terrain ( 0, 0 ) ( 0, -1 ) initialThings
                in
                    -- Then it is not moved
                    movedThings |> equal initialThings
        , test "Can move to an occupied tile if ingredients are mixable" <|
            \() ->
                let
                    -- Given a bun with a neighbour
                    initialThings =
                        Grid.fromList [ ( ( 14, 10 ), water ), ( ( 15, 10 ), flour ) ]

                    -- When moving on onto neighbour
                    movedThings =
                        SweetBuns.attemptMove Set.empty ( 14, 10 ) ( 15, 10 ) initialThings

                    -- Then it is not moved
                    expectedThings =
                        Grid.fromList [ ( ( 15, 10 ), bun ) ]
                in
                    movedThings |> equal expectedThings
        , test "Can move by swaping places" <|
            \() ->
                let
                    -- Given a a chilli,
                    -- with *sweet* water on top of it
                    -- and *sweet* flour beside it
                    initialThings =
                        Grid.fromList
                            [ ( ( 0, 0 ), chilli )
                            , ( ( 0, 1 ), Water (Just Sugar) )
                            , ( ( -1, 0 ), Flour (Just Sugar) )
                            ]

                    -- And terrain below it
                    terrain =
                        Set.fromList [ ( -1, -1 ), ( 0, -1 ) ]

                    -- When moving flour onto chilli
                    movedThings =
                        SweetBuns.attemptMove terrain ( -1, 0 ) ( 0, 0 ) initialThings

                    -- Then they trade places, (causing a chain reaction)
                    expectedThings =
                        Grid.fromList
                            [ ( ( -1, 0 ), chilli )
                            , ( ( 0, 1 ), Water (Just Sugar) )
                            , ( ( 0, 0 ), Flour (Just Sugar) )
                            ]
                in
                    movedThings |> equal expectedThings
        , test "Can not move to an non-adjacent tile" <|
            \() ->
                let
                    -- Given a bun
                    initialThings =
                        Grid.fromList [ ( ( 10, 10 ), bun ) ]

                    -- When moving to a non-adjacent tile
                    movedThings =
                        SweetBuns.attemptMove Set.empty ( 10, 10 ) ( 10, 100 ) initialThings
                in
                    -- Then it is not moved
                    movedThings |> equal initialThings
        ]


gameProgressSuite : Test
gameProgressSuite =
    describe "Game progress"
        [ test "Game over if all spawn tiles are covered" <|
            \() ->
                let
                    -- Given a single spawn tile
                    floor =
                        Grid.fromList [ ( ( 0, 0 ), Spawner [ water ] ) ]

                    -- And it is covered
                    things =
                        Grid.fromList [ ( ( 0, 0 ), water ) ]
                in
                    -- When checking if game over
                    SweetBuns.isGameOver floor things
                        -- Then it is game over
                        |> Expect.true "Supposed to be 'Game over'"
        ]



-- # HELPERS


sugar : Thingy
sugar =
    Flavouring Sugar


chilli : Thingy
chilli =
    Flavouring Chilli


water : Thingy
water =
    Water Nothing


flour : Thingy
flour =
    Flour Nothing


bun : Thingy
bun =
    Bun Nothing


{-| Verrify that buns do not move
-}
expectNoMovemenemt : Set.Set Grid.Coords -> Grid.Grid Thingy -> Expectation
expectNoMovemenemt terrain buns =
    let
        floor : Grid.Grid FloorTile
        floor =
            terrain |> Set.toList |> List.map (\coords -> ( coords, WallTile )) |> Grid.fromList
    in
        SweetBuns.applyGravity floor buns |> equal buns
