module SweetBunsTest exposing (collectSuite, consciousMovementSuite, fallingSuite, gameProgressSuite, spawnSuite)

import Dict
import Expect exposing (Expectation, equal, fail)
import FloorTile exposing (FloorTile(..))
import Fuzz exposing (..)
import Grid
import Random exposing (Seed)
import Set
import SweetBuns exposing (Board)
import Test exposing (..)
import Thingy exposing (Flavour(..), Thingy(..))


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
                            , ( ( 0, 1 ), Spawner [ bun ] )
                            , ( ( 0, 2 ), Spawner [ bun ] )
                            , ( ( 0, 3 ), Spawner [ bun ] )
                            ]

                    -- When spawning
                    ( spawnedThing, _ ) =
                        SweetBuns.spawnSingelThingRnd seed initialKitchen Set.empty
                in
                spawnedThing
                    |> Maybe.map
                        (\( coords, thing ) ->
                            Grid.get coords initialKitchen |> equal (Just (Spawner [ bun ]))
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
                            , ( ( 0, 1 ), Spawner [ water ] )
                            , ( ( 0, 2 ), Spawner [ flour ] )
                            , ( ( 0, 3 ), Spawner [ bun ] )
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
                            case Grid.get coords initialKitchen of
                                Just (Spawner things) ->
                                    equal (Just thing) (things |> List.head)

                                _ ->
                                    Expect.fail ("Expected a spawner at " ++ Debug.toString coords)
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
                            [ ( spawnerCoords, Spawner spawnables )
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
                                ("Should be one of " ++ Debug.toString spawnables)
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
                        SweetBuns.collectThings
                            { floor = initialKitchen
                            , things = initialThings
                            }

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
                        SweetBuns.collectThings { floor = initialKitchen, things = initialThings }

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
                        SweetBuns.collectThings
                            { floor = initialKitchen
                            , things = initialThings
                            }
                in
                equal score 100
        ]


fallingSuite : Test
fallingSuite =
    describe "Falling things fall downward"
        [ test "Move down" <|
            \_ ->
                let
                    -- Given a single bun (free of obstacles)
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.applyGravity Set.empty initialState

                    -- Then it moves down
                    expectedState =
                        Grid.fromList [ ( ( 2, 0 ), bun ) ]
                in
                equal movedState expectedState
        , test "Is stable if nothing is going to fall"
            (\_ ->
                let
                    -- Given a single grounded bun
                    initialState =
                        Grid.fromList [ ( ( 0, 1 ), bun ) ]

                    terrain =
                        Set.fromList [ ( 0, 0 ) ]

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
                        Set.fromList [ ( 0, 0 ) ]

                    -- When checking if stable
                    stability =
                        SweetBuns.isStable terrain initialState
                in
                -- Then is not stable
                Expect.false "Should not be stable" stability
            )
        , test "Obstacles prevent downward movement" <|
            \_ ->
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
        , test "Other buns prevent downward movement" <|
            \_ ->
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
        , test "Stays still, even though moving to the right is possible" <|
            \_ ->
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
        , test "Stays still, even though moving to the left is possible" <|
            \_ ->
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
        , test "Do not overwrite each other" <|
            \_ ->
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
                            |> Set.fromList

                    -- When progressing movement
                    movedState =
                        SweetBuns.applyGravity terrain initialState

                    -- Then all buns still are there
                    -- (Exact placement is not relevant)
                in
                equal (Dict.size movedState) (Dict.size initialState)
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
                        SweetBuns.applyGravity Set.empty initialState

                    -- Then a Bun is created
                    expectedState =
                        Grid.fromList [ ( ( 0, 0 ), bun ), ( ( 0, -1 ), Obstacle ) ]
                in
                equal movedState expectedState
        , describe "SweetBuns.shouldFall – Fall prediction"
            [ test "A single Bun on an otherwise empty board should fall" <|
                \() ->
                    SweetBuns.shouldFall ( 0, 0 )
                        { emptyBoard
                            | things = Grid.fromList [ ( ( 0, 0 ), bun ) ]
                        }
                        |> equal True
            , test "A single Obstacle on an otherwise empty board should *not* fall" <|
                \() ->
                    SweetBuns.shouldFall ( 0, 0 )
                        { emptyBoard
                            | things = Grid.fromList [ ( ( 0, 0 ), Obstacle ) ]
                        }
                        |> equal False
            , test "Water on top of Flour should 'fall'" <|
                \() ->
                    SweetBuns.shouldFall ( 0, 0 )
                        { emptyBoard
                            | things =
                                Grid.fromList
                                    [ ( ( 0, 1 ), water )
                                    , ( ( 0, 0 ), flour )
                                    ]
                        }
                        |> equal True
            , test "Water on top of an Obstacle Flour should *not* fall" <|
                \() ->
                    SweetBuns.shouldFall ( 0, 0 )
                        { emptyBoard
                            | things =
                                Grid.fromList
                                    [ ( ( 0, 1 ), water )
                                    , ( ( 0, 0 ), Obstacle )
                                    ]
                        }
                        |> equal False
            , test "Flour on top of a WallTile should *not* fall" <|
                \() ->
                    SweetBuns.shouldFall ( 0, 0 )
                        { emptyBoard
                            | things =
                                Grid.fromList
                                    [ ( ( 0, 1 ), water )
                                    ]
                            , floor =
                                Grid.fromList
                                    [ ( ( 0, 0 ), WallTile )
                                    ]
                        }
                        |> equal False
            ]
        , describe "SweetBuns.isEmptyTile – Movement posibilities"
            (let
                tileCoords =
                    ( 0, 0 )

                testIsEmpty msg board expectedResult =
                    test msg <|
                        \() ->
                            SweetBuns.isEmptyTile tileCoords board
                                |> equal expectedResult
             in
             [ testIsEmpty "Completely empty cell is empty" emptyBoard True
             , testIsEmpty "PlainTile tile is empty"
                { emptyBoard
                    | floor = Grid.fromList [ ( tileCoords, PlainTile ) ]
                }
                True
             , testIsEmpty "WallTile tile is empty"
                { emptyBoard
                    | floor = Grid.fromList [ ( tileCoords, WallTile ) ]
                }
                False
             ]
            )
        ]


consciousMovementSuite : Test
consciousMovementSuite =
    describe "Conscious movement initiated by the player"
        [ test "Can move to an empty adjacens tile" <|
            \() ->
                let
                    -- Given a bun with no neighbours
                    initialBoard =
                        { emptyBoard
                            | things =
                                Grid.fromList [ ( ( 10, 20 ), bun ) ]
                        }

                    -- When moving
                    movedThings =
                        SweetBuns.attemptMove ( 10, 20 ) ( 10, 21 ) initialBoard

                    -- Then it is moved
                    expectedThings =
                        Grid.fromList [ ( ( 10, 21 ), bun ) ]
                in
                movedThings |> equal expectedThings
        , test "Can not move to an occupied tile" <|
            \() ->
                let
                    -- Given a bun with a neighbour
                    initialBoard =
                        { emptyBoard
                            | things =
                                Grid.fromList
                                    [ ( ( 14, 10 ), bun )
                                    , ( ( 15, 10 ), bun )
                                    ]
                        }

                    -- When moving on onto neighbour
                    movedThings =
                        SweetBuns.attemptMove ( 14, 10 ) ( 15, 10 ) initialBoard
                in
                -- Then it is not moved
                movedThings |> equal initialBoard.things
        , test "Can not move to an terrain tile" <|
            \() ->
                let
                    initialBoard =
                        { -- Given a single bun
                          things = Grid.fromList [ ( ( 0, 0 ), bun ) ]
                        , -- And some terrain
                          floor =
                            Grid.fromList [ ( ( 0, -1 ), WallTile ) ]
                        }

                    -- When moving on onto terrain
                    movedThings =
                        SweetBuns.attemptMove ( 0, 0 ) ( 0, -1 ) initialBoard
                in
                -- Then it is not moved
                movedThings |> equal initialBoard.things
        , test "Can move to an occupied tile if ingredients are mixable" <|
            \() ->
                let
                    -- Given a bun with a neighbour
                    initialBoard =
                        { emptyBoard
                            | things =
                                Grid.fromList
                                    [ ( ( 14, 10 ), water )
                                    , ( ( 15, 10 ), flour )
                                    ]
                        }

                    -- When moving on onto neighbour
                    movedThings =
                        SweetBuns.attemptMove ( 14, 10 ) ( 15, 10 ) initialBoard

                    -- Then it is not moved
                    expectedThings =
                        Grid.fromList [ ( ( 15, 10 ), bun ) ]
                in
                movedThings |> equal expectedThings
        , test "Can move by swaping places, if it destabilizes the grid" <|
            \() ->
                let
                    initialBoard =
                        { -- Given a a chilli,
                          -- with *sweet* water on top of it
                          -- and *sweet* flour below it
                          things =
                            Grid.fromList
                                [ ( ( 0, 2 ), Water (Just Sugar) )
                                , ( ( 0, 1 ), chilli )
                                , ( ( 0, 0 ), Flour (Just Sugar) )
                                ]
                        , -- And terrain below it
                          floor =
                            Grid.fromList [ ( ( 0, -1 ), WallTile ) ]
                        }

                    -- When moving flour onto chilli
                    movedThings =
                        SweetBuns.attemptMove ( 0, 1 ) ( 0, 2 ) initialBoard

                    -- Then they trade places, (causing a chain reaction)
                    expectedThings =
                        Grid.fromList
                            [ ( ( 0, 2 ), chilli )
                            , ( ( 0, 1 ), Water (Just Sugar) )
                            , ( ( 0, 0 ), Flour (Just Sugar) )
                            ]
                in
                movedThings |> equal expectedThings
        , test "Can not move by swaping places, if the result is stable" <|
            \() ->
                let
                    initialBoard =
                        { -- Given a a chilli,
                          -- with *sweet* water on top of it
                          -- and *chilly* flour below it
                          things =
                            Grid.fromList
                                [ ( ( 0, 2 ), Water (Just Sugar) )
                                , ( ( 0, 1 ), chilli )
                                , ( ( 0, 0 ), Flour (Just Chilli) )
                                ]
                        , -- And terrain below it
                          floor =
                            Grid.fromList [ ( ( 0, -1 ), WallTile ) ]
                        }

                    -- When moving flour onto chilli
                    movedThings =
                        SweetBuns.attemptMove ( 0, 1 ) ( 0, 2 ) initialBoard
                in
                -- Then nothing happens (since it causes no chain reaction)
                movedThings |> equal initialBoard.things
        , describe "Can move to any tile in a cross pattern"
            (let
                -- Given a bun
                initialBoard =
                    { emptyBoard
                        | things = Grid.fromList [ ( ( 10, 10 ), bun ) ]
                    }

                testMoveOk targetCoords =
                    let
                        -- When moving
                        movedThings =
                            SweetBuns.attemptMove ( 10, 10 ) targetCoords initialBoard

                        -- Then that is ok
                        expectedThings =
                            Grid.fromList [ ( targetCoords, bun ) ]
                    in
                    movedThings |> equal expectedThings
             in
             [ fuzz int "(verically)" <|
                \y -> testMoveOk ( 10, y )
             , fuzz int "(horizontally)" <|
                \x -> testMoveOk ( x, 10 )
             ]
            )
        , test "Can not move tiles outside cross pattern" <|
            \() ->
                let
                    -- Given a bun
                    initialBoard =
                        { emptyBoard
                            | things = Grid.fromList [ ( ( 10, 10 ), bun ) ]
                        }

                    --  When attempting to move
                    movedThings =
                        SweetBuns.attemptMove ( 10, 10 ) ( 11, 11 ) initialBoard

                    -- Then is prevented
                in
                movedThings |> equal initialBoard.things
        ]


gameProgressSuite : Test
gameProgressSuite =
    describe "Game progress"
        [ test "Game over if all spawn tiles are covered and conditions are stable" <|
            \() ->
                let
                    -- Given a single spawn tile with flooe under it
                    board =
                        { floor =
                            Grid.fromList
                                [ ( ( 0, 1 ), Spawner [ water ] )
                                , ( ( 0, 0 ), WallTile )
                                ]
                        , -- And it is covered
                          things =
                            Grid.fromList [ ( ( 0, 1 ), water ) ]
                        }
                in
                -- When checking if game over
                SweetBuns.isGameOver board
                    -- Then it is game over
                    |> Expect.true "Supposed to be 'Game over'"
        , test "Not Game over if all spawn tiles, but falling cold free one of them" <|
            \() ->
                let
                    -- Given a single spawn tile with floor nor directly under it
                    board =
                        { floor =
                            Grid.fromList
                                [ ( ( 0, 2 ), Spawner [ water ] )
                                , ( ( 0, 0 ), WallTile )
                                ]
                        , -- And it is covered right now
                          things =
                            Grid.fromList [ ( ( 0, 2 ), water ) ]
                        }
                in
                -- When checking if game over
                SweetBuns.isGameOver board
                    -- Then it is not game over
                    |> Expect.false "Not supposed to be 'Game over'"
        ]



-- # HELPERS


emptyBoard : Board
emptyBoard =
    { floor = Grid.empty
    , things = Grid.empty
    }


sugar : Thingy
sugar =
    Flavouring { flavour = Sugar, packaged = False }


chilli : Thingy
chilli =
    Flavouring { flavour = Chilli, packaged = False }


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
    SweetBuns.applyGravity terrain buns |> equal buns
