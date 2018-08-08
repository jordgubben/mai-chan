module SweetBunsTest exposing (..)

import Set
import Grid
import SweetBuns exposing (Thingy(..))
import Test exposing (..)
import Expect exposing (Expectation, equal)


bunStepSuit : Test
bunStepSuit =
    describe "Bun movement pattern"
        [ test "Move down" <|
            (\_ ->
                let
                    -- Given a single bun (free of obstacles)
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), bun ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.step Set.empty initialState

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
                        SweetBuns.step terrain initialState

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
                        SweetBuns.step terrain initialState

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
                            [ ( ( 1, 2 ), bunNr 1 )
                            , ( ( 1, 1 ), bunNr 2 )
                            , ( ( 2, 1 ), bunNr 3 )
                            ]

                    terrain =
                        Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.step terrain initialState

                    -- Then all buns still are there
                    -- (Exact placement is not relevant)
                    expectedState =
                        Grid.fromList
                            [ ( ( 2, 2 ), bunNr 1 )
                            , ( ( 0, 1 ), bunNr 2 )
                            , ( ( 2, 1 ), bunNr 3 )
                            ]
                in
                    equal movedState expectedState
            )
        ]



-- # HELPERS


bun : Thingy
bun =
    Bun "@"


bunNr : Int -> Thingy
bunNr nr =
    Bun ("@" ++ toString nr)


{-| Verrify that buns do not move
-}
expectNoMovemenemt : Set.Set Grid.Coords -> Grid.Grid SweetBuns.Thingy -> Expectation
expectNoMovemenemt terrain buns =
    SweetBuns.step terrain buns |> equal buns
