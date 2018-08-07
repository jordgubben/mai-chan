module SweetBunsTest exposing (..)

import Set
import Grid
import SweetBuns
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
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.step Set.empty initialState

                    -- Then it moves down
                    expectedState =
                        (Grid.fromList [ ( ( 2, 0 ), "@" ) ])
                in
                    equal movedState expectedState
            )
        , test "Obstacles prevent downward movement" <|
            (\_ ->
                let
                    -- Given a single bun surrounded by terrain
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

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
                        Grid.fromList [ ( ( 2, 1 ), "@" ), ( ( 2, 2 ), "@" ) ]

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
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

                    terrain =
                        Set.fromList [ ( 1, 1 ), ( 2, 0 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.step terrain initialState

                    -- Then is moves to the right
                    expectedState =
                        Grid.fromList [ ( ( 3, 1 ), "@" ) ]
                in
                    equal movedState expectedState
            )
        , test "Moves left if only option" <|
            (\_ ->
                let
                    -- Given a bun bocked from moving down and to the right
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

                    terrain =
                        Set.fromList [ ( 3, 1 ), ( 2, 0 ) ]

                    -- When progressing movement
                    movedState =
                        SweetBuns.step terrain initialState

                    -- Then it moves to the left
                    expectedState =
                        Grid.fromList [ ( ( 1, 1 ), "@" ) ]
                in
                    equal movedState expectedState
            )
        ]



-- # HELPERS


{-| Verrify that buns do not move
-}
expectNoMovemenemt : Set.Set Grid.Coords -> Grid.Grid SweetBuns.Bun -> Expectation
expectNoMovemenemt terrain buns =
    SweetBuns.step terrain buns |> equal buns
