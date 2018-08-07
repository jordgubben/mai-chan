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
                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

                    movedState =
                        SweetBuns.step Set.empty initialState

                    expectedState =
                        (Grid.fromList [ ( ( 2, 0 ), "@" ) ])
                in
                    equal movedState expectedState
            )
        , test "Stop at obstacles" <|
            (\_ ->
                let
                    terrain =
                        Set.fromList [ ( 2, 0 ) ]

                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), "@" ) ]

                    movedState =
                        SweetBuns.step terrain initialState

                    expectedState =
                        initialState
                in
                    equal movedState expectedState
            )
        , test "Stack buns" <|
            (\_ ->
                let
                    terrain =
                        Set.fromList [ ( 2, 0 ) ]

                    initialState =
                        Grid.fromList [ ( ( 2, 1 ), "@" ), ( ( 2, 2 ), "@" ) ]

                    movedState =
                        SweetBuns.step terrain initialState

                    expectedState =
                        initialState
                in
                    equal movedState expectedState
            )
        ]
