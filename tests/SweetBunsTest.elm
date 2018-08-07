module SweetBunsTest exposing (..)

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
                        SweetBuns.step initialState

                    expectedState =
                        (Grid.fromList [ ( ( 2, 0 ), "@" ) ])
                in
                    equal movedState expectedState
            )
        ]
