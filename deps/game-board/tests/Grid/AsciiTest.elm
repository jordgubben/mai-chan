module Grid.AsciiTest exposing (asciiSuite)

import Expect exposing (Expectation, equal)
import Grid
import Grid.Ascii as Ascii
import Test exposing (..)


asciiSuite : Test
asciiSuite =
    describe "AsciiGris(s)"
        [ test "Creates grids from asci" <|
            \() ->
                let
                    -- Given some assci
                    srcAscii =
                        ( ( 3, 2 )
                        , """
                            #==#
                            1234
                            """
                        )

                    -- When converting
                    asciiGrid =
                        Ascii.fromAscii srcAscii

                    -- Then becomes a grid
                    expectedGrid =
                        Grid.fromList
                            [ ( ( 3, 2 ), '#' )
                            , ( ( 4, 2 ), '=' )
                            , ( ( 5, 2 ), '=' )
                            , ( ( 6, 2 ), '#' )
                            , ( ( 3, 1 ), '1' )
                            , ( ( 4, 1 ), '2' )
                            , ( ( 5, 1 ), '3' )
                            , ( ( 6, 1 ), '4' )
                            ]
                in
                equal expectedGrid asciiGrid

        --        , todo "Creates ascii from grids"
        --        , todo "Cobvert back and forth without destructing"
        ]
