module Grid.MovementRangeTest exposing (chartSuite)

import Expect exposing (Expectation)
import Grid.MovementRange as Range exposing (Coords)
import Set exposing (Set)
import Test exposing (..)


chartSuite : Test
chartSuite =
    describe "Charting"
        [ describe "Range.chart"
            [ test "Can move to closest neighbours by taking one (1) step" <|
                \() ->
                    Expect.equal
                        (Range.chart Set.empty ( 0, 0 ) 1)
                        (Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( -1, 0 ), ( 0, -1 ) ])
            , test "Can move a bit further by taking two (2) step2" <|
                \() ->
                    Expect.equal
                        (Range.chart Set.empty ( 0, 0 ) 2)
                        (List.concat
                            [ [ ( 0, 2 ) ]
                            , [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
                            , [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
                            , [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
                            , [ ( 0, -2 ) ]
                            ]
                            |> Set.fromList
                        )
            , test "Hindered by obstacles" <|
                \() ->
                    Expect.equal
                        (Range.chart (Set.fromList [ ( 0, 1 ) ]) ( 0, 0 ) 2)
                        (List.concat
                            [ [ ( -1, 1 ), ( 1, 1 ) ]
                            , [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
                            , [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
                            , [ ( 0, -2 ) ]
                            ]
                            |> Set.fromList
                        )
            , test "Can move to the closest neighbours from a non-origo starting point" <|
                \() ->
                    Expect.equal
                        (Range.chart Set.empty ( 100, 100 ) 1)
                        (Set.fromList [ ( 100, 100 ), ( 100, 101 ), ( 101, 100 ), ( 99, 100 ), ( 100, 99 ) ])
            ]
        ]
