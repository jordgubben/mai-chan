module Grid.EdgeDetectionTest exposing (allEdges, edgesSuite, noEdges, terrainSculptingSuite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Grid.EdgeDetection as EdgeDetection exposing (Coords, EdgeSet)
import Set exposing (Set)
import Test exposing (..)



-- Test: Terrain scuplting


terrainSculptingSuite =
    describe "Terrain sculpting"
        [ describe "TerrainPalette"
            [ fuzz2 (tuple ( bool, bool )) (tuple ( bool, bool )) "Can store and retrive tiles based on EdgeSet" <|
                \( t, b ) ( l, r ) ->
                    let
                        edgeSet =
                            EdgeSet t b l r

                        exampleTile =
                            "Example tile"

                        palette =
                            EdgeDetection.emptyPalette
                                |> EdgeDetection.addEdgeTile edgeSet exampleTile
                    in
                    EdgeDetection.getEdgeTile edgeSet palette
                        |> Expect.equal (Just exampleTile)
            ]
        ]



-- Test: EdgeDetection.edges


edgesSuite : Test
edgesSuite =
    describe "EdgeDetection.edges: Coords -> Area -> EdgeSet Bool"
        [ fuzz (tuple ( int, int ))
            "It all alone, then all sides are edges"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y ) Set.empty
                    |> Expect.equal allEdges
            )
        , fuzz (tuple ( int, int ))
            "If compleatly surrounded, then there are no edges"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y )
                    (Set.fromList
                        [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                    )
                    |> Expect.equal noEdges
            )

        -- Specific edges
        , fuzz (tuple ( int, int ))
            "If something to the left then this is not the left edge"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y ) (Set.fromList [ ( x - 1, y ) ])
                    |> Expect.equal { allEdges | left = False }
            )
        , fuzz (tuple ( int, int ))
            "This is not the right edge if something is it's right"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y ) (Set.fromList [ ( x + 1, y ) ])
                    |> Expect.equal { allEdges | right = False }
            )
        , fuzz (tuple ( int, int ))
            "This is not the bottom edge if something is above it"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y ) (Set.fromList [ ( x, y + 1 ) ])
                    |> Expect.equal { allEdges | top = False }
            )
        , fuzz (tuple ( int, int ))
            "This is not the bottom edge if something is below it"
            (\( x, y ) ->
                EdgeDetection.edges ( x, y ) (Set.fromList [ ( x, y - 1 ) ])
                    |> Expect.equal { allEdges | bottom = False }
            )
        ]


allEdges : EdgeSet Bool
allEdges =
    { top = True, bottom = True, left = True, right = True }


noEdges : EdgeSet Bool
noEdges =
    { top = False, bottom = False, left = False, right = False }
