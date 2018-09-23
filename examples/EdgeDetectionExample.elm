module EdgeDetectionExample exposing (..)

{-| Illustrate how edge detection can be used to outline areas on a grid
-}

import Set exposing (Set)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Grid
import Grid.EdgeDetection as EdgeDetection exposing (EdgeSet)
import FloatingIslandsTiles exposing (staticTile, renderTile)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }



-- App types


type alias Model =
    { landscape : Grid.Grid Bool
    }


type Msg
    = Toggle Grid.Coords


initialModel : Model
initialModel =
    (let
        box =
            Grid.drawBox True { width = 5, height = 5 }

        cross =
            [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( -1, 0 ), ( -2, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, -1 ), ( 0, -2 ) ]
                |> List.map (\c -> ( c, True ))
                |> Grid.fromList

        landscape =
            Dict.union
                (cross |> Grid.translate ( -1, -1 ))
                (box |> Grid.translate ( 1, 1 ))
                |> Grid.translate ( 3, 3 )
     in
        { landscape = landscape }
    )



-- App functions


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle coords ->
            case (Dict.get coords model.landscape) of
                Just True ->
                    { model | landscape = Dict.remove coords model.landscape }

                _ ->
                    { model | landscape = Grid.put coords True model.landscape }


view : Model -> Html Msg
view { landscape } =
    let
        landscapeArea =
            Dict.keys landscape |> Set.fromList

        landscapeWithEdedges =
            Dict.map (\coords _ -> EdgeDetection.edges coords landscapeArea) landscape
    in
        Html.div []
            (List.map outerBox
                [ Grid.toHtmlDiv
                    ( 16, 16 )
                    (\coords value ->
                        let
                            color =
                                if value then
                                    "darkblue"
                                else
                                    "lightblue"
                        in
                            Html.div
                                [ style
                                    [ ( "width", "100%" )
                                    , ( "height", "100%" )
                                    , ( "background-color", color )
                                    , ( "cursor", "pointer" )
                                    ]
                                , onClick (Toggle coords)
                                ]
                                [ Html.text " " ]
                    )
                    (Dict.union
                        landscape
                        canvas
                    )
                , Grid.toHtmlDiv ( 16, 16 )
                    (\_ edges -> edgeBox "darkgreen" "lightgreen" edges)
                    landscapeWithEdedges
                , Grid.toHtmlDiv ( 16, 16 )
                    (\_ edges ->
                        (EdgeDetection.getEdgeTile edges turfPalette)
                            |> Maybe.withDefault FloatingIslandsTiles.turfSoloTile
                            |> FloatingIslandsTiles.renderTile never
                    )
                    landscapeWithEdedges
                ]
            )


{-| Paintable area.
-}
canvas : Grid.Grid Bool
canvas =
    (Grid.drawBox False
        { width = Grid.numCols initialModel.landscape
        , height = Grid.numRows initialModel.landscape
        }
    )


{-| Wrapper element used to line up the different views next to each other.
-}
outerBox : Html msg -> Html msg
outerBox content =
    Html.div
        [ style
            [ ( "display", "inline-block" )
            , ( "margin", "20px" )
            , ( "border", "1px solid black" )
            ]
        ]
        [ content ]


filler : String -> Html msg
filler color =
    Html.div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", color )
            ]
        ]
        []


edgeBox : String -> String -> EdgeSet Bool -> Html msg
edgeBox edgeColor fillColor edges =
    let
        borderStyle edgeGetter =
            (if edgeGetter edges then
                edgeColor
             else
                fillColor
            )
    in
        Html.div
            [ style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                , ( "background-color", fillColor )
                , ( "box-sizing", "border-box" )
                , ( "border", "5px solid " ++ fillColor )
                , ( "border-left-color", borderStyle .left )
                , ( "border-right-color", borderStyle .right )
                , ( "border-top-color", borderStyle .top )
                , ( "border-bottom-color", borderStyle .bottom )
                ]
            ]
            []


turfPalette : EdgeDetection.TerrainPalette FloatingIslandsTiles.Tile
turfPalette =
    EdgeDetection.emptyPalette
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = False, left = False, right = False }
                FloatingIslandsTiles.turfNoEdges
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = False, left = True, right = False }
                FloatingIslandsTiles.turfEdgeTopLeft
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = False, left = True, right = False }
                FloatingIslandsTiles.turfEdgeLeft
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = False, left = True, right = False }
                FloatingIslandsTiles.turfEdgeLeft
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = True, left = True, right = False }
                FloatingIslandsTiles.turfEdgeBottomLeft
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = False, left = False, right = False }
                FloatingIslandsTiles.turfEdgeTop
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = True, left = False, right = False }
                FloatingIslandsTiles.turfEdgeBottom
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = False, left = False, right = True }
                FloatingIslandsTiles.turfEdgeTopRight
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = False, left = False, right = True }
                FloatingIslandsTiles.turfEdgeRight
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = True, left = False, right = True }
                FloatingIslandsTiles.turfEdgeBottomRight
           )
        -- Pipes
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = True, left = False, right = False }
                FloatingIslandsTiles.turfEdgeTopBottom
           )
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = False, left = True, right = True }
                FloatingIslandsTiles.turfEdgeLeftRight
           )
        -- Ends
        |> (EdgeDetection.addEdgeTile
                { top = False, bottom = True, left = True, right = True }
                FloatingIslandsTiles.turfEdgeBottomLeftRight
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = False, left = True, right = True }
                FloatingIslandsTiles.turfEdgeTopLeftRight
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = True, left = False, right = True }
                FloatingIslandsTiles.turfEdgeTopBottomRight
           )
        |> (EdgeDetection.addEdgeTile
                { top = True, bottom = True, left = True, right = False }
                FloatingIslandsTiles.turfEdgeTopBottomLeft
           )
