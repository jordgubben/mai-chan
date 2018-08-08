module SweetBuns exposing (..)

import Set exposing (Set)
import Dict
import Html exposing (Html, text)
import Html.Attributes as Att exposing (class, style)
import Html.Events as Ev exposing (onMouseEnter, onMouseLeave, onClick)
import Grid exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }


type alias FullTile =
    { content : Maybe Thingy
    , back : String
    }


type alias Model =
    { selectedTile : Coords
    , things : Grid Thingy
    }


type Msg
    = SelectColumn Coords
    | StepBuns
    | Rotate Coords


type Thingy
    = Bun String
    | Obstacle


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    , things = Dict.union initialBuns initialObstacles
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectColumn coords ->
            { model | selectedTile = coords }

        StepBuns ->
            let
                ( buns, obstacles ) =
                    Dict.partition
                        (\_ t ->
                            case t of
                                Bun _ ->
                                    True

                                _ ->
                                    False
                        )
                        model.things

                terrain =
                    Set.union
                        outerFrame
                        (obstacles |> Dict.keys |> Set.fromList)

                buns_ =
                    step terrain buns

                things_ =
                    Dict.union buns_ obstacles
            in
                { model | things = things_ }

        Rotate ( x, y ) ->
            let
                pickedThings =
                    Grid.pickRect (Size 2 2) ( x, y ) model.things

                remainingThings =
                    Dict.keys pickedThings
                        |> List.foldl Dict.remove model.things

                tranformedThings =
                    pickedThings
                        |> Grid.translate ( -x, -y )
                        |> Grid.rotCv
                        |> Grid.translate ( x, y + 1 )
            in
                { model | things = Dict.union tranformedThings remainingThings }


{-| Move all buns downwards (if possible).
-}
step : Set Coords -> Grid v -> Grid v
step terrain buns =
    Dict.foldl
        (\( x, y ) b g ->
            let
                -- Can not moved to a tile occupied by a bun or terrain
                -- (Both old and new possitons taken account for)
                canMove nc =
                    not (Set.member nc terrain || Dict.member nc g || Dict.member nc buns)

                right =
                    ( x + 1, y )

                left =
                    ( x - 1, y )

                down =
                    ( x, y - 1 )
            in
                if canMove down then
                    Grid.put down b g
                else if canMove right then
                    Grid.put right b g
                else if canMove left then
                    Grid.put left b g
                else
                    Grid.put ( x, y ) b g
        )
        Grid.empty
        buns


view : Model -> Html Msg
view model =
    let
        ( selX, selY ) =
            model.selectedTile

        finalGrid : Grid FullTile
        finalGrid =
            back
                -- Render highlighed column
                |> Dict.map
                    (\( x, y ) tile ->
                        if (x == selX || x == selX + 1) && (y == selY || y == selY + 1) then
                            { tile | back = "lightgreen" }
                        else
                            tile
                    )
                -- Render terrain
                |> Dict.map
                    (\( x, y ) tile ->
                        if Set.member ( x, y ) outerFrame then
                            { tile | back = "black" }
                        else
                            tile
                    )
                -- Render buns
                |> Dict.map
                    (\coords tile ->
                        { tile
                            | content = (Dict.get coords model.things)
                        }
                    )
    in
        Html.div []
            [ Grid.toHtmlDiv ( tileSide, tileSide ) renderTile finalGrid
            , Html.p []
                [ (model.selectedTile |> toString |> Html.text)
                ]
            , Html.button [ onClick StepBuns ] [ text "Step" ]
            ]


renderTile : Coords -> FullTile -> Html Msg
renderTile coords tile =
    Html.div
        [ style
            [ ( "width", toString (tileSide) ++ "px" )
            , ( "height", toString (tileSide) ++ "px" )
            , ( "background-color", tile.back )
            , ( "border", "1px solid darkgray" )
            ]
        , onMouseEnter (SelectColumn coords)
        , onClick (Rotate coords)
        ]
        [ tile.content |> Maybe.map renderThingy |> Maybe.withDefault (text "")
        , Html.span
            [ class "debug", style [ ( "font-size", "25%" ) ] ]
            [ Html.text (toString coords) ]
        ]


{-| Render some ting insde tile
-}
renderThingy : Thingy -> Html msg
renderThingy thingy =
    case thingy of
        Bun str ->
            text str

        Obstacle ->
            Html.div
                [ style
                    [ ( "width", "26px" )
                    , ( "height", "26px" )
                    , ( "margin", "2px 2px" )
                    , ( "background-color", "black" )
                    , ( "border", "1px dotted darkgray" )
                    ]
                ]
                [ text "X" ]


tileSide : Int
tileSide =
    32


initialBuns : Grid Thingy
initialBuns =
    Grid.fromList
        [ ( ( 0, 0 ), "🍪" )
        , ( ( 1, 0 ), "🍪" )
        , ( ( 2, 0 ), "🍭" )
        , ( ( 3, 0 ), "🍪" )
        , ( ( 4, 0 ), "🍪" )
        , ( ( 4, -1 ), "🍩" )
        , ( ( 5, 0 ), "🍪" )
        ]
        |> Dict.map (\_ str -> Bun str)


outerFrame : Set Coords
outerFrame =
    (Grid.lineRect () { width = 8, height = 10 }
        |> Grid.translate ( -1, -8 )
        |> Dict.keys
        |> Set.fromList
    )


initialObstacles : Grid Thingy
initialObstacles =
    [ ( 1, -3 ), ( 4, -3 ), ( 3, -8 ), ( 4, -7 ), ( 5, -6 ) ] |> List.map (\c -> ( c, Obstacle )) |> Grid.fromList


back : Grid FullTile
back =
    Grid.drawBox { content = Nothing, back = "lightgray" } { width = 10, height = 12 }
        |> Grid.translate ( -2, -10 )
