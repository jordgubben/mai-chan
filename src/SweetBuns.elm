module SweetBuns exposing (..)

import Set exposing (Set)
import Dict
import Time exposing (Time, second)
import Html exposing (Html, text)
import Html.Attributes as Att exposing (class, style)
import Html.Events as Ev exposing (onMouseEnter, onMouseLeave, onClick)
import Grid exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none ), update = update, view = view, subscriptions = subscriptions }


type alias FullTile =
    { content : Maybe Thingy
    , highlight : Bool
    , floor : FloorTile
    }


type alias Model =
    { selectedTile : Coords
    , things : Grid Thingy
    }


type Msg
    = SelectColumn Coords
    | StepBuns
    | Rotate Coords
    | Tick Time


type Thingy
    = Bun String
    | Obstacle


type FloorTile
    = PlainTile
    | BunSpawner
    | BunCollector
    | WallTile


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    , things = Dict.union initialBuns initialObstacles
    }



-- # UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectColumn coords ->
            ( { model | selectedTile = coords }, Cmd.none )

        Tick _ ->
            ( advanceThings model, Cmd.none )

        StepBuns ->
            ( advanceThings model, Cmd.none )

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
                ( { model | things = Dict.union tranformedThings remainingThings }, Cmd.none )


advanceThings : { a | things : Grid Thingy } -> { a | things : Grid Thingy }
advanceThings model =
    let
        activeThings =
            collectThings kitchenLevel model.things

        ( buns, obstacles ) =
            Dict.partition (always isBun) activeThings

        terrain =
            Set.union
                (kitchenLevel
                    |> Dict.filter
                        (\_ tile ->
                            case tile of
                                WallTile ->
                                    True

                                _ ->
                                    False
                        )
                    |> Dict.keys
                    |> Set.fromList
                )
                (obstacles |> Dict.keys |> Set.fromList)

        buns_ =
            step terrain buns

        things_ =
            obstacles
                |> Dict.union (spawnThings (Bun "🍬") kitchenLevel)
                |> Dict.union buns_
    in
        { model | things = things_ }


isBun : Thingy -> Bool
isBun thing =
    case thing of
        Bun _ ->
            True

        _ ->
            False


spawnThings : Thingy -> Grid FloorTile -> Grid Thingy
spawnThings thing level =
    let
        spawnPoints : List Coords
        spawnPoints =
            level
                |> Dict.filter (\_ tile -> isBunSpawner tile)
                |> Dict.keys
    in
        spawnPoints |> List.map (\point -> ( point, thing )) |> Grid.fromList


isBunSpawner : FloorTile -> Bool
isBunSpawner floorTile =
    case floorTile of
        BunSpawner ->
            True

        _ ->
            False


collectThings : Grid FloorTile -> Grid Thingy -> Grid Thingy
collectThings level things =
    let
        collectorsPoints =
            level |> Dict.filter (\_ tile -> isBunCollector tile) |> Dict.keys |> Set.fromList

        remainingThings =
            things |> Dict.filter (\coords _ -> not <| Set.member coords collectorsPoints)
    in
        remainingThings


isBunCollector : FloorTile -> Bool
isBunCollector floorTile =
    case floorTile of
        BunCollector ->
            True

        _ ->
            False


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



-- # SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (2 * second) Tick



-- # VIEW


view : Model -> Html Msg
view model =
    let
        ( selX, selY ) =
            model.selectedTile

        finalGrid : Grid FullTile
        finalGrid =
            kitchenLevel
                -- Convert to full tiles
                |> Dict.map (\_ floorTile -> FullTile Nothing False floorTile)
                -- Render highlighed column
                |> Dict.map
                    (\( x, y ) tile ->
                        if (x == selX || x == selX + 1) && (y == selY || y == selY + 1) then
                            { tile | highlight = True }
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
            , ( "background-color", getTileColor tile )
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


getTileColor : FullTile -> String
getTileColor tile =
    if tile.highlight then
        "yellow"
    else
        case tile.floor of
            PlainTile ->
                "lightgray"

            BunSpawner ->
                "antiquewhite"

            BunCollector ->
                "lawngreen"

            WallTile ->
                "darkgray"


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


initialObstacles : Grid Thingy
initialObstacles =
    [ ( 1, -3 ), ( 4, -3 ), ( 3, -8 ), ( 4, -7 ), ( 5, -6 ) ] |> List.map (\c -> ( c, Obstacle )) |> Grid.fromList


kitchenLevel : Grid FloorTile
kitchenLevel =
    kitchenFloor
        |> Dict.union kitchenSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


kitchenSpawners : Grid FloorTile
kitchenSpawners =
    Grid.drawBox BunSpawner { width = 2, height = 1 }
        |> Grid.translate ( -1, 0 )


kitchenCollectors : Grid FloorTile
kitchenCollectors =
    Grid.drawBox BunCollector { width = 2, height = 1 }
        |> Grid.translate ( 5, -9 )


kitchenFloor : Grid FloorTile
kitchenFloor =
    Grid.drawBox PlainTile { width = 10, height = 12 }
        |> Grid.translate ( -2, -10 )


kitchenWalls : Grid FloorTile
kitchenWalls =
    Grid.lineRect WallTile { width = 10, height = 12 }
        |> Grid.translate ( -2, -10 )
