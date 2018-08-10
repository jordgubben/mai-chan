module SweetBuns exposing (..)

import Random exposing (Seed)
import Set exposing (Set)
import Dict
import Time exposing (Time, second)
import Task
import Html exposing (Html, text)
import Html.Attributes as Att exposing (class, style)
import Html.Events as Ev exposing (onMouseEnter, onMouseLeave, onClick)
import Grid exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform InitSeed Time.now )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias FullTile =
    { content : Maybe Thingy
    , highlight : Bool
    , floor : FloorTile
    }


type alias Model =
    { selectedTile : Coords
    , things : Grid Thingy
    , turnCount : Int
    , seed : Seed
    }


type Msg
    = SelectColumn Coords
    | AdvancementClick
    | AdvancementTick Time
    | Rotate Coords
    | InitSeed Time


type Thingy
    = Bun String
    | Flour
    | Water
    | Obstacle


type FloorTile
    = PlainTile
    | Spawner Thingy
    | BunCollector
    | WallTile


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    , things = Dict.union initialBuns initialObstacles
    , turnCount = 0
    , seed = Random.initialSeed 0
    }



-- # UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitSeed time ->
            ( { model | seed = time |> Time.inMilliseconds |> round |> Random.initialSeed }
            , Cmd.none
            )

        SelectColumn coords ->
            ( { model | selectedTile = coords }, Cmd.none )

        AdvancementTick _ ->
            ( advanceThings model, Cmd.none )

        AdvancementClick ->
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
                ( { model | things = Dict.union tranformedThings remainingThings }
                , Cmd.none
                )


advanceThings : Model -> Model
advanceThings model =
    let
        activeThings =
            collectThings kitchenLevel model.things

        ( movers, obstacleThings ) =
            Dict.partition (always isMover) activeThings

        obstacleTiles =
            Set.union
                (kitchenLevel
                    |> Dict.filter (always isObstacleTile)
                    |> Dict.keys
                    |> Set.fromList
                )
                (obstacleThings |> Dict.keys |> Set.fromList)

        ( spawnedThings, seed_ ) =
            spawnSingelThingRnd model.seed kitchenLevel

        movers_ =
            moveAll obstacleTiles movers

        things_ =
            obstacleThings
                |> Dict.union spawnedThings
                |> Dict.union movers_
    in
        { model | things = things_, turnCount = model.turnCount + 1, seed = seed_ }


isMover : Thingy -> Bool
isMover thing =
    case thing of
        Obstacle ->
            False

        _ ->
            True


isObstacleTile : FloorTile -> Bool
isObstacleTile tile =
    case tile of
        WallTile ->
            True

        _ ->
            False


spawnThingEverywhere : Grid FloorTile -> Grid Thingy
spawnThingEverywhere level =
    findSpawnPoints level |> Grid.fromList


spawnSingelThingRnd : Seed -> Grid FloorTile -> ( Grid Thingy, Seed )
spawnSingelThingRnd seed level =
    let
        ( singlePoint, seed_ ) =
            pickRandom seed (findSpawnPoints level)
    in
        ( case singlePoint of
            Just ( coords, thing ) ->
                Grid.fromList [ ( coords, thing ) ]

            Nothing ->
                Grid.empty
        , seed_
        )


findSpawnPoints : Grid FloorTile -> List ( Coords, Thingy )
findSpawnPoints level =
    level
        |> Dict.toList
        |> List.filterMap
            (\( coords, tile ) ->
                case tile of
                    Spawner thing ->
                        Just ( coords, thing )

                    _ ->
                        Nothing
            )


pickRandom : Seed -> List a -> ( Maybe a, Seed )
pickRandom seed list =
    let
        ( pointsToDrop, seed_ ) =
            (Random.step (Random.int 0 (List.length list - 1)) seed)
    in
        ( list
            |> List.drop pointsToDrop
            |> List.head
        , seed_
        )


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


{-| Move all buns (if possible).
-}
moveAll : Set Coords -> Grid Thingy -> Grid Thingy
moveAll obstacles movers =
    List.foldl (moveSingle obstacles) movers (Dict.keys movers)


{-| Move a single
-}
moveSingle : Set Coords -> Coords -> Grid Thingy -> Grid Thingy
moveSingle obstacles ( x, y ) movers =
    case Grid.get ( x, y ) movers of
        Nothing ->
            movers

        Just activeMover ->
            let
                -- Can only move to a tile if it empty or if the mover is mixable with the current occupant
                canMoveTo nc =
                    if Set.member nc obstacles then
                        False
                    else
                        case (Dict.get nc movers) of
                            Just occupant ->
                                (mixIngredients activeMover occupant /= Nothing)

                            Nothing ->
                                True

                doMoveTo nc =
                    movers
                        |> Dict.remove ( x, y )
                        |> Dict.insert nc
                            (case (Dict.get nc movers) of
                                Just occupant ->
                                    (mixIngredients activeMover occupant |> Maybe.withDefault activeMover)

                                Nothing ->
                                    activeMover
                            )

                -- Can move Downm right or left
                viableMovementOptions =
                    List.filter canMoveTo [ ( x, y - 1 ), ( x + 1, y ), ( x - 1, y ) ]
            in
                List.head viableMovementOptions
                    |> Maybe.map doMoveTo
                    |> Maybe.withDefault movers


mixIngredients : Thingy -> Thingy -> Maybe Thingy
mixIngredients a b =
    case ( a, b ) of
        ( Water, Flour ) ->
            Just (Bun "🍞")

        ( Flour, Water ) ->
            Just (Bun "🍞")

        _ ->
            Nothing



-- # SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (2 * second) AdvancementTick



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
            , Html.div [ class "debug" ]
                [ Html.button [ onClick AdvancementClick ] [ text "Advance" ]
                , Html.p [] [ ("Turn count: " ++ (toString model.turnCount)) |> text ]
                , Html.p [] [ ("Random seed: " ++ (toString model.seed)) |> text ]
                , Html.p [] [ ("Selected tile: " ++ (toString model.selectedTile)) |> text ]
                ]
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

            Spawner _ ->
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

        Flour ->
            text "🌾"

        Water ->
            text "💧"

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
        [ ( ( 0, 0 ), Water )
        , ( ( 1, 0 ), Water )
        , ( ( 2, 0 ), Flour )
        , ( ( 3, 0 ), Flour )
        , ( ( 4, 0 ), Bun "🍪" )
        , ( ( 4, -1 ), Bun "🍩" )
        , ( ( 5, 0 ), Bun "🍪" )
        ]


initialObstacles : Grid Thingy
initialObstacles =
    [ ( 1, -3 ), ( 4, -3 ), ( 3, -8 ), ( 4, -7 ), ( 5, -6 ) ] |> List.map (\c -> ( c, Obstacle )) |> Grid.fromList


kitchenLevel : Grid FloorTile
kitchenLevel =
    kitchenFloor
        |> Dict.union waterSpawners
        |> Dict.union flourSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


waterSpawners : Grid FloorTile
waterSpawners =
    Grid.drawBox (Spawner Water) { width = 3, height = 1 }
        |> Grid.translate ( -1, 0 )


flourSpawners : Grid FloorTile
flourSpawners =
    Grid.drawBox (Spawner Flour) { width = 3, height = 1 }
        |> Grid.translate ( 4, 0 )


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
