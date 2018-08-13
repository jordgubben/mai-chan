module SweetBuns exposing (..)

import Random exposing (Seed)
import Set exposing (Set)
import Dict
import Time exposing (Time, second)
import Delay
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


type alias Model =
    { selectedTile : Coords
    , things : Grid Thingy
    , turnCount : Int
    , seed : Seed
    }


type Msg
    = SelectColumn Coords
    | TurnTick Time
    | Move
    | Spawn
    | Collect
    | Rotate Coords
    | InitSeed Time


type Thingy
    = Bun
    | Flour
    | Water
    | Obstacle


type FloorTile
    = PlainTile
    | Spawner Thingy
    | BunCollector
    | WallTile


{-| Complete tile used produced when rendering
-}
type alias RenderableTile =
    { content : Maybe Thingy
    , highlight : Bool
    , floor : FloorTile
    }


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    , things = initialThings
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

        TurnTick _ ->
            ( { model | turnCount = model.turnCount + 1 }, Delay.after 0 second Spawn )

        Spawn ->
            ( spawnThings model, Delay.after 1 second Move )

        Move ->
            ( { model
                | things = moveThings model.things
              }
            , Delay.after 1 second Collect
            )

        Collect ->
            ( { model
                | things = collectThings kitchenLevel model.things
              }
            , Cmd.none
            )

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


isGameOver : Grid FloorTile -> Grid Thingy -> Bool
isGameOver floor things =
    let
        spawnPoints : Set Coords
        spawnPoints =
            findSpawnPoints floor
                |> List.map Tuple.first
                |> Set.fromList

        occupants : Set Coords
        occupants =
            things
                |> Dict.keys
                |> Set.fromList
    in
        Set.diff
            spawnPoints
            occupants
            |> Set.isEmpty


{-| Spawn ingredients at spawn points
-}
spawnThings : Model -> Model
spawnThings model =
    let
        ( spawnedThings, seed_ ) =
            spawnSingelThingRnd model.seed kitchenLevel (model.things |> Dict.keys |> Set.fromList)

        things_ =
            Dict.union spawnedThings model.things
    in
        { model | things = things_, seed = seed_ }


spawnSingelThingRnd : Seed -> Grid FloorTile -> Set Coords -> ( Grid Thingy, Seed )
spawnSingelThingRnd seed level spawnObstacles =
    let
        -- Pick a random unoccupied spawners coordinates
        ( singlePoint, seed_ ) =
            pickRandom seed
                (findSpawnPoints level
                    |> List.filter (Tuple.first >> (flip Set.member) spawnObstacles >> not)
                )
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


{-| Auto-move things around, poretially causing mixing of ingredients
-}
moveThings : Grid Thingy -> Grid Thingy
moveThings things =
    let
        ( movers, obstacleThings ) =
            Dict.partition (always isMover) things

        obstacleTiles =
            Set.union
                (kitchenLevel
                    |> Dict.filter (always isObstacleTile)
                    |> Dict.keys
                    |> Set.fromList
                )
                (obstacleThings |> Dict.keys |> Set.fromList)

        movers_ =
            moveAllMovers obstacleTiles movers
    in
        obstacleThings
            |> Dict.union movers_


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


{-| Move all buns (if possible).
-}
moveAllMovers : Set Coords -> Grid Thingy -> Grid Thingy
moveAllMovers obstacles movers =
    List.foldl (moveSingleMover obstacles) movers (Dict.keys movers)


{-| Move a single thing
-}
moveSingleMover : Set Coords -> Coords -> Grid Thingy -> Grid Thingy
moveSingleMover obstacles ( x, y ) movers =
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
            Just Bun

        ( Flour, Water ) ->
            Just Bun

        _ ->
            Nothing


collectThings : Grid FloorTile -> Grid Thingy -> Grid Thingy
collectThings level things =
    let
        collectionPoints =
            level |> Dict.filter (\_ tile -> isBunCollector tile) |> Dict.keys |> Set.fromList

        -- Collect thing if it is a _bun_ on a collection point
        ( collectedThings, remainingThings ) =
            things
                |> Dict.partition
                    (\coords thing ->
                        (Set.member coords collectionPoints)
                            && thing
                            == Bun
                    )
    in
        remainingThings


isBunCollector : FloorTile -> Bool
isBunCollector floorTile =
    case floorTile of
        BunCollector ->
            True

        _ ->
            False



-- # SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if not <| isGameOver kitchenLevel model.things then
        Time.every turnDuration TurnTick
    else
        Sub.none


turnDuration : Time
turnDuration =
    (3 * second)



-- # VIEW


view : Model -> Html Msg
view model =
    let
        ( selX, selY ) =
            model.selectedTile

        finalGrid : Grid RenderableTile
        finalGrid =
            kitchenLevel
                -- Convert to full tiles
                |> Dict.map (\_ floorTile -> RenderableTile Nothing False floorTile)
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
                [ Html.button [ onClick Spawn ] [ text "Spawn!" ]
                , Html.button [ onClick Move ] [ text "Move!" ]
                , Html.button [ onClick Collect ] [ text "Collect" ]
                , Html.p [] [ ("Turn count: " ++ (toString model.turnCount)) |> text ]
                , Html.p [] [ ("Random seed: " ++ (toString model.seed)) |> text ]
                , Html.p [] [ "Selected tile: " ++ (model.selectedTile |> toString) |> text ]
                , Html.p [] [ "Game over? " ++ ((isGameOver kitchenLevel model.things) |> toString) |> text ]
                ]
            ]


renderTile : Coords -> RenderableTile -> Html Msg
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


getTileColor : RenderableTile -> String
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
        Bun ->
            Html.span [ style [ ( "font-size", "200%" ) ] ] [ text "🍞" ]

        Flour ->
            Html.span [ style [ ( "font-size", "200%" ) ] ] [ text "🌾" ]

        Water ->
            Html.span [ style [ ( "font-size", "200%" ) ] ] [ text "💧" ]

        Obstacle ->
            Html.div
                [ style
                    [ ( "width", "50px" )
                    , ( "height", "50px" )
                    , ( "margin", "4px 4px" )
                    , ( "background-color", "black" )
                    , ( "border", "2px dotted darkgray" )
                    ]
                ]
                []



-- LEVEL


tileSide : Int
tileSide =
    64


initialThings : Grid Thingy
initialThings =
    initialObstacles


initialObstacles : Grid Thingy
initialObstacles =
    [ ( 2, -2 )
    , ( 2, -3 )
    , ( 3, -2 )
    , ( 3, -3 )
    ]
        |> List.map (\c -> ( c, Obstacle ))
        |> Grid.fromList


kitchenLevel : Grid FloorTile
kitchenLevel =
    kitchenFloor
        |> Dict.union waterSpawners
        |> Dict.union flourSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


waterSpawners : Grid FloorTile
waterSpawners =
    Grid.drawBox (Spawner Water) { width = 2, height = 1 }
        |> Grid.translate ( 0, 0 )


flourSpawners : Grid FloorTile
flourSpawners =
    Grid.drawBox (Spawner Flour) { width = 2, height = 1 }
        |> Grid.translate ( 4, 0 )


kitchenCollectors : Grid FloorTile
kitchenCollectors =
    Grid.drawBox BunCollector { width = 2, height = 1 }
        |> Grid.translate ( 2, -5 )


kitchenFloor : Grid FloorTile
kitchenFloor =
    Grid.drawBox PlainTile { width = 6, height = 6 }
        |> Grid.translate ( 0, -5 )


kitchenWalls : Grid FloorTile
kitchenWalls =
    Grid.lineRect WallTile { width = 8, height = 8 }
        |> Grid.translate ( -1, -6 )
