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
    { selectedTile : Maybe Coords
    , things : Grid Thingy
    , turnCount : Int
    , moveCount : Int
    , seed : Seed
    }


type Msg
    = SelectTile Coords
    | TurnTick Time
    | Move
    | Spawn
    | Collect
    | InitSeed Time


type Thingy
    = Bun { sweet : Bool }
    | Flour { sweet : Bool }
    | Water { sweet : Bool }
    | Shuggar
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
    { selectedTile = Nothing
    , things = initialThings
    , turnCount = 0
    , moveCount = 0
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

        SelectTile coords ->
            case model.selectedTile of
                Just moverCoords ->
                    ( { model
                        | things =
                            attemptMove
                                (kitchenLevel
                                    |> Dict.filter (always isObstacleTile)
                                    |> Dict.keys
                                    |> Set.fromList
                                )
                                moverCoords
                                coords
                                model.things
                        , selectedTile = Nothing
                        , moveCount = model.moveCount + 1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | selectedTile = Just coords }, Cmd.none )

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


{-| Handle players attempt to move something
-}
attemptMove : Set Coords -> Coords -> Coords -> Grid Thingy -> Grid Thingy
attemptMove terrain from to things =
    if isValidMove from to && not (Set.member to terrain) then
        case ( Grid.get from things, Grid.get to things ) of
            ( Just thing, Nothing ) ->
                things
                    |> Dict.remove from
                    |> Dict.insert to thing

            ( Just someThing, Just otherThing ) ->
                mixIngredients someThing otherThing
                    |> Maybe.map
                        (\newThing ->
                            things |> Dict.remove from |> Dict.insert to newThing
                        )
                    |> Maybe.withDefault things

            ( Nothing, _ ) ->
                things
    else
        things


{-| Determine if a move from one place to another is valid, irrespective of environment.
-}
isValidMove : Coords -> Coords -> Bool
isValidMove from to =
    let
        ( fromX, fromY ) =
            from

        ( toX, toY ) =
            to

        ( dx, dy ) =
            ( toX - fromX, toY - fromY )
    in
        (-1 <= dx && dx <= 1) && (-1 <= dy && dy <= 1)


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
    if not (Set.member ( x, y - 1 ) obstacles) then
        case ( Grid.get ( x, y ) movers, Grid.get ( x, y - 1 ) movers ) of
            ( Nothing, _ ) ->
                movers

            ( Just activeMover, Nothing ) ->
                movers
                    |> Dict.remove ( x, y )
                    |> Dict.insert ( x, y - 1 ) activeMover

            ( Just activeMover, Just occupant ) ->
                mixIngredients activeMover occupant
                    |> Maybe.map
                        (\newThing ->
                            movers |> Dict.remove ( x, y ) |> Dict.insert ( x, y - 1 ) newThing
                        )
                    |> Maybe.withDefault movers
    else
        movers


mixIngredients : Thingy -> Thingy -> Maybe Thingy
mixIngredients a b =
    case ( a, b ) of
        ( Water taste1, Flour taste2 ) ->
            Just (Bun { sweet = taste1.sweet || taste2.sweet })

        ( Flour taste1, Water taste2 ) ->
            Just (Bun { sweet = taste1.sweet || taste2.sweet })

        ( Water _, Shuggar ) ->
            Just (Water { sweet = True })

        ( Shuggar, Water _ ) ->
            Just (Water { sweet = True })

        ( Flour _, Shuggar ) ->
            Just (Flour { sweet = True })

        ( Shuggar, Flour _ ) ->
            Just (Flour { sweet = True })

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
                            && isCollectableBun thing
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


isCollectableBun : Thingy -> Bool
isCollectableBun thing =
    case thing of
        Bun _ ->
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
        finalGrid : Grid RenderableTile
        finalGrid =
            kitchenLevel
                -- Convert to full tiles
                |> Dict.map (\_ floorTile -> RenderableTile Nothing False floorTile)
                -- Render highlighed column
                |> Dict.map
                    (\coords tile ->
                        if (Just coords == model.selectedTile) then
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
                , Html.p [] [ "Move count: " ++ (model.moveCount |> toString) |> text ]
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
        , onClick (SelectTile coords)
        ]
        [ tile.content |> Maybe.map renderThingy |> Maybe.withDefault (text "")
        , Html.span
            [ class "debug", style [ ( "font-size", "25%" ) ] ]
            [ Html.text (toString coords) ]
        ]


getTileColor : RenderableTile -> String
getTileColor tile =
    if tile.highlight then
        "aqua"
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
        Bun { sweet } ->
            if (sweet) then
                renderFoodStuff '🍪' "Sweet Bun"
            else
                renderFoodStuff '🍞' " Bun"

        Flour { sweet } ->
            if sweet then
                renderFoodStuff '🌾' "Sweet Flour"
            else
                renderFoodStuff '🌾' "Flour"

        Water { sweet } ->
            if (sweet) then
                renderFoodStuff '💧' " Sweet Water"
            else
                renderFoodStuff '💧' " Water"

        Shuggar ->
            renderFoodStuff '🍯' " Shuggar"

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


renderFoodStuff : Char -> String -> Html msg
renderFoodStuff icon str =
    Html.div []
        [ Html.span [ style [ ( "font-size", "150%" ) ] ] [ icon |> String.fromChar |> text ]
        , Html.span [ style [ ( "font-size", "50%" ) ] ] [ str |> text ]
        ]



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
        |> Dict.union shuggarSpawners
        |> Dict.union flourSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


waterSpawners : Grid FloorTile
waterSpawners =
    Grid.drawBox (Spawner (Water neutralTaste)) { width = 2, height = 1 }
        |> Grid.translate ( 0, 0 )


shuggarSpawners : Grid FloorTile
shuggarSpawners =
    Grid.drawBox (Spawner Shuggar) { width = 2, height = 1 }
        |> Grid.translate ( 2, 0 )


flourSpawners : Grid FloorTile
flourSpawners =
    Grid.drawBox (Spawner (Flour neutralTaste)) { width = 2, height = 1 }
        |> Grid.translate ( 4, 0 )


neutralTaste : { sweet : Bool }
neutralTaste =
    { sweet = False }


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
