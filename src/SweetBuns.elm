module SweetBuns exposing (..)

import Random exposing (Seed)
import Set exposing (Set)
import Dict
import Time exposing (Time, second)
import Delay
import Task
import Html exposing (Html, text)
import Html.Attributes as Att exposing (id, class, style)
import Html.Events as Ev exposing (onMouseEnter, onMouseLeave, onClick)
import Sprite exposing (Sprite)
import Grid exposing (..)
import Thingy exposing (Thingy(..), Flavour(..))


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform IntiGame Time.now )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Complete game model
-}
type alias Model =
    { selectedTile : Maybe Coords
    , things : Grid Thingy
    , moveCount : Int
    , totalScore : Int
    , seed : Seed
    }


initialModel : Model
initialModel =
    { selectedTile = Nothing
    , things = initialThings
    , moveCount = 0
    , totalScore = 0
    , seed = Random.initialSeed 0
    }


{-| Overall game messages
-}
type Msg
    = SelectTile Coords
    | Fall
    | Spawn
    | Collect
    | IntiGame Time
    | RestartGame


type FloorTile
    = PlainTile
    | Spawner (List Thingy)
    | BunCollector
    | WallTile


{-| Complete tile used produced when rendering
-}
type alias RenderableTile =
    { content : Maybe Thingy
    , highlight : Bool
    , floor : FloorTile
    }



-- # UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IntiGame time ->
            ( initGame time model, Cmd.none )

        RestartGame ->
            ( restartGame model, Cmd.none )

        SelectTile coords ->
            if not <| isGameOver kitchenLevel model.things then
                selectTile coords model
            else
                ( model, Cmd.none )

        Spawn ->
            ( spawnThings model, Cmd.none )

        Fall ->
            ( { model
                | things = applyGravity kitchenLevel model.things
              }
            , Delay.after 1 second Collect
            )

        Collect ->
            let
                ( things_, scoreIncrement ) =
                    collectThings kitchenLevel model.things
            in
                ( { model
                    | things = things_
                    , totalScore = model.totalScore + scoreIncrement
                  }
                , Cmd.none
                )


{-| Start game with a half full board.
-}
initGame : Time -> Model -> Model
initGame time model =
    let
        seed =
            time |> Time.inMilliseconds |> round |> Random.initialSeed

        ( newThings, seed_ ) =
            fillBoard seed
    in
        { model | seed = seed_, things = Dict.union model.things newThings }


restartGame : Model -> Model
restartGame model =
    let
        ( newThings, seed_ ) =
            fillBoard model.seed
    in
        { model
            | seed = seed_
            , things = Dict.union initialThings newThings
            , moveCount = 0
            , totalScore = 0
        }


{-| Fill board to the brim with various things
-}
fillBoard : Seed -> ( Grid Thingy, Seed )
fillBoard seed =
    let
        filledRegion =
            { boardSize | height = boardSize.height - 1 }

        --
        -- Start with more Water and Flour than Flavouring
        filler =
            [ Water Nothing
            , Water Nothing
            , Flour Nothing
            , Flour Nothing
            , Flavouring Sugar
            , Flavouring Chilli
            , Flavouring Chocolate
            ]
    in
        Grid.drawBox () filledRegion
            |> Grid.translate ( 0, 0 - filledRegion.height )
            |> Dict.foldl
                (\coords _ ( things, seed ) ->
                    let
                        ( newThing, seed_ ) =
                            pickRandom seed filler
                    in
                        ( Grid.put coords (newThing |> Maybe.withDefault Obstacle) things
                        , seed_
                        )
                )
                ( Grid.empty, seed )


{-| Handle players selection of a tile.
If no tile has been selcted, then just select it.
If a tile is already selected, then attempt to move.
-}
selectTile : Coords -> Model -> ( Model, Cmd Msg )
selectTile coords model =
    case model.selectedTile of
        Just moverCoords ->
            let
                things_ =
                    attemptMove
                        (kitchenLevel
                            |> Dict.filter (always isObstacleTile)
                            |> Dict.keys
                            |> Set.fromList
                        )
                        moverCoords
                        coords
                        model.things
            in
                ( { model
                    | things = things_
                    , selectedTile = Nothing
                    , moveCount = model.moveCount + 1
                  }
                , if model.things /= things_ then
                    Delay.after 0.25 second Spawn
                  else
                    Cmd.none
                )

        Nothing ->
            ( { model | selectedTile = Just coords }, Cmd.none )


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
        ( spawnedThing, seed_ ) =
            spawnSingelThingRnd model.seed kitchenLevel (model.things |> Dict.keys |> Set.fromList)

        things_ =
            spawnedThing
                |> Maybe.map
                    (\( coords, thing ) ->
                        Dict.insert coords thing model.things
                    )
                |> Maybe.withDefault model.things
    in
        { model | things = things_, seed = seed_ }


spawnSingelThingRnd : Seed -> Grid FloorTile -> Set Coords -> ( Maybe ( Coords, Thingy ), Seed )
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
            Just ( coords, things ) ->
                let
                    ( rndThing, seed__ ) =
                        pickRandom seed_ things
                in
                    rndThing
                        |> Maybe.map (\t -> Just ( coords, t ))
                        |> Maybe.withDefault Nothing

            Nothing ->
                Nothing
        , seed_
        )


findSpawnPoints : Grid FloorTile -> List ( Coords, List Thingy )
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


{-| Pick a random element from a list
-}
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
        let
            things_ =
                move from to things
        in
            -- If movement could not be prefomed in any other way,
            -- Then let the things teade places
            if things == things_ then
                Grid.swap from to things
            else
                things_
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


isStable : Grid FloorTile -> Grid Thingy -> Bool
isStable floor things =
    things == applyGravity floor things


{-| Let things fall down where possible, poretially causing mixing of ingredients.
-}
applyGravity : Grid FloorTile -> Grid Thingy -> Grid Thingy
applyGravity level things =
    let
        ( fallers, obstacleThings ) =
            Dict.partition (always Thingy.isFaller) things

        obstacles : Set Coords
        obstacles =
            Set.union
                (level
                    |> Dict.filter (always isObstacleTile)
                    |> Dict.keys
                    |> Set.fromList
                )
                (obstacleThings |> Dict.keys |> Set.fromList)

        fallers_ =
            List.foldl
                (\( x, y ) m ->
                    if not (Set.member ( x, y - 1 ) obstacles) then
                        move ( x, y ) ( x, y - 1 ) m
                    else
                        m
                )
                fallers
                (Dict.keys fallers)
    in
        Dict.union
            fallers_
            obstacleThings


isObstacleTile : FloorTile -> Bool
isObstacleTile tile =
    case tile of
        WallTile ->
            True

        _ ->
            False


{-| If possible, move thing from one set of coords to another.
Will mix ingredients if possible.
If move can not be preformed, then the original unchanged grid is returned.
-}
move : Coords -> Coords -> Grid Thingy -> Grid Thingy
move from to things =
    case ( Grid.get from things, Grid.get to things ) of
        ( Nothing, _ ) ->
            things

        ( Just activeMover, Nothing ) ->
            things
                |> Dict.remove from
                |> Dict.insert to activeMover

        ( Just activeMover, Just occupant ) ->
            Thingy.mixIngredients activeMover occupant
                |> Maybe.map
                    (\newThing ->
                        things |> Dict.remove from |> Dict.insert to newThing
                    )
                |> Maybe.withDefault things


{-| Collect all collectable Buns on collectors.
-}
collectThings : Grid FloorTile -> Grid Thingy -> ( Grid Thingy, Int )
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
                            && Thingy.isCollectableBun thing
                    )
    in
        ( remainingThings, 100 * (Dict.size collectedThings) )


isBunCollector : FloorTile -> Bool
isBunCollector floorTile =
    case floorTile of
        BunCollector ->
            True

        _ ->
            False



-- # SUBSCRIPTIONS


{-| Keep updating regularly while game is running.

    - Spawn new things now and then
    - Let things fall (if anything could fall)

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if not <| isGameOver kitchenLevel model.things then
        Sub.batch
            ([ Time.every spawnInterval (\_ -> Spawn) ]
                -- Only Have things fall there is something thst could fall (save messages)
                ++ (if not <| isStable kitchenLevel model.things then
                        [ Time.every fallInterval (\_ -> Fall) ]
                    else
                        []
                   )
            )
    else
        Sub.none


spawnInterval : Time
spawnInterval =
    (3 * second)


fallInterval : Time
fallInterval =
    (0.25 * second)



-- # VIEW


{-| View the entire game, including UI and debug.
-}
view : Model -> Html Msg
view model =
    Html.div
        [ id "game-container"
        , style
            [ ( "position", "relative" )
            , ( "width", (6 * tileSide |> toString) ++ "px" )
            , ( "height", (8 * tileSide |> toString) ++ "px" )
            , ( "border", "10px solid black" )
            ]
        ]
        [ viewScore model.totalScore
        , viewBoardContainer model
        , viewDebug model
        ]


viewScore : Int -> Html msg
viewScore score =
    Html.div
        [ id "score"
        , style
            [ ( "position", "absolute" )
            , ( "top", 0 ) |> px
            , ( "font-size", tileSide - 4 ) |> px
            ]
        ]
        [ Html.span
            [ class "label"
            , style
                [ ( "display", "inline-block" )
                , ( "width", 2 * tileSide ) |> px
                , ( "padding", "10px 0" )
                , ( "text-align", "left" )
                ]
            ]
            [ text "Score"
            ]
        , Html.span
            [ class "value"
            , style
                [ ( "display", "inline-block" )
                , ( "width", 4 * tileSide ) |> px
                , ( "padding", "10px 0" )
                , ( "text-align", "right" )
                ]
            ]
            [ text <| (toString score)
            ]
        ]


viewBoardContainer : Model -> Html Msg
viewBoardContainer model =
    Html.div
        [ id "board-container"
        , style
            [ ( "position", "absolute" )
            , ( "top", (1 * tileSide |> toString) ++ "px" )
            , ( "left", "0" )
            ]
        ]
        [ viewBoard model
        , if isGameOver kitchenLevel model.things then
            viewGameOver
          else
            text ""
        ]


{-| View the content of the game board
-}
viewBoard : Model -> Html Msg
viewBoard model =
    let
        finalGrid : Grid RenderableTile
        finalGrid =
            Grid.drawBox (PlainTile) boardSize
                |> Grid.translate ( 0, 1 - boardSize.height )
                |> Dict.intersect kitchenLevel
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
        Grid.toHtmlDiv ( tileSide, tileSide ) renderTile finalGrid


{-| View the 'Game over' modal dialog box
-}
viewGameOver : Html Msg
viewGameOver =
    Html.div
        [ id "game-over"
        , style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "width", "90%" )
            , ( "height", "90%" )
            , ( "padding", "5%" )
            , ( "color", "darkred" )
            , ( "text-align", "center" )
            , ( "background-color", "white" )
            , ( "opacity", "0.75" )
            ]
        ]
        [ Html.h1 [ style [ ( "opacity", "1" ) ] ] [ text "Game over" ]
        , Html.a
            [ style [ ( "opacity", "1" ) ]
            , Att.href "#restart"
            , onClick RestartGame
            ]
            [ text "Try again?" ]
        ]


{-| View various debug information.

    (Hidden using css-tricks in thw final game)

-}
viewDebug : Model -> Html Msg
viewDebug model =
    Html.div
        [ class "debug"
        , style
            [ ( "position", "fixed" )
            , ( "bottom", "0" )
            , ( "right", "0" )
            , ( "width", "100%" )
            , ( "height", (4 * tileSide |> toString) ++ "px" )
            , ( "background-color", "lightgray" )
            ]
        ]
        [ Html.button [ onClick Spawn ] [ text "Spawn!" ]
        , Html.button [ onClick Fall ] [ text "Fall!" ]
        , Html.button [ onClick Collect ] [ text "Collect" ]
        , Html.p [] [ "Move count: " ++ (model.moveCount |> toString) |> text ]
        , Html.p [] [ "Random seed: " ++ (toString model.seed) |> text ]
        , Html.p [] [ "Selected tile: " ++ (model.selectedTile |> toString) |> text ]
        , Html.p [] [ "Game over? " ++ ((isGameOver kitchenLevel model.things) |> toString) |> text ]
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
        [ tile.content |> Maybe.map Thingy.toHtml |> Maybe.withDefault (text "")
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


{-| Render the most basic sprite
-}
renderSprite : String -> Sprite a -> Html msg
renderSprite cssClass sprite =
    Html.span
        [ class <| cssClass ++ "-sprite"
        , style <| Sprite.sprite sprite
        ]
        []


px : ( String, Int ) -> ( String, String )
px ( name, value ) =
    ( name, (value |> toString) ++ "px" )



-- LEVEL


tileSide : Int
tileSide =
    Thingy.spriteSide


initialThings : Grid Thingy
initialThings =
    Grid.empty


kitchenLevel : Grid FloorTile
kitchenLevel =
    kitchenFloor
        |> Dict.union kitchenSpawners
        |> Dict.union kitchenCollectors
        |> Dict.union kitchenWalls


kitchenSpawners : Grid FloorTile
kitchenSpawners =
    Grid.drawBox (Spawner spawnables) { width = 6, height = 1 }
        |> Grid.translate ( 0, 0 )


{-| Spawnable things.
Weighted so that Flour, Water and Flavourings spawn 1/3 of the time each.
(Thus preventing an abundance of Flavourings)
-}
spawnables : List Thingy
spawnables =
    List.concat
        [ spawnableFlavours
        , List.repeat (List.length spawnableFlavours) (Water Nothing)
        , List.repeat (List.length spawnableFlavours) (Flour Nothing)
        ]


spawnableFlavours : List Thingy
spawnableFlavours =
    [ Flavouring Sugar, Flavouring Chilli, Flavouring Chocolate ]


kitchenCollectors : Grid FloorTile
kitchenCollectors =
    Grid.drawBox BunCollector { boardSize | height = 1 }
        |> Grid.translate ( 0, -5 )


kitchenFloor : Grid FloorTile
kitchenFloor =
    Grid.drawBox PlainTile { width = 6, height = 6 }
        |> Grid.translate ( 0, -5 )


kitchenWalls : Grid FloorTile
kitchenWalls =
    Grid.lineRect WallTile { width = 8, height = 8 }
        |> Grid.translate ( -1, -6 )


boardSize : Size Int
boardSize =
    Size 6 6
