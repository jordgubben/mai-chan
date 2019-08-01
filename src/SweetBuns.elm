module SweetBuns exposing
    ( Board
    , applyGravity
    , attemptMove
    , collectThings
    , isGameOver
    , isStable
    , shouldFall
    , spawnSingelThingRnd
    )

import Browser
import Delay exposing (TimeUnit(..))
import Dict
import FloorTile exposing (FloorTile(..))
import Grid exposing (..)
import Html exposing (Html, text)
import Html.Attributes as Att exposing (class, id, style)
import Html.Events exposing (onClick, onDoubleClick)
import Kitchen exposing (level)
import Random exposing (Seed)
import Set exposing (Set)
import String exposing (fromFloat, fromInt)
import Task
import Thingy exposing (Flavour(..), Thingy(..))
import Time exposing (Posix, toSecond)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Task.perform IntiGame Time.now )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Complete game model.
-}
type alias Model =
    { selectedTile : Maybe Coords
    , things : Grid Thingy
    , moveCount : Int
    , totalScore : Int
    , seed : Seed
    }


{-| Subset relevant to most operations.
-}
type alias Board =
    { things : Grid Thingy
    , floor : Grid FloorTile
    }


initialModel : Model
initialModel =
    { selectedTile = Nothing
    , things = Kitchen.level.things
    , moveCount = 0
    , totalScore = 0
    , seed = Random.initialSeed 0
    }


{-| Overall game messages.
-}
type Msg
    = SelectTile Coords
    | ActivateTile Coords
    | Fall
    | Spawn
    | Collect
    | IntiGame Posix
    | RestartGame


{-| Complete tile used produced when rendering.
-}
type alias RenderableTile =
    { content : Maybe Thingy
    , highlight : Maybe Highlight
    , floor : FloorTile
    }


type Highlight
    = SelectedMover
    | PossibleMovementDestination
    | ForbiddenMovementDestination
    | WillFall



-- # UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IntiGame time ->
            ( initGame time model, Cmd.none )

        RestartGame ->
            ( restartGame model, Cmd.none )

        SelectTile coords ->
            if not <| isGameOver { floor = level.floor, things = model.things } then
                selectTile coords model

            else
                ( model, Cmd.none )

        ActivateTile coords ->
            if not <| isGameOver { floor = level.floor, things = model.things } then
                ( { model | things = activateTile coords model.things }, Cmd.none )

            else
                ( model, Cmd.none )

        Spawn ->
            ( spawnThings model, Cmd.none )

        Fall ->
            ( { model
                | things =
                    applyGravity
                        (FloorTile.obstacleTileArea level.floor)
                        model.things
              }
            , Delay.after 1 Second Collect
            )

        Collect ->
            let
                ( things_, scoreIncrement ) =
                    collectThings { floor = level.floor, things = model.things }
            in
            ( { model
                | things = things_
                , totalScore = model.totalScore + scoreIncrement
              }
            , Cmd.none
            )


{-| Start a first game with a half full board.
-}
initGame : Posix -> Model -> Model
initGame time model =
    let
        seed =
            time |> Time.posixToMillis |> Random.initialSeed

        ( newThings, seed_ ) =
            fillBoard seed
    in
    { model | seed = seed_, things = Dict.union model.things newThings }


{-| Start another game after a game over
-}
restartGame : Model -> Model
restartGame model =
    let
        ( newThings, seed_ ) =
            fillBoard model.seed
    in
    { model
        | seed = seed_
        , things = Dict.union level.things newThings
        , moveCount = 0
        , totalScore = 0
    }


{-| Fill board to the brim with various things
-}
fillBoard : Seed -> ( Grid Thingy, Seed )
fillBoard fillSeed =
    let
        boardSize =
            level.size

        filledRegion =
            { boardSize | height = level.size.height - 1 }

        -- Start with more Water and Flour than Flavouring
        filler =
            [ Water Nothing
            , Water Nothing
            , Flour Nothing
            , Flour Nothing
            , Flavouring { flavour = Sugar, packaged = True }
            , Flavouring { flavour = Chilli, packaged = True }
            , Flavouring { flavour = Chocolate, packaged = True }
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
            ( Grid.empty, fillSeed )


{-| Handle players selection of a tile (triggred by clicking).

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
                        moverCoords
                        coords
                        { floor = level.floor, things = model.things }
            in
            ( { model
                | things = things_
                , selectedTile = Nothing
                , moveCount = model.moveCount + 1
              }
            , if model.things /= things_ then
                Delay.after 0.25 Second Spawn

              else
                Cmd.none
            )

        Nothing ->
            ( { model | selectedTile = Just coords }, Cmd.none )


{-| Activate a Thingy (triggerd by double clicking)

Acitvates the thing at given coordinates
(but only if there is something there).

-}
activateTile : Coords -> Grid Thingy -> Grid Thingy
activateTile coords things =
    Dict.get coords things
        |> Maybe.map
            (\t ->
                case t of
                    -- Unpack flavourings
                    Flavouring f ->
                        Flavouring { f | packaged = False }

                    -- .. otherwise do nothing
                    _ ->
                        t
            )
        |> Maybe.map (\t -> Grid.put coords t things)
        |> Maybe.withDefault things


{-| Detect game over.

The game is over if all spawn points are still covered after things have fallen.

-}
isGameOver : Board -> Bool
isGameOver { floor, things } =
    let
        spawnPoints : Set Coords
        spawnPoints =
            FloorTile.spawnPoints floor
                |> List.map Tuple.first
                |> Set.fromList

        occupants : Set Coords
        occupants =
            things
                |> Dict.keys
                |> Set.fromList

        allSpawnersOccupied =
            Set.diff
                spawnPoints
                occupants
                |> Set.isEmpty

        boardStable =
            isStable (FloorTile.obstacleTileArea floor) things
    in
    allSpawnersOccupied && boardStable


{-| Spawn ingredients at spawn points.
-}
spawnThings : Model -> Model
spawnThings model =
    let
        ( spawnedThing, seed_ ) =
            spawnSingelThingRnd model.seed level.floor (model.things |> Dict.keys |> Set.fromList)

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
                (FloorTile.spawnPoints level
                    |> List.filter (Tuple.first >> (\b a -> Set.member a b) spawnObstacles >> not)
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


{-| Pick a random element from a list.
-}
pickRandom : Seed -> List a -> ( Maybe a, Seed )
pickRandom seed list =
    let
        ( pointsToDrop, seed_ ) =
            Random.step (Random.int 0 (List.length list - 1)) seed
    in
    ( list
        |> List.drop pointsToDrop
        |> List.head
    , seed_
    )


{-| Check if a move is possible, so that it can be highlighted properly.
-}
isPossibleMove : Board -> Coords -> Coords -> Bool
isPossibleMove board from to =
    attemptMove from to board /= board.things


{-| Handle players attempt to move something.
-}
attemptMove : Coords -> Coords -> Board -> Grid Thingy
attemptMove from to { floor, things } =
    -- Only move if move is withing movement pattern
    if not (isValidMove from to) then
        things
        -- Only move if not into terrain

    else if
        Grid.get to floor
            |> Maybe.map FloorTile.isObstacleTile
            |> Maybe.withDefault False
    then
        things
        -- If movement is possible, do it
        -- (might mix things)

    else if moveMixing from to things /= things then
        moveMixing from to things
        -- If movement could not be prefomed in any other way,
        -- Then let the things trade places if that causes a chain reaction

    else if not (Grid.swap from to things |> isStable (FloorTile.obstacleTileArea floor)) then
        Grid.swap from to things
        -- Else change nothing

    else
        things


{-| Determine if a move is valid based only on coordinates, ignoring Board state.
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
    dx == 0 || dy == 0


isStable : Set Coords -> Grid Thingy -> Bool
isStable terrain things =
    things == applyGravity terrain things


{-| Let things fall where possible, potetially causing mixing of ingredients.
-}
applyGravity : Set Coords -> Grid Thingy -> Grid Thingy
applyGravity terrain things =
    let
        ( fallers, nonFallers ) =
            partitionFallers
                { things = things
                , floor =
                    -- Recreate wall tiles (refactor potential)
                    terrain
                        |> Set.toList
                        |> List.map (\c -> ( c, WallTile ))
                        |> Grid.fromList
                }

        obstacles : Set Coords
        obstacles =
            Set.union
                terrain
                (nonFallers |> Dict.keys |> Set.fromList)

        fallers_ =
            List.foldl
                (\( x, y ) m ->
                    if not (Set.member ( x, y - 1 ) obstacles) then
                        moveMixing ( x, y ) ( x, y - 1 ) m

                    else
                        m
                )
                fallers
                (Dict.keys fallers)
    in
    -- Join things again, with updated falers overwriting nonFallers
    Dict.union
        fallers_
        nonFallers


partitionFallers : Board -> ( Grid Thingy, Grid Thingy )
partitionFallers board =
    Dict.partition (\coords _ -> shouldFall coords board) board.things


shouldFall : Coords -> Board -> Bool
shouldFall ( x, y ) board =
    let
        faller =
            Grid.get ( x, y ) board.things
                |> Maybe.map Thingy.isFaller
                |> Maybe.withDefault False

        vacancyBelow =
            isEmptyTile ( x, y - 1 ) board

        mixableBelow =
            Maybe.map2 Thingy.mixIngredients
                (Grid.get ( x, y ) board.things)
                (Grid.get ( x, y - 1 ) board.things)
                |> Maybe.map (always True)
                |> Maybe.withDefault False
    in
    faller && (vacancyBelow || mixableBelow)


isEmptyTile : Coords -> Board -> Bool
isEmptyTile coords { things, floor } =
    let
        thing =
            Grid.get coords things

        floorTile =
            Grid.get coords floor
    in
    case ( thing, floorTile ) of
        ( Just _, _ ) ->
            False

        ( Nothing, Just f ) ->
            FloorTile.isObstacleTile f

        ( Nothing, Nothing ) ->
            True


{-| If possible, move thing from one set of coords to another.
Will mix ingredients if possible.
If move can not be preformed, then the original unchanged grid is returned.
-}
moveMixing : Coords -> Coords -> Grid Thingy -> Grid Thingy
moveMixing from to things =
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
collectThings : Board -> ( Grid Thingy, Int )
collectThings { floor, things } =
    let
        collectionPoints =
            floor |> Dict.filter (\_ tile -> FloorTile.isBunCollector tile) |> Dict.keys |> Set.fromList

        -- Collect thing if it is a _bun_ on a collection point
        ( collectedThings, remainingThings ) =
            things
                |> Dict.partition
                    (\coords thing ->
                        Set.member coords collectionPoints
                            && Thingy.isCollectableBun thing
                    )
    in
    ( remainingThings, 100 * Dict.size collectedThings )



-- # SUBSCRIPTIONS


{-| Keep updating regularly while game is running.

    - Spawn new things now and then
    - Let things fall (if anything could fall)

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if not <| isGameOver { floor = level.floor, things = model.things } then
        Sub.batch
            ([ Time.every (spawnInterval model) (\_ -> Spawn) ]
                -- Only Have things fall there is something thst could fall (save messages)
                ++ (if not <| isStable (FloorTile.obstacleTileArea level.floor) model.things then
                        [ Time.every fallInterval (\_ -> Fall) ]

                    else
                        []
                   )
            )

    else
        Sub.none


{-| Spawn intervall (relative to portion of board filled)Increases the spawn when the board fills up.

    Increasing the spawn intervall means we give the player more
    time to think when the board is getting crammed.

    Decreasing the spawn intervall when the board is nearly
    empty means it will fill upp again faster.

    Note:
    It's currenly unclear what effect constantly shifting
    time intervalls has on the subscription.
    Theoretically it could constantly be resetting on every change,
    causing the associated message to be sent less often.
    Rounding intervall to the nearest second should eliviate moste
    of the problem if it exists.

-}
spawnInterval : { a | things : Grid Thingy } -> Float
spawnInterval { things } =
    let
        baseIntervall =
            0.25

        extraIntervall =
            9.0

        coveredArea =
            Dict.size things |> toFloat

        totalArea =
            (level.size.width * level.size.height) |> toFloat

        coveredPortion =
            coveredArea / totalArea

        intervall =
            baseIntervall + extraIntervall * coveredPortion * coveredPortion
    in
    (round intervall |> toFloat) * second


fallInterval : Float
fallInterval =
    0.25 * second


second =
    1000.0



-- # VIEW


{-| View the entire game, including UI and debug.
-}
view : Model -> Html Msg
view model =
    Html.div
        [ id "game-container"
        , style "position" "relative"
        , style "width" ((6 * tileSide |> fromInt) ++ "px")
        , style "height" ((8 * tileSide |> fromInt) ++ "px")
        , style "border" "5px solid black"
        ]
        [ viewScore model.totalScore
        , viewBoardContainer model
        , viewInfo
            (model.selectedTile
                |> Maybe.map (\coords -> Dict.get coords model.things)
                |> Maybe.withDefault Nothing
            )
        , viewDebug model
        ]


viewScore : Int -> Html msg
viewScore score =
    Html.div
        [ id "score"
        , style "position" "absolute"
        , (\( a, b ) -> style a b) (( "top", 0 ) |> px)
        , (\( a, b ) -> style a b) (( "font-size", tileSide // 2 - 10 ) |> px)
        ]
        [ Html.span
            [ class "label"
            , style "display" "inline-block"
            , (\( a, b ) -> style a b) (( "width", 2 * tileSide ) |> px)
            , style "padding" "10px 0"
            , style "text-align" "left"
            ]
            [ text "Score"
            ]
        , Html.span
            [ class "value"
            , style "display" "inline-block"
            , (\( a, b ) -> style a b) (( "width", 4 * tileSide ) |> px)
            , style "padding" "10px 0"
            , style "text-align" "right"
            ]
            [ text <| fromInt score
            ]
        ]


viewBoardContainer : Model -> Html Msg
viewBoardContainer model =
    Html.div
        [ id "board-container"
        , style "position" "absolute"
        , style "top" ((tileSide // 2 |> fromInt) ++ "px")
        , style "left" "0"
        ]
        [ viewBoard model
        , if isGameOver { floor = level.floor, things = model.things } then
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
            Grid.drawBox PlainTile level.size
                |> Grid.translate ( 0, 1 - level.size.height )
                |> Dict.intersect level.floor
                -- Convert to full tiles
                |> Dict.map (\_ floorTile -> RenderableTile Nothing Nothing floorTile)
                -- Render highlighed tiles
                |> Dict.map (\tileCoords tile -> { tile | highlight = getPossibleHighlight model tileCoords })
                -- Render buns
                |> Dict.map
                    (\coords tile ->
                        { tile
                            | content = Dict.get coords model.things
                        }
                    )
    in
    Grid.toHtmlDiv ( tileSide, tileSide ) renderTile finalGrid


{-| Determine if the tile at the given coords should be highlighted in some way.
-}
getPossibleHighlight : Model -> Coords -> Maybe Highlight
getPossibleHighlight { things, selectedTile } tileCoords =
    if shouldFall tileCoords { things = things, floor = level.floor } then
        Just WillFall

    else
        case selectedTile of
            Just selectedCoords ->
                if tileCoords == selectedCoords then
                    Just SelectedMover

                else if isValidMove selectedCoords tileCoords then
                    if isPossibleMove { floor = level.floor, things = things } selectedCoords tileCoords then
                        Just PossibleMovementDestination

                    else
                        Just ForbiddenMovementDestination

                else
                    Nothing

            Nothing ->
                Nothing


viewInfo : Maybe Thingy -> Html msg
viewInfo tile =
    Html.div
        [ id "tile-info"
        , class "info-box"
        , style "position" "absolute"
        , (\( a, b ) -> style a b) (( "width", level.size.width * tileSide ) |> px)
        , (\( a, b ) -> style a b) (( "height", 3 * (tileSide // 2) ) |> px)
        , (\( a, b ) -> style a b) (( "bottom", 0 ) |> px)
        , (\( a, b ) -> style a b) (( "left", 0 ) |> px)
        , style "background-color" "white"
        ]
        (tile
            |> Maybe.map (\t -> [ Thingy.viewInfo t ])
            |> Maybe.withDefault []
        )


{-| View the 'Game over' modal dialog box
-}
viewGameOver : Html Msg
viewGameOver =
    Html.div
        [ id "game-over"
        , style "position" "absolute"
        , style "top" "0"
        , style "width" "90%"
        , style "height" "90%"
        , style "padding" "5%"
        , style "color" "darkred"
        , style "text-align" "center"
        , style "background-color" "white"
        , style "opacity" "0.75"
        ]
        [ Html.h1 [ style "opacity" "1" ] [ text "Game over" ]
        , Html.a
            [ style "opacity" "1"
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
        , style "position" "fixed"
        , style "bottom" "0"
        , style "right" "0"
        , style "width" "100%"
        , style "height" ((4 * tileSide |> fromInt) ++ "px")
        , style "background-color" "lightgray"
        ]
        [ Html.button [ onClick Spawn ] [ text "Spawn!" ]
        , Html.button [ onClick Fall ] [ text "Fall!" ]
        , Html.button [ onClick Collect ] [ text "Collect" ]
        , Html.p [] [ "Move count: " ++ (model.moveCount |> fromInt) |> text ]
        , Html.p []
            [ "Random seed: "
                ++ (model.seed
                        |> Random.step (Random.int Random.minInt Random.maxInt)
                        |> Tuple.first
                        >> fromInt
                   )
                |> text
            ]
        , Html.p []
            [ "Selected tile: "
                ++ (model.selectedTile
                        |> Maybe.map strFromCoords
                        |> Maybe.withDefault ""
                   )
                |> text
            ]
        , Html.p [] [ "Spawn interval: " ++ (spawnInterval model |> fromFloat) ++ "ms" |> text ]
        , Html.p []
            [ "Game over? "
                ++ (isGameOver { floor = level.floor, things = model.things }
                        |> strFromBool
                   )
                |> text
            ]
        ]


{-| Render all the layers of a single Grid cell
-}
renderTile : Coords -> RenderableTile -> Html Msg
renderTile coords tile =
    Html.div
        [ style "width" (fromInt tileSide ++ "px")
        , style "height" (fromInt tileSide ++ "px")
        , style "background-color" (getTileColor tile)
        , style "border" "1px solid darkgray"
        , onClick (SelectTile coords)
        , onDoubleClick (ActivateTile coords)
        ]
        [ tile.content |> Maybe.map Thingy.toHtml |> Maybe.withDefault (text "")
        , Html.span
            [ class "debug", style "font-size" "25%" ]
            [ Html.text (strFromCoords coords) ]
        ]


{-| Determine tile collor, first by Highlight then by FloorTile
-}
getTileColor : RenderableTile -> String
getTileColor { highlight, floor } =
    case ( highlight, floor ) of
        ( Just SelectedMover, _ ) ->
            "darkblue"

        ( Just PossibleMovementDestination, _ ) ->
            "palegreen"

        ( Just ForbiddenMovementDestination, _ ) ->
            "indianred"

        ( Just WillFall, _ ) ->
            "orange"

        ( Nothing, Spawner _ ) ->
            "antiquewhite"

        ( Nothing, PlainTile ) ->
            "lemonchiffon"

        ( Nothing, BunCollector ) ->
            "yellowgreen"

        ( Nothing, WallTile ) ->
            "darkgray"


strFromBool : Bool -> String
strFromBool b =
    if b then
        "Yes"

    else
        "No"


strFromCoords : Coords -> String
strFromCoords ( x, y ) =
    "(" ++ fromInt x ++ ", " ++ fromInt y


px : ( String, Int ) -> ( String, String )
px ( name, value ) =
    ( name, (value |> fromInt) ++ "px" )


tileSide : Int
tileSide =
    Thingy.spriteSide
