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


type alias Bun =
    String


type alias FullTile =
    { content : Maybe Bun
    , back : String
    }


type alias Model =
    { selectedTile : Coords
    , buns : Grid Bun
    }


type Msg
    = SelectColumn Coords
    | StepBuns


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    , buns = initialBuns
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectColumn coords ->
            { model | selectedTile = coords }

        StepBuns ->
            { model | buns = step terrain model.buns }


{-| Move all buns downwards (if possible).
-}
step : Set Coords -> Grid Bun -> Grid Bun
step terrain buns =
    Dict.foldl
        (\( x, y ) b g ->
            let
                canMove nc =
                    not (Set.member nc terrain || Dict.member nc g)

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
                        if x == selX then
                            { tile | back = "pink" }
                        else
                            tile
                    )
                -- Render terrain
                |> Dict.map
                    (\( x, y ) tile ->
                        if Set.member ( x, y ) terrain then
                            { tile | back = "black" }
                        else
                            tile
                    )
                -- Render buns
                |> Dict.map
                    (\coords tile ->
                        { tile
                            | content = (Dict.get coords model.buns)
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
        ]
        [ Html.text (Maybe.withDefault "" tile.content)
        , Html.span [ class "debug" ] [ Html.text (toString coords) ]
        ]


tileSide : Int
tileSide =
    32


initialBuns : Grid Bun
initialBuns =
    Grid.fromList
        [ ( ( 0, 0 ), "🍪" )
        , ( ( 1, 0 ), "🍪" )
        , ( ( 2, 0 ), "🍪" )
        , ( ( 3, 0 ), "🍪" )
        , ( ( 4, 0 ), "🍪" )
        , ( ( 4, -1 ), "🍩" )
        , ( ( 5, 0 ), "🍪" )
        ]


terrain : Set Coords
terrain =
    Set.union
        (Grid.lineRect () { width = 8, height = 10 }
            |> Grid.translate ( -1, -8 )
            |> Dict.keys
            |> Set.fromList
        )
        (Set.fromList [ ( 1, -3 ), ( 4, -3 ), ( 3, -8 ), ( 4, -7 ), ( 5, -6 ) ])


back : Grid FullTile
back =
    Grid.drawBox { content = Nothing, back = "lightgray" } { width = 10, height = 12 }
        |> Grid.translate ( -2, -10 )
