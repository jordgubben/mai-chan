module SweetBuns exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Html.Events as Ev exposing (onMouseEnter, onMouseLeave)
import Grid exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, update = update, view = view }


type alias Bun =
    String


type alias FullTile =
    String


type alias Model =
    { selectedTile : Coords
    }


type Msg
    = EnterTile Coords
    | LeaveTile Coords


initialModel : Model
initialModel =
    { selectedTile = ( 0, 0 )
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnterTile coords ->
            { model | selectedTile = coords }

        _ ->
            model


view : Model -> Html Msg
view model =
    let
        ( selX, selY ) =
            model.selectedTile

        highlightColumn : Grid FullTile
        highlightColumn =
            List.range 0 12 |> List.map (\ranY -> ( ( selX, -ranY ), "O" )) |> Grid.fromList

        finalGrid =
            back |> Dict.union highlightColumn |> Dict.union buns
    in
        Html.div []
            [ Grid.toHtmlDiv ( tileSide, tileSide ) renderTile finalGrid
            , Html.p []
                [ (model.selectedTile |> toString |> Html.text)
                ]
            ]


renderTile : Coords -> FullTile -> Html Msg
renderTile coords tile =
    Html.div
        [ style
            [ ( "width", toString (tileSide) ++ "px" )
            , ( "height", toString (tileSide) ++ "px" )
            , ( "background-color", "lightgray" )
            , ( "border", "1px solid darkgray" )
            ]
        , onMouseEnter (EnterTile coords)
        , onMouseLeave (LeaveTile coords)
        ]
        [ Html.text tile
        ]


tileSide : Int
tileSide =
    32


buns : Grid Bun
buns =
    Grid.fromList
        [ ( ( 0, 0 ), "🍪" )
        , ( ( 1, 0 ), "🍪" )
        , ( ( 2, 0 ), "🍪" )
        , ( ( 3, 0 ), "🍪" )
        , ( ( 4, 0 ), "🍪" )
        , ( ( 5, 0 ), "🍪" )
        ]


back : Grid FullTile
back =
    Grid.drawBox " " { width = 6, height = 12 }
        |> Grid.translate ( 0, -12 )


all : Grid FullTile
all =
    Dict.union buns back
