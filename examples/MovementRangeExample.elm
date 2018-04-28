module MovementRangeExample exposing (main)

import Set exposing (Set)
import Dict exposing (Dict)
import Grid exposing (Grid)
import Grid.MovementRange as Range exposing (..)
import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, href)


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }



-- MODEL


type alias Model =
    { board : Grid Bool
    , position : Coords
    , steps : Int
    }


initialModel : Model
initialModel =
    Model tileMap ( 0, 0 ) 5


tileMap : Grid Bool
tileMap =
    Grid.empty
        |> Grid.put ( 5, 5 ) True
        |> Grid.put ( -5, 5 ) True
        |> Grid.put ( 5, -5 ) True
        |> Grid.put ( -5, -5 ) True
        |> Grid.put ( 1, 2 ) True
        |> Grid.put ( 1, 1 ) True
        |> Grid.put ( 1, 0 ) True
        |> Grid.put ( 1, -1 ) True
        |> Grid.put ( 1, -2 ) True



-- UPDATE


type Msg
    = ToggleObstacle Coords
    | Move Coords
    | ModifySteps Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleObstacle coords ->
            let
                board_ =
                    Grid.put coords (not (Maybe.withDefault False (Grid.get coords model.board))) model.board
            in
                { model | board = board_ }

        Move coords ->
            { model | position = coords }

        ModifySteps change ->
            { model | steps = (max 1 (model.steps + change)) }



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        board =
            chartMovementRange model.position model.steps model.board
    in
        Html.div []
            [ viewControlPanel model
            , (Grid.toHtmlTable (renderTile model.position) board)
            ]


viewControlPanel : { b | steps : a } -> Html.Html Msg
viewControlPanel { steps } =
    Html.div []
        [ Html.span [] [ Html.text "Steps" ]
        , Html.button [ onClick (ModifySteps (0 + 1)) ] [ Html.text "▲" ]
        , Html.button [ onClick (ModifySteps (0 - 1)) ] [ Html.text "▼" ]
        , Html.span [] [ Html.text (toString steps) ]
        ]


chartMovementRange : Coords -> Int -> Grid Bool -> Grid Bool
chartMovementRange start steps tileMap =
    let
        range =
            Range.chart
                (tileMap |> Dict.keys |> Set.fromList)
                start
                steps
    in
        Set.foldr (\r m -> Grid.put r False m) tileMap range


renderTile : Coords -> Coords -> Bool -> Html.Html Msg
renderTile standingPosition c t =
    Html.div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color"
              , (if t then
                    "lightgray"
                 else
                    "lightgreen"
                )
              )
            ]
        ]
        [ if (c == standingPosition) then
            Html.span [ style [ ( "font-size", "150%" ) ] ] [ Html.text "🐍" ]
          else if (not t) then
            Html.a [ onClick (Move c), href "#go-here" ] [ Html.text "Go!" ]
          else
            Html.text ""
        ]
