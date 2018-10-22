module Thingy exposing (Thingy(..), Flavour(..), isFaller, isCollectableBun, mixIngredients, viewInfo, toHtml, spriteSide)

import Array
import Html exposing (Html, text)
import Html.Attributes as Att exposing (id, class, style)
import Sprite exposing (Sprite)


type Thingy
    = Bun (Maybe Flavour)
    | Flour (Maybe Flavour)
    | Water (Maybe Flavour)
    | Flavouring { flavour : Flavour, packaged : Bool }
    | Obstacle


type Flavour
    = Sugar
    | Chocolate
    | Chilli



-- # Categorisation


{-| Is the Thingy that falls is nothing is below to stop it?
-}
isFaller : Thingy -> Bool
isFaller thing =
    case thing of
        Obstacle ->
            False

        _ ->
            True


{-|

    Is this Thingy a Bun ready for collection?
-}
isCollectableBun : Thingy -> Bool
isCollectableBun thing =
    case thing of
        Bun _ ->
            True

        _ ->
            False



-- # Mixing


{-| Combine ingredients (things) into new things.
Returns Noting if the provided commbination is not supported.
-}
mixIngredients : Thingy -> Thingy -> Maybe Thingy
mixIngredients a b =
    case ( a, b ) of
        ( Water waterFlavour, Flour flourFlavour ) ->
            mixBun { water = waterFlavour, flour = flourFlavour }

        ( Flour flourFlavour, Water waterFlavour ) ->
            mixBun { water = waterFlavour, flour = flourFlavour }

        ( Water Nothing, Flavouring { flavour, packaged } ) ->
            if not packaged then
                Just <| Water (Just flavour)
            else
                Nothing

        ( Flavouring { flavour, packaged }, Water Nothing ) ->
            if not packaged then
                Just <| Water (Just flavour)
            else
                Nothing

        ( Flour Nothing, Flavouring { flavour, packaged } ) ->
            if not packaged then
                Just <| Flour (Just flavour)
            else
                Nothing

        ( Flavouring { flavour, packaged }, Flour Nothing ) ->
            if not packaged then
                Just <| Flour (Just flavour)
            else
                Nothing

        _ ->
            Nothing


mixBun : { flour : Maybe Flavour, water : Maybe Flavour } -> Maybe Thingy
mixBun { flour, water } =
    case ( flour, water ) of
        ( Just flavour, Nothing ) ->
            Just <| Bun <| Just flavour

        ( Nothing, Just flavour ) ->
            Just <| Bun <| Just flavour

        ( Just flourFlavour, Just waterFlavour ) ->
            if (flourFlavour == waterFlavour) then
                Just <| Bun <| Just flourFlavour
            else
                Nothing

        ( Nothing, Nothing ) ->
            Just <| Bun Nothing



-- # Textual representation


viewInfo : Thingy -> Html msg
viewInfo thingy =
    let
        containerDiv attributes elements =
            Html.div
                ([ style
                    [ ( "position", "relative" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    ]
                 ]
                    ++ attributes
                )
                elements

        iconDiv icon =
            Html.div
                [ class "icon"
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    , ( "width", spriteSide ) |> px
                    , ( "height", spriteSide ) |> px
                    , ( "padding", spriteSide // 4 ) |> px
                    , ( "background-color", "darkgray" )
                    ]
                ]
                [ icon ]

        descriptionDiv ( name, summary ) =
            Html.div
                [ class "description"
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "right", "0" )
                    , ( "width", spriteSide * 4 ) |> px
                    , ( "height", spriteSide * 1 ) |> px
                    , ( "padding", spriteSide // 4 ) |> px
                    ]
                ]
                [ Html.div [ style [ ( "padding", 5 ) |> px ] ]
                    [ Html.strong [ class "name" ] [ text name ]
                    , text " - "
                    , Html.span [ class "text" ] [ text summary ]
                    ]
                ]
    in
        containerDiv []
            [ iconDiv <| toHtml thingy
            , descriptionDiv <| describe thingy
            ]


describe : Thingy -> ( String, String )
describe thingy =
    case thingy of
        Flour _ ->
            ( "Flour", "Mix with water to make a Bun." )

        Water _ ->
            ( "Water", "Mix with Flour to make a Bun." )

        Flavouring _ ->
            ( "Flavour", "Mix with Flavour or Water." )

        Bun _ ->
            ( "Bun", "Send it off!" )

        Obstacle ->
            ( "Obstacle", "Do ignore it." )



-- # Visual representation


{-| Render some Thingy as HTML
-}
toHtml : Thingy -> Html msg
toHtml thingy =
    case thingy of
        Bun flavour ->
            renderBun flavour

        Flour flavour ->
            renderFlour flavour

        Water flavour ->
            renderWater flavour

        Flavouring { flavour, packaged } ->
            renderFlavouring flavour packaged

        Obstacle ->
            obstacleHtml


renderFlavouring : Flavour -> Bool -> Html msg
renderFlavouring flavour packaged =
    let
        ( cssClass, sprite ) =
            case flavour of
                Sugar ->
                    ( "sugar", sugarSprite )

                Chocolate ->
                    ( "chocolate", chocolateSprite )

                Chilli ->
                    ( "chilli", chilliSprite )
    in
        Html.div [ style [ ( "position", "relative" ) ] ]
            [ renderSprite cssClass sprite
            , Html.div
                [ style
                    (if packaged then
                        [ ( "position", "absolute" )
                        , ( "width", 50 ) |> px
                        , ( "height", 50 ) |> px
                        , ( "top", 0 ) |> px
                        , ( "left", 0 ) |> px
                        , ( "margin", "4px 4px" )
                        , ( "background-color", "rgb(0,128, 255, 0.25)" )
                        , ( "border", "2px solid rgb(0,128, 255, 0.7)" )
                        ]
                     else
                        []
                    )
                ]
                []
            ]


obstacleHtml : Html msg
obstacleHtml =
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


renderBun : Maybe Flavour -> Html msg
renderBun flavour =
    let
        sprite =
            getSpriteByFlavour bunSprites flavour
    in
        renderSprite "bun" sprite


renderWater : Maybe Flavour -> Html msg
renderWater flavour =
    let
        sprite =
            getSpriteByFlavour waterSprites flavour
    in
        renderSprite "water" sprite


renderFlour : Maybe Flavour -> Html msg
renderFlour flavour =
    let
        sprite =
            getSpriteByFlavour flourSprites flavour
    in
        renderSprite "flour" sprite


{-| Pick color pattern based on Flavour.
-}
pickFlavourColors : Maybe Flavour -> ( String, String )
pickFlavourColors flavour =
    case flavour of
        Just Sugar ->
            ( "darkblue", "lightblue" )

        Just Chilli ->
            ( "darkred", "orange" )

        Just Chocolate ->
            ( "black", "brown" )

        Nothing ->
            ( "gray", "lightgray" )


type alias BorderDecoratedSpriteStyle a =
    { a
        | borderRadius : Int
        , debugName : String
        , primaryColor : String
        , secondaryColor : String
    }


{-| Render the most basic sprite.
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



-- # Sprites


chilliSprite : Sprite {}
chilliSprite =
    staticSprite ( 1, 0 )


sugarSprite : Sprite {}
sugarSprite =
    staticSprite ( 2, 0 )


chocolateSprite : Sprite {}
chocolateSprite =
    staticSprite ( 3, 0 )


type alias FlavouredSpriteKit =
    { sweet : Sprite {}
    , chilli : Sprite {}
    , coco : Sprite {}
    , basic : Sprite {}
    }


getSpriteByFlavour : FlavouredSpriteKit -> Maybe Flavour -> Sprite {}
getSpriteByFlavour kit flavour =
    case flavour of
        Just Sugar ->
            kit.sweet

        Just Chilli ->
            kit.chilli

        Just Chocolate ->
            kit.coco

        Nothing ->
            kit.basic


bunSprites : FlavouredSpriteKit
bunSprites =
    { chilli = staticSprite ( 1, 1 )
    , sweet = staticSprite ( 2, 1 )
    , coco = staticSprite ( 3, 1 )
    , basic = staticSprite ( 0, 1 )
    }


waterSprites : FlavouredSpriteKit
waterSprites =
    { chilli = staticSprite ( 1, 2 )
    , sweet = staticSprite ( 2, 2 )
    , coco = staticSprite ( 3, 2 )
    , basic = staticSprite ( 0, 2 )
    }


flourSprites : FlavouredSpriteKit
flourSprites =
    { chilli = staticSprite ( 1, 3 )
    , sweet = staticSprite ( 2, 3 )
    , coco = staticSprite ( 3, 3 )
    , basic = staticSprite ( 0, 3 )
    }


staticSprite : ( Int, Int ) -> Sprite {}
staticSprite pos =
    { baseSprite | dope = Array.fromList ([ pos ]) }


baseSprite : Sprite {}
baseSprite =
    { sheet = "Thingy--sprites.png"
    , rows = 4
    , columns = 4
    , size = ( 256, 256 )
    , frame = 0
    , dope = Array.fromList ([ ( 0, 0 ) ])
    }


spriteSide : Int
spriteSide =
    64
