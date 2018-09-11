module Thingy exposing (Thingy(..), Flavour(..), isFaller, isCollectableBun, mixIngredients, describe, toHtml, spriteSide)

import Array
import Html exposing (Html, text)
import Html.Attributes as Att exposing (id, class, style)
import Sprite exposing (Sprite)


type Thingy
    = Bun (Maybe Flavour)
    | Flour (Maybe Flavour)
    | Water (Maybe Flavour)
    | Flavouring Flavour
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

        ( Water Nothing, Flavouring flavour ) ->
            Just <| Water (Just flavour)

        ( Flavouring flavour, Water Nothing ) ->
            Just <| Water (Just flavour)

        ( Flour Nothing, Flavouring flavour ) ->
            Just <| Flour (Just flavour)

        ( Flavouring flavour, Flour Nothing ) ->
            Just <| Flour (Just flavour)

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


describe : Thingy -> String
describe thingy =
    case thingy of
        Flour _ ->
            "Flour - Mix with water to make a Bun."

        Water _ ->
            "Water - Mix with Flour to make a Bun."

        Flavouring _ ->
            "Flavour - Mix with Flavour or Water."

        Bun _ ->
            "Bun - Send it off!"

        Obstacle ->
            "Obstacle - Do ignore it."



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

        Flavouring flavour ->
            renderFlavouring flavour

        Obstacle ->
            obstacleHtml


renderFlavouring : Flavour -> Html msg
renderFlavouring flavour =
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
        renderSprite cssClass sprite


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
