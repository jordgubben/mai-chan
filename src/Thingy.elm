module Thingy exposing (Thingy(..), Flavour(..), isFaller, isCollectableBun, mixIngredients, viewInfo, toHtml, spriteSide)

import Html exposing (Html, text)
import Html.Attributes as Att exposing (id, class, style)
import NeoSprite exposing (Sprite, Sheet)


{-| Helper preview program to used to check that all sprites are rendered correctly.
-}
main : Html m
main =
    let
        neutralThingies =
            [ ( "Neutral flavoured Bun", (Bun Nothing) )
            , ( "Neutral flavoured Flour", (Flour Nothing) )
            , ( "Neutral flavoured Water", (Water Nothing) )
            ]

        -- Everything Chilli
        allChilli =
            [ ( "Chilli", (Flavouring { flavour = Chilli, packaged = False }) )
            , ( "Packaged Chilli", (Flavouring { flavour = Chilli, packaged = True }) )
            , ( "Chilli flavoured Bun", (Bun (Just Chilli)) )
            , ( "Chilli flavoured Flour", (Flour (Just Chilli)) )
            , ( "Chilli flavoured Water", (Water (Just Chilli)) )
            ]

        -- Everything Sweet
        allSweets =
            [ ( "Sugar", (Flavouring { flavour = Sugar, packaged = False }) )
            , ( "Packaged Sugar", (Flavouring { flavour = Sugar, packaged = True }) )
            , ( "Sweet Bun (OMG! Game title!)", (Bun (Just Sugar)) )
            , ( "Sweet Flour", (Flour (Just Sugar)) )
            , ( "Sweet Water", (Water (Just Sugar)) )
            ]

        -- Everything Chocolate
        allChocolate =
            [ ( "Chocolate", (Flavouring { flavour = Chocolate, packaged = False }) )
            , ( "Packaged Chocolate", (Flavouring { flavour = Chocolate, packaged = True }) )
            , ( "Choco Bun ", (Bun (Just Chocolate)) )
            , ( "Choco Flour ", (Flour (Just Chocolate)) )
            , ( "Choco Water ", (Water (Just Chocolate)) )
            ]

        -- Misc. others
        allOthers =
            [ ( "Obstacle", Obstacle )
            ]

        displayThings title thingies =
            Html.div []
                ((Html.h2 [] [ text title ])
                    :: (thingies
                            |> List.map
                                (\( desc, thing ) ->
                                    Html.div
                                        [ style
                                            [ ( "display", "inline-block" )
                                            , ( "border", "1px solid black" )
                                            , ( "width", "100px" )
                                            , ( "margin", "5px" )
                                            , ( "padding", "10px" )
                                            ]
                                        ]
                                        [ thing |> toHtml, text desc ]
                                )
                       )
                )
    in
        Html.div []
            [ Html.h1 [] [ text "Thingy preview" ]
            , Html.p [] [ text "Bellow are all the possible renderable states of a 'Thingy'." ]
            , (displayThings "Flavour neutral things" neutralThingies)
            , (displayThings "All the Chilli" allChilli)
            , (displayThings "All the Sweets" allSweets)
            , (displayThings "All the chocolate" allChocolate)
            , (displayThings "All other things" allOthers)
            ]



-- # Types


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
            mixFlavouringWithWater packaged flavour

        ( Flavouring { flavour, packaged }, Water Nothing ) ->
            mixFlavouringWithWater packaged flavour

        ( Flour Nothing, Flavouring { flavour, packaged } ) ->
            mixFlavouringWithFlour packaged flavour

        ( Flavouring { flavour, packaged }, Flour Nothing ) ->
            mixFlavouringWithFlour packaged flavour

        _ ->
            Nothing


mixFlavouringWithWater : Bool -> Flavour -> Maybe Thingy
mixFlavouringWithWater packaged flavour =
    if not packaged then
        Just <| Water (Just flavour)
    else
        Nothing


mixFlavouringWithFlour : Bool -> Flavour -> Maybe Thingy
mixFlavouringWithFlour packaged flavour =
    if not packaged then
        Just <| Flour (Just flavour)
    else
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
        ( cssClass, spriteKit ) =
            case flavour of
                Sugar ->
                    ( "sugar", sugarSprites )

                Chocolate ->
                    ( "chocolate", chocolateSprites )

                Chilli ->
                    ( "chilli", chilliSprites )

        sprite =
            if packaged then
                spriteKit.packaged
            else
                spriteKit.unpackaged
    in
        Html.div [ style [ ( "position", "relative" ) ] ]
            [ renderSprite cssClass sprite ]


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
renderSprite : String -> Sprite -> Html msg
renderSprite cssClass sprite =
    Html.span
        [ class <| cssClass ++ "-sprite" ]
        [ NeoSprite.toHtml sprite ]


px : ( String, Int ) -> ( String, String )
px ( name, value ) =
    ( name, (value |> toString) ++ "px" )



-- # Sprites


{-| Collection of Sprites for Flavouring (i.e. Chilli, Cocolate, Sugar)
-}
type alias FlavourSpriteKit =
    { unpackaged : Sprite
    , packaged : Sprite
    }


chilliSprites : FlavourSpriteKit
chilliSprites =
    { unpackaged = staticSprite ( 1, 0 )
    , packaged = staticSprite ( 1, 1 )
    }


sugarSprites : FlavourSpriteKit
sugarSprites =
    { unpackaged = staticSprite ( 2, 0 )
    , packaged = staticSprite ( 2, 1 )
    }


chocolateSprites : FlavourSpriteKit
chocolateSprites =
    { unpackaged = staticSprite ( 3, 0 )
    , packaged = staticSprite ( 3, 1 )
    }


{-| Collection of Sprites for a Thingy with flavour (currently Flour, Water or Bun)
-}
type alias FlavouringSpriteKit =
    { sweet : Sprite
    , chilli : Sprite
    , coco : Sprite
    , basic : Sprite
    }


getSpriteByFlavour : FlavouringSpriteKit -> Maybe Flavour -> Sprite
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


bunSprites : FlavouringSpriteKit
bunSprites =
    { chilli = staticSprite ( 1, 2 )
    , sweet = staticSprite ( 2, 2 )
    , coco = staticSprite ( 3, 2 )
    , basic = staticSprite ( 0, 2 )
    }


waterSprites : FlavouringSpriteKit
waterSprites =
    { chilli = staticSprite ( 1, 3 )
    , sweet = staticSprite ( 2, 3 )
    , coco = staticSprite ( 3, 3 )
    , basic = staticSprite ( 0, 3 )
    }


flourSprites : FlavouringSpriteKit
flourSprites =
    { chilli = staticSprite ( 1, 4 )
    , sweet = staticSprite ( 2, 4 )
    , coco = staticSprite ( 3, 4 )
    , basic = staticSprite ( 0, 4 )
    }


staticSprite : ( Int, Int ) -> Sprite
staticSprite index =
    NeoSprite.fromSheet spriteSheet index


spriteSheet : Sheet
spriteSheet =
    { imageUrl = "Thingy--sprites.png"
    , imageWidth = 256
    , imageHeight = 256
    , spriteWidth = spriteSide
    , spriteHeight = spriteSide
    }


spriteSide : Int
spriteSide =
    64
