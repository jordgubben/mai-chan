module NeoSprite exposing (Sheet, Sprite, fromSheet, toHtml, toSvg)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SvgAt


{-| Usage example
-}
main : Svg msg
main =
    Html.div []
        [ Html.h1 [] [ text "Sprite(s)" ]
        , Html.h2 [] [ text "SVG" ]
        , Svg.svg
            [ SvgAt.width (300 |> px)
            , SvgAt.height (300 |> px)
            , SvgAt.viewBox "0 0 300 300"
            ]
            [ toSvg breadSprite ( 100, 50 )
            , toSvg cocoBunSprite ( 150, 60 )
            , toSvg breadSprite ( 200, 70 )
            ]
        , Html.h2 [] [ text "HTML" ]
        , Html.div []
            [ toHtml breadSprite
            , toHtml cocoBunSprite
            , toHtml breadSprite
            ]
        ]


breadSprite : Sprite
breadSprite =
    fromSheet exampleSheet ( 0, 1 )


cocoBunSprite : Sprite
cocoBunSprite =
    fromSheet exampleSheet ( 3, 1 )


{-| A sample sheet of 4x4 sprites Borroeedfrom "Mai-chan's sweet buns".
-}
exampleSheet : Sheet
exampleSheet =
    { imageUrl = "http://superattack.se/games/mai-chan/Thingy--sprites.png"
    , imageWidth = 256
    , imageHeight = 256
    , spriteWidth = 64
    , spriteHeight = 64
    }



-- # TYPES


{-| A single Sprite on a sprite sheet image.
-}
type alias Sprite =
    { imageUrl : String
    , imageWidth : Int
    , imageHeight : Int
    , offsetX : Int
    , offsetY : Int
    , spriteWidth : Int
    , spriteHeight : Int
    }


{-| A sheet of (same size) Sprites organized in a grid.
-}
type alias Sheet =
    { imageUrl : String
    , imageWidth : Int
    , imageHeight : Int
    , spriteWidth : Int
    , spriteHeight : Int
    }


fromSheet : Sheet -> ( Int, Int ) -> Sprite
fromSheet sheet ( x, y ) =
    { imageUrl = sheet.imageUrl
    , imageWidth = sheet.imageWidth
    , imageHeight = sheet.imageHeight
    , spriteWidth = sheet.spriteWidth
    , spriteHeight = sheet.spriteHeight
    , offsetX = x * sheet.spriteWidth
    , offsetY = y * sheet.spriteHeight
    }



-- # RENDERING


{-| Convert Sprite to SVG.
The second argument is the position.
Wrap with a <g>-element for more elaborate transformations.
-}
toSvg : Sprite -> ( Int, Int ) -> Svg msg
toSvg sprite ( x, y ) =
    let
        left =
            sprite.offsetX

        right =
            sprite.imageWidth - (sprite.offsetX + sprite.spriteWidth)

        top =
            sprite.offsetY

        bottom =
            sprite.imageHeight - (sprite.offsetY + sprite.spriteHeight)

        imageInset =
            "inset("
                ++ ([ top, right, bottom, left ]
                        |> List.map px
                        |> String.join " "
                   )
                ++ ")"
    in
    Svg.image
        [ SvgAt.x (x - sprite.offsetX |> fromInt)
        , SvgAt.y (y - sprite.offsetY |> fromInt)
        , SvgAt.width (sprite.imageWidth |> fromInt)
        , SvgAt.height (sprite.imageHeight |> fromInt)
        , SvgAt.xlinkHref sprite.imageUrl
        , SvgAt.clipPath imageInset
        ]
        []


{-| Convert sprite to HTML.
-}
toHtml : Sprite -> Html msg
toHtml sprite =
    Html.span
        [ style "display" "inline-block"
        , style "background-image" ("url('" ++ sprite.imageUrl ++ "')")
        , style "width" (sprite.spriteWidth |> px)
        , style "height" (sprite.spriteHeight |> px)
        , style "background-position"
            ([ 0 - sprite.offsetX, 0 - sprite.offsetY ]
                |> List.map px
                |> String.join " "
            )
        ]
        []


px : Int -> String
px =
    fromInt >> (\s -> s ++ "px")


{-| Convert Int to String.
(Simplify migration to Elm 0.19)
-}
fromInt : Int -> String
fromInt i =
    i |> toString
