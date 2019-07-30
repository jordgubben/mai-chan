module ThingyTest exposing (bun, chilli, flour, mixingSuite, sugar, testMix, water)

import Expect exposing (Expectation, equal, fail)
import Test exposing (..)
import Thingy exposing (Flavour(..), Thingy(..))


{-|

    Test the combination of various things

-}
mixingSuite : Test
mixingSuite =
    describe "Mixing ingredients"
        [ testMix "Mixing Flour with Water produces a Bun"
            ( flour, water )
            (Just bun)
        , testMix "Mixing Water with Sugar produces a *Sweet* Water"
            ( water, sugar )
            (Just <| Water (Just Sugar))
        , testMix "Already *sweet* Water and  Sugar do not mix"
            ( Water (Just Sugar), sugar )
            Nothing
        , testMix "Already *sweet* Flour and Sugar do not mix"
            ( Flour (Just Sugar), sugar )
            Nothing
        , testMix "Mixing Flour with Sugar produces a *Sweet* Flour"
            ( flour, sugar )
            (Just <| Flour (Just Sugar))
        , testMix "Mixing Flour with Chilli produces a *Spicy* Flour"
            ( flour, chilli )
            (Just <| Flour (Just Chilli))
        , testMix "Mixing *sweet* Water with (neutral) Flour produces a *sweet* Bun"
            ( Water (Just Sugar), Flour Nothing )
            (Just <| Bun (Just Sugar))
        , testMix "Mixing *sweet* Flour with (neutral) Water produces a *sweet* Bun"
            ( Water Nothing, Flour (Just Sugar) )
            (Just <| Bun (Just Sugar))
        , testMix "Mixing *sweet* Flour with *sweet* Water produces a *sweet* Bun"
            ( Water (Just Sugar), Flour (Just Sugar) )
            (Just <| Bun (Just Sugar))
        , testMix "Can not mix *sweet* Flour with *spicy* Water"
            ( Water (Just Chilli), Flour (Just Sugar) )
            Nothing
        , testMix "Can not mix packaged Sugar with Water"
            ( Water Nothing, Flavouring { flavour = Sugar, packaged = True } )
            Nothing
        , testMix "Can not mix packaged Sugar with Flour"
            ( Flour Nothing, Flavouring { flavour = Sugar, packaged = True } )
            Nothing
        ]


{-| Test that mixing of both A+B and B+A yield the same expected result
-}
testMix : String -> ( Thingy, Thingy ) -> Maybe Thingy -> Test
testMix message ( ingredient1, ingredient2 ) expectedResult =
    describe message
        [ test
            ("Mixing ["
                ++ (ingredient1 |> Debug.toString)
                ++ "] with ["
                ++ (ingredient2 |> Debug.toString)
                ++ "] should produce ["
                ++ (expectedResult |> Debug.toString)
                ++ "]"
            )
          <|
            \() ->
                Thingy.mixIngredients ingredient1 ingredient2 |> equal expectedResult
        , test
            ("Mixing ["
                ++ (ingredient2 |> Debug.toString)
                ++ "] with ["
                ++ (ingredient1 |> Debug.toString)
                ++ "] should produce ["
                ++ (expectedResult |> Debug.toString)
                ++ "]"
            )
          <|
            \() ->
                Thingy.mixIngredients ingredient2 ingredient1 |> equal expectedResult
        ]



-- # HELPERS


sugar : Thingy
sugar =
    Flavouring { flavour = Sugar, packaged = False }


chilli : Thingy
chilli =
    Flavouring { flavour = Chilli, packaged = False }


water : Thingy
water =
    Water Nothing


flour : Thingy
flour =
    Flour Nothing


bun : Thingy
bun =
    Bun Nothing
