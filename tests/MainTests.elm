module MainTests exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, float, floatRange, int, list, string)
import Html exposing (div, li, ul)
import Html.Attributes exposing (class)
import Main
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes, tag, text)


suite : Test
suite =
    describe "The Main module"
        [ describe "Main.model"
            [ describe "Checking initialization"
                [ test "Slider val starts at 1" <|
                    \_ ->
                        Expect.equal Main.init.sliderVal 1.0
                , test "Input string is empty" <|
                    \_ ->
                        Expect.equal Main.init.total ""
                , test "Original recipe list is empty" <|
                    \_ ->
                        Expect.equal (List.length Main.init.listNewIngredients) 0
                , test "Scaled recipe list is empty" <|
                    \_ ->
                        Expect.equal (List.length Main.init.changedNewIngs) 0
                , test "First error message is false" <|
                    \_ ->
                        Expect.equal Main.init.noQuantFound False
                , test "Second error messiage is false" <|
                    \_ ->
                        Expect.equal Main.init.inputError False
                ]
            , describe "Entering random strings to recipe"
                [ fuzz string "Adding any string to original recipe list" <|
                    \strng ->
                        let
                            mod =
                                Main.init
                                    |> Main.update (Main.ChangeTotal strng)
                                    |> Main.update (Main.KeyDown 13)
                        in
                        if strng == "" then
                            Expect.equal 0 (List.length mod.listNewIngredients)

                        else
                            Expect.equal 1 (List.length mod.listNewIngredients)
                , fuzz string "Adding any string to scaled recipe list" <|
                    \strng ->
                        let
                            mod =
                                Main.init
                                    |> Main.update (Main.ChangeTotal strng)
                                    |> Main.update (Main.KeyDown 13)
                        in
                        if strng == "" then
                            Expect.equal 0 (List.length mod.changedNewIngs)

                        else
                            Expect.equal 1 (List.length mod.changedNewIngs)
                ]
            , describe "Adding any number plus a string to recipe list"
                [ fuzz (floatRange 0.0 10000.0) "Adding float with a fixed string to recipe list" <|
                    \flt ->
                        let
                            mod =
                                Main.init
                                    |> Main.update (Main.ChangeTotal (String.fromFloat flt ++ " cups of water"))
                                    |> Main.update (Main.KeyDown 13)
                        in
                        case List.head mod.changedNewIngs of
                            Nothing ->
                                Expect.equal 1 3

                            Just x ->
                                case String.toFloat x.quantity of
                                    Nothing ->
                                        Expect.equal 1 2

                                    Just y ->
                                        Expect.within (Absolute 0.00001) flt y
                ]
            ]
        ]
