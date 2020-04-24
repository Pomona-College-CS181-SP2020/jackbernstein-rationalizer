module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
            [ describe "Main.model.initial"
                [ test "Inital complete boolean is false" <|
                    \_ ->
                        Expect.equal Main.init.complete False
                ]
            ]
        ]
