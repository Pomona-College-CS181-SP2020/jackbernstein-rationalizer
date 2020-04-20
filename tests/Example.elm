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
                [ test "Initial list has one item" <|
                    \_ ->
                        Expect.equal (List.length Main.init.ingredients) 1
                , test "Inital complete boolean is false" <|
                    \_ ->
                        Expect.equal Main.init.complete False
                ]
            ]
        , describe "Main.view"
            [ describe "Main.view.inital"
                [ test "Add ingredients button exists" <|
                    \_ ->
                        Main.view Main.init
                            |> Query.fromHtml
                            |> Query.has [ tag "button", text "Add another ingredient" ]
                , test "Remove ingredients button exists" <|
                    \_ ->
                        Main.view Main.init
                            |> Query.fromHtml
                            |> Query.has [ tag "button", text "Remove ingredient" ]
                , test "Three input fields" <|
                    \_ ->
                        Main.view Main.init
                            |> Query.fromHtml
                            |> Query.findAll [ tag "input" ]
                            |> Query.count (Expect.equal 3)
                ]
            ]
        ]
