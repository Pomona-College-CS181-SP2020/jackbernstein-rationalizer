module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "The Main module"
            [ describe "Main.data"
                [ test "Initial list has one item" <|
                    \_ ->
                        Expect.equal (List.length Main.init.ingredients) 1
                ]
            ]
        ]
