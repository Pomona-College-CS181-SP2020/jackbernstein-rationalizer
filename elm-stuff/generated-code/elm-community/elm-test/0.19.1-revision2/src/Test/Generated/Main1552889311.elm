module Test.Generated.Main1552889311 exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 165457621160381, processes = 4, paths = ["/Users/jackbernstein/elmProjects/jackbernstein-rationalizer/tests/Example.elm"]}