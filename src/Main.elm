module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (title)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Ingredient = 
              {
                food : String
              , quantity : Int
              }


type alias Model = Ingredient


init : Model
init =
  {food = "", quantity = 0}



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model = model



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [  text "Title"
    ]