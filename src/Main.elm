module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, Attribute, button, div, text, h1, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)



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
  = Change String


update : Msg -> Model -> Model
update msg model =
            case msg of 
                Change newFood -> 
                    {model | food = newFood }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [  h1 [] [text "Title"]
    , text "Add an ingredient"
    , input [ placeholder "Ingredient", value model.food, onInput Change ] []
    ]