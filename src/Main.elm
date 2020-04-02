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
              , quantity : Float
              }


type alias Model = Ingredient


init : Model
init =
  {food = "", quantity = 0}



-- UPDATE


type Msg
  = Food String
  | Quantity String


update : Msg -> Model -> Model
update msg model =
            case msg of 
                Food newFood -> 
                    {model | food = newFood }
                
                Quantity quant ->
                    case String.toFloat quant of
                        Just x -> 
                            {model | quantity = x}
                        Nothing ->
                            model



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [  h1 [] [text "Secret Krabby Patty recipe"]
    , div [] [text "Add ingredients"]
    , div [] [
          input [ placeholder "Ingredient", value model.food, onInput Food ] []
        , input [ placeholder "Quantity", value (String.fromFloat model.quantity), onInput Quantity] []
        ] 
    ]