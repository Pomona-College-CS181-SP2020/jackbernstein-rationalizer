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
                food1 : String
              , quantity1 : Float
              , food2 : String
              , quantity2 : Float
              ,food3 : String
              , quantity3 : Float
              }


type alias Model = Ingredient


init : Model
init =
  {food1 = ""
  , quantity1 = 0
  , food2 = ""
  , quantity2 = 0
  , food3 = ""
  , quantity3 = 0
  }



-- UPDATE


type Msg
  = Food Int String
  | Quantity Int String


update : Msg -> Model -> Model
update msg model =
            case msg of 
                Food num newFood -> 
                    case num of 
                        1 -> {model | food1 = newFood}
                        2 -> {model | food2 = newFood}
                        3 -> {model | food3 = newFood}
                        _ -> model
                
                Quantity num quant ->
                    case String.toFloat quant of
                        Just x -> 
                            case num of
                                1 -> {model | quantity1 = x}
                                2 -> {model | quantity2 = x}
                                3 -> {model | quantity3 = x}
                                _ -> model
                        Nothing ->
                            model



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [  h1 [] [text "Secret Krabby Patty recipe"]
    , div [] [text "Add ingredients"]
    , div [] [
          input [ placeholder "Ingredient", value model.food1, onInput (Food 1)] []
        , input [ placeholder "Quantity", value (String.fromFloat model.quantity1), onInput (Quantity 1)] []
        ] 
    , div [] [
          input [ placeholder "Ingredient", value model.food2, onInput (Food 2)] []
        , input [ placeholder "Quantity", value (String.fromFloat model.quantity2), onInput (Quantity 2)] []
        ]
    , div [] [
          input [ placeholder "Ingredient", value model.food3, onInput (Food 3)] []
        , input [ placeholder "Quantity", value (String.fromFloat model.quantity3), onInput (Quantity 3)] []
        ]
    ]