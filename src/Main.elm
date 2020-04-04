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


type alias Model = List Ingredient


init : Model
init =
  [{food = ""
  , quantity = 0}]



-- UPDATE


type Msg
  = Food Int String
  | Quantity Int String
  | AddFood


update : Msg -> Model -> Model
update msg model =
            case msg of 
                Food num newFood -> updateFood model num newFood 
                Quantity num quant -> updateQuantity model num quant
                AddFood -> List.append model [{food = "", quantity = 0}]

updateFood : List Ingredient -> Int -> String -> List Ingredient
updateFood lst num newFood = 
  case num of
    0 -> case lst of
          [] -> []
          (ing::ings) -> ({ing | food = newFood}::ings)
    x -> case lst of
          [] -> []
          (ing::ings) -> ing :: updateFood ings (num - 1) newFood

updateQuantity : List Ingredient -> Int -> String -> List Ingredient
updateQuantity lst num newQuant =
  case num of 
    0 -> case lst of
          [] -> []
          (ing::ings) -> case String.toFloat newQuant of
                          Nothing -> (ing::ings)
                          Just x -> ({ing | quantity = x}::ings)
    x -> case lst of
          [] -> []
          (ing::ings) -> ing :: updateQuantity ings (num - 1) newQuant




-- VIEW


view : Model -> Html Msg
view model =
  div []
    ([  h1 [] [text "Secret Krabby Patty recipe"]
    , div [] [text "Add ingredients!"]
    , button [onClick AddFood] [text "Add another ingredients"]
     ] ++ viewIngredients model 0)
    

viewIngredients : List Ingredient -> Int -> List (Html Msg)
viewIngredients lst num = 
  case lst of
    [] -> []
    (food::foods) -> [div [] [
          input [placeholder "Ingredient", value food.food, onInput (Food num)] []
        , input [placeholder "Quantity", value (String.fromFloat food.quantity), onInput (Quantity num)] []]]
        ++ (viewIngredients foods (num + 1))