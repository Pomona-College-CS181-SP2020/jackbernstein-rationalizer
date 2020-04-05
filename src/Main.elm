module Main exposing (..)


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
  , unit : String
  }

type alias Recipe = 
  {
    ingredients : List Ingredient
  , complete : Bool
  }


type alias Model = Recipe


init : Model
init =
  {ingredients = 
  [
    {food = ""
  , quantity = 0
  , unit = ""}
  ]
  , complete = False
  }



-- UPDATE


type Msg
  = Food Int String
  | Quantity Int String
  | AddFood
  | Unit Int String
  | RecipeDone
  | DeleteFood


update : Msg -> Model -> Model
update msg model =
  case msg of 
      Food num newFood -> {model | ingredients = updateFood model.ingredients num newFood}
      Quantity num quant -> {model | ingredients = updateQuantity model.ingredients num quant}
      AddFood -> {model | ingredients = List.append model.ingredients [{food = "", quantity = 0, unit = ""}]}
      Unit num unt-> model
      RecipeDone -> {model | complete = True}
      DeleteFood -> 
        if List.length model.ingredients > 1
        then {model | ingredients = List.take (List.length model.ingredients - 1) model.ingredients}
        else model


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
      (ing::ings) -> 
        case String.toFloat newQuant of
          Nothing -> (ing::ings)
          Just x -> ({ing | quantity = x}::ings)
    x -> 
      case lst of
        [] -> []
        (ing::ings) -> ing :: updateQuantity ings (num - 1) newQuant




-- VIEW


view : Model -> Html Msg
view model =
  case model.complete of 
    True -> 
      div [] [
         
      ]
    False -> 
      div []
        ([  h1 [] [text "Secret Krabby Patty Formula"]
        , div [] [text "Add ingredients!"]
        , div [] [button [onClick AddFood] [text "Add another ingredients"], button [onClick DeleteFood] [text "Remove ingredient"]]
        ] ++ viewIngredients model.ingredients 0
        ++ [button [onClick RecipeDone] [text "Submit Formula"]])

listIngredients : List Ingredient -> Html Msg
listIngredients lst = div [] []

    

viewIngredients : List Ingredient -> Int -> List (Html Msg)
viewIngredients lst num = 
  case lst of
    [] -> []
    (food::foods) -> 
        [div [] 
        [
          input [placeholder "Ingredient", value food.food, onInput (Food num)] []
        , input [placeholder "Quantity", value (String.fromFloat food.quantity), onInput (Quantity num)] []
        , input [placeholder "Units (optional)", value food.unit, onInput (Unit num)] []
        ]
        ]
        ++ (viewIngredients foods (num + 1))