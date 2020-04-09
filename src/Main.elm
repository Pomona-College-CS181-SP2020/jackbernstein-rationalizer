module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, h1, img, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Ingredient =
    { food : String
    , quantity : String
    , unit : String
    }


type alias Recipe =
    { ingredients : List Ingredient
    , complete : Bool
    }


type alias Model =
    Recipe


init : Model
init =
    { ingredients =
        [ { food = ""
          , quantity = ""
          , unit = ""
          }
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
        Food num newFood ->
            { model | ingredients = updateFood model.ingredients num newFood }

        Quantity num quant ->
            { model | ingredients = updateQuantity model.ingredients num quant }

        AddFood ->
            { model | ingredients = List.append model.ingredients [ { food = "", quantity = "", unit = "" } ] }

        Unit num unt ->
            { model | ingredients = updateUnits model.ingredients num unt }

        RecipeDone ->
            { model | complete = areIngredientsFilled model.ingredients True }

        DeleteFood ->
            if List.length model.ingredients > 1 then
                { model | ingredients = List.take (List.length model.ingredients - 1) model.ingredients }

            else
                model


areIngredientsFilled : List Ingredient -> Bool -> Bool
areIngredientsFilled ings b =
    case ings of
        [] ->
            True

        i :: is ->
            if String.endsWith "." i.quantity
            then False
            else  
                if String.isEmpty i.food || String.isEmpty i.quantity
                then False
                else areIngredientsFilled is b


updateFood : List Ingredient -> Int -> String -> List Ingredient
updateFood lst num newFood =
    case num of
        0 ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    { ing | food = newFood } :: ings

        x ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    ing :: updateFood ings (num - 1) newFood


updateQuantity : List Ingredient -> Int -> String -> List Ingredient
updateQuantity lst num newQuant =
    case num of
        0 ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    case String.toFloat newQuant of
                        Nothing ->
                            checkIncompleteFloat ing newQuant :: ings

                        Just x ->
                            { ing | quantity = newQuant } :: ings

        x ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    ing :: updateQuantity ings (num - 1) newQuant


checkIncompleteFloat : Ingredient -> String -> Ingredient
checkIncompleteFloat ing str =
    let
        digits =
            String.foldr (\a b -> (Char.isDigit a || a == '.') && b) True str

        decimals =
            String.indexes "." str
    in
    if digits && List.length decimals < 2 then
        { ing | quantity = str }

    else
        ing


updateUnits : List Ingredient -> Int -> String -> List Ingredient
updateUnits lst num newUnit =
    case num of
        0 ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    { ing | unit = newUnit } :: ings

        x ->
            case lst of
                [] ->
                    []

                ing :: ings ->
                    ing :: updateUnits ings (num - 1) newUnit



-- VIEW


view : Model -> Html Msg
view model =
    case model.complete of
        True ->
            div []
                []

        False ->
            div []
                ([ h1 [] [ text "Secret Krabby Patty Formula" ]
                 , img [ src "burger.jpg" ] []
                 , div [] [ text "Add ingredients!" ]
                 , div [] [ button [ onClick AddFood ] [ text "Add another ingredient" ], button [ onClick DeleteFood ] [ text "Remove ingredient" ] ]
                 ]
                    ++ viewIngredients model.ingredients 0
                    ++ [ button [ onClick RecipeDone ] [ text "Submit Formula" ] ]
                )


listIngredients : List Ingredient -> Html Msg
listIngredients lst =
    div [] []


viewIngredients : List Ingredient -> Int -> List (Html Msg)
viewIngredients lst num =
    case lst of
        [] ->
            []

        food :: foods ->
            [ div []
                [ input [ placeholder "Ingredient", value food.food, onInput (Food num) ] []
                , input [ placeholder "Quantity", value food.quantity, onInput (Quantity num) ] []
                , input [ placeholder "Units (optional)", value food.unit, onInput (Unit num) ] []
                ]
            ]
                ++ viewIngredients foods (num + 1)

