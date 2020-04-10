module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, h1, img, input, li, ol, text)
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
    , submitError : Bool
    , rationalize : Bool
    , scale : Bool
    , newIngredients : List Ingredient
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
    , submitError = False
    , rationalize = False
    , scale = False
    , newIngredients = []
    }



-- UPDATE


type Msg
    = Food Int String
    | Quantity Int String
    | AddFood
    | Unit Int String
    | RecipeDone
    | DeleteFood
    | RationalizeTrue
    | ScaleTrue
    | Rationalize Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Food num newFood ->
            { model | ingredients = updateFood model.ingredients num newFood, submitError = False }

        Quantity num quant ->
            { model | ingredients = updateQuantity model.ingredients num quant, submitError = False }

        AddFood ->
            { model | ingredients = List.append model.ingredients [ { food = "", quantity = "", unit = "" } ], submitError = False }

        Unit num unt ->
            { model | ingredients = updateUnits model.ingredients num unt, submitError = False }

        RecipeDone ->
            { model | complete = areIngredientsFilled model.ingredients True, submitError = not (areIngredientsFilled model.ingredients True) }

        DeleteFood ->
            if List.length model.ingredients > 1 then
                { model | ingredients = List.take (List.length model.ingredients - 1) model.ingredients }

            else
                model

        RationalizeTrue ->
            let 
                ingreds = newRecipe model.ingredients
            in
                { model | rationalize = True , newIngredients = ingreds }

        ScaleTrue ->
            { model | scale = True }
        
        Rationalize num rat -> 
            let
                multiplier = getMultiplier model.ingredients num rat 
            in
                {model | newIngredients = mapIngredients model.newIngredients model.ingredients multiplier 0}


getIngQuant : List Ingredient -> Int -> Float
getIngQuant lst num =
        case lst of 
            [] -> 0.0
            ing :: ings ->
                case num of 
                    0 -> case String.toFloat ing.quantity of 
                        Just x -> x
                        Nothing -> 0.0
                    x -> getIngQuant ings (num - 1)


mapIngredients : List Ingredient -> List Ingredient ->  Float -> Int -> List Ingredient
mapIngredients lst oldIngs flt num = 
    case lst of 
        [] -> []
        ing :: ings -> 
                {ing | quantity = (String.fromFloat (getIngQuant oldIngs num * flt))} :: mapIngredients ings oldIngs flt (num + 1)


newRecipe : List Ingredient -> List Ingredient
newRecipe lst = 
    case lst of 
        [] -> []
        ing :: ings ->
            {ing | quantity = ""} :: newRecipe ings



getMultiplier : List Ingredient -> Int -> String -> Float
getMultiplier lst num rat = 
    case String.toFloat rat of 
        Just x -> 
            case num of 
                0 -> 
                    case lst of 
                        [] -> 1.0
                        ing :: ings -> 
                            case String.toFloat ing.quantity of 
                                Just y -> x/y 
                                Nothing -> 1.0

                z -> getMultiplier lst (num - 1) rat 
        Nothing -> 1.0



areIngredientsFilled : List Ingredient -> Bool -> Bool
areIngredientsFilled ings b =
    case ings of
        [] ->
            True

        i :: is ->
            if String.endsWith "." i.quantity then
                False

            else if String.isEmpty i.food || String.isEmpty i.quantity then
                False

            else
                areIngredientsFilled is b


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
            case model.rationalize of
                True ->
                    div []
                        ([ div [] [ text "Select an ingredient to rationalize " ]
                         ]
                            ++ buttonIngredients model.newIngredients 0
                        )

                False ->
                    case model.scale of
                        True ->
                            div [] []

                        False ->
                            div []
                                [ h1 [] [ text "Time to Rationalize!" ]
                                , div [] [ text "This is your recipe" ]
                                , ol [] (listIngredients model.ingredients)
                                , div [] [ text """Would you like to select an ingredient, input a quantity, and build the new recipe around it, or would you 
                                                like to scale all the ingredients?""" ]
                                , div [] [ button [ onClick RationalizeTrue ] [ text "Select ingredient" ], button [ onClick ScaleTrue ] [ text "scale" ] ]
                                ]

        False ->
            div []
                ([ h1 [] [ text "Secret Krabby Patty Formula" ]
                 , img [ src "burger.jpg", style "height" "90px", style "width" "160px" ] []
                 , div [] [ text "Add ingredients!" ]
                 , div [] [ button [ onClick AddFood ] [ text "Add another ingredient" ], button [ onClick DeleteFood ] [ text "Remove ingredient" ] ]
                 ]
                    ++ viewIngredients model.ingredients 0
                    ++ [ button [ onClick RecipeDone ] [ text "Submit Formula" ] ]
                    ++ errorMessage model
                )


buttonIngredients : List Ingredient -> Int ->  List (Html Msg)
buttonIngredients lst numb =
    case lst of
        [] ->
            []

        ing :: ings ->
            div [] [ text ing.food, input [value ing.quantity, onInput (Rationalize numb)] [] ] :: buttonIngredients ings (numb + 1)


listIngredients : List Ingredient -> List (Html Msg)
listIngredients lst =
    case lst of
        [] ->
            []

        ing :: ings ->
            li [] [ text (ing.food ++ ": " ++ ing.quantity ++ " " ++ ing.unit) ] :: listIngredients ings


errorMessage : Model -> List (Html Msg)
errorMessage model =
    case model.submitError of
        True ->
            [ div [] [ text "Must fill out all ingredients and quantities" ] ]

        False ->
            []


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
