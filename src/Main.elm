module Main exposing (..)

import Browser
import Html exposing (..)
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
    , tempFood : String
    , tempQuant : String
    , tempUnit : String
    , optionIng : Ingredient
    , optionFood : String
    , optionNumb : String
    }


type alias Model =
    Recipe


init : Model
init =
    { ingredients = []
    , complete = False
    , submitError = False
    , rationalize = False
    , scale = False
    , newIngredients = []
    , tempFood = ""
    , tempQuant = ""
    , tempUnit = ""
    , optionIng =
        { food = ""
        , quantity = ""
        , unit = ""
        }
    , optionFood = ""
    , optionNumb = ""
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
    | Scale Float
    | Back
    | BackRationalize
    | BackScale
    | Reset
    | ChangeTempFood String
    | ChangeTempQuant String
    | ChangeTempUnit String
    | AddToList Ingredient
    | SetOption String
    | NewRationalize String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewRationalize strng ->
            case String.toFloat strng of
                Nothing ->
                    model

                Just x ->
                    { model | optionNumb = strng }

        SetOption strng ->
            if strng == "Select an Ingredient" then
                { model | optionFood = "" }

            else
                { model | optionFood = strng }

        AddToList ing ->
            verifyAdd model ing

        ChangeTempFood food ->
            { model | tempFood = food }

        ChangeTempQuant quant ->
            { model | tempQuant = quant }

        ChangeTempUnit unit ->
            { model | tempUnit = unit }

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
                ingreds =
                    newRecipe model.ingredients
            in
            { model | rationalize = True, newIngredients = ingreds }

        ScaleTrue ->
            { model | scale = True, newIngredients = model.ingredients }

        Rationalize num rat ->
            let
                multiplier =
                    getMultiplier model.ingredients num rat
            in
            { model | newIngredients = mapIngredients model.newIngredients model.ingredients multiplier 0 }

        Scale flt ->
            { model | newIngredients = mapIngredients model.newIngredients model.ingredients flt 0 }

        Back ->
            { model | complete = False, submitError = False }

        BackRationalize ->
            { model | rationalize = False }

        BackScale ->
            { model | scale = False }

        Reset ->
            init


rationalizeIngs : Model -> String -> Model
rationalizeIngs model strng =
    model



-- if String.isEmpty model.optionIng.food
-- then { model | optionNumb = strng }
-- else {model | optionNumb = strng}


verifyAdd : Model -> Ingredient -> Model
verifyAdd model ing =
    if String.isEmpty ing.food then
        model

    else
        case String.toFloat ing.quantity of
            Nothing ->
                model

            Just x ->
                { model | ingredients = model.ingredients ++ [ ing ], tempFood = "", tempQuant = "", tempUnit = "", newIngredients = model.newIngredients ++ [ ing ] }


getIngQuant : List Ingredient -> Int -> Float
getIngQuant lst num =
    case lst of
        [] ->
            0.0

        ing :: ings ->
            case num of
                0 ->
                    case String.toFloat ing.quantity of
                        Just x ->
                            x

                        Nothing ->
                            0.0

                x ->
                    getIngQuant ings (num - 1)


mapIngredients : List Ingredient -> List Ingredient -> Float -> Int -> List Ingredient
mapIngredients lst oldIngs flt num =
    case lst of
        [] ->
            []

        ing :: ings ->
            { ing | quantity = String.fromFloat (getIngQuant oldIngs num * flt) } :: mapIngredients ings oldIngs flt (num + 1)


newRecipe : List Ingredient -> List Ingredient
newRecipe lst =
    case lst of
        [] ->
            []

        ing :: ings ->
            { ing | quantity = "" } :: newRecipe ings


getMultiplier : List Ingredient -> Int -> String -> Float
getMultiplier lst num rat =
    case String.toFloat rat of
        Just x ->
            case num of
                0 ->
                    case lst of
                        [] ->
                            1.0

                        ing :: ings ->
                            case String.toFloat ing.quantity of
                                Just y ->
                                    x / y

                                Nothing ->
                                    1.0

                z ->
                    getMultiplier lst (num - 1) rat

        Nothing ->
            1.0


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
    div [ class "row" ]
        [ div [ class "column" ]
            ([ h1 [] [ text "OG recipe" ]
             , div [] [ text "Add ingredients!" ]
             , div []
                [ input [ placeholder "Quantity", value model.tempQuant, onInput ChangeTempQuant ] []
                , input [ placeholder "Units (optional)", value model.tempUnit, onInput ChangeTempUnit ] []
                , input [ placeholder "Ingredient", value model.tempFood, onInput ChangeTempFood ] []
                ]
             , button [ onClick (AddToList { food = model.tempFood, quantity = model.tempQuant, unit = model.tempUnit }) ] [ text "Add Ingredient" ]
             ]
                ++ viewIngredients model.ingredients
            )
        , div [ class "column" ]
            ([ h1 [] [ text "Time to rationalize this recipe into oblivion" ]
             , h2 [] [ text "New recipe" ]
             , div [] [ button [ onClick (Scale 0.5) ] [ text "Halve" ], button [ onClick (Scale 1) ] [ text "Original" ], button [ onClick (Scale 2) ] [ text "Double" ] ]
             , div []
                [ select [ onInput SetOption ] ([ option [] [ text "Select an Ingredient" ] ] ++ ingredientOptions model.ingredients)
                , input [ value model.optionNumb, onInput NewRationalize ] []
                , text ("foodiefood: " ++ model.optionFood)
                ]
             ]
                ++ viewIngredients model.newIngredients
            )
        ]


ingredientOptions : List Ingredient -> List (Html Msg)
ingredientOptions ings =
    case ings of
        [] ->
            []

        food :: foods ->
            option [ value food.food ] [ text food.food ] :: ingredientOptions foods



-- view2 : Model -> Html Msg
-- view2 model =
--     case model.complete of
--         True ->
--             case model.rationalize of
--                 True ->
--                     div []
--                         ([ h1 [] [ text "Rationalize Ingredients " ]
--                          ]
--                             ++ buttonIngredients model.newIngredients 0
--                             ++ [ button [ onClick BackRationalize ] [ text "Back" ] ]
--                             ++ [ button [ onClick Reset ] [ text "Start Over" ] ]
--                         )
--                 False ->
--                     case model.scale of
--                         True ->
--                             div []
--                                 ([ h1 [] [ text "Scale Ingredients" ]
--                                  , div [] [ button [ onClick (Scale 0.5) ] [ text "Halve" ], button [ onClick (Scale 1) ] [ text "Original" ], button [ onClick (Scale 2) ] [ text "Double" ] ]
--                                  ]
--                                     ++ listIngredients model.newIngredients
--                                     ++ [ button [ onClick BackScale ] [ text "Back" ]
--                                        , button [ onClick Reset ] [ text "Reset" ]
--                                        ]
--                                 )
--                         False ->
--                             div [ class "row" ]
--                                 [ div [ class "column" ]
--                                     ([ h1 [] [ text "Secret Krabby Patty Formula" ]
--                                      , div [] [ text "Add ingredients!" ]
--                                      , div [] [ button [ onClick AddFood ] [ text "Add another ingredient" ], button [ onClick DeleteFood ] [ text "Remove ingredient" ] ]
--                                      ]
--                                         ++ viewIngredients model.ingredients 0
--                                         ++ [ button [ onClick RecipeDone ] [ text "Submit Formula" ] ]
--                                         ++ errorMessage model
--                                     )
--                                 , div [ class "column" ]
--                                     [ h1 [] [ text "Time to Rationalize!" ]
--                                     , div [] [ text "This is your recipe" ]
--                                     , ol [] (listIngredients model.ingredients)
--                                     , div [] [ text """Would you like to select an ingredient, input a quantity, and build the new recipe around it, or would you
--                                                     like to scale all the ingredients?""" ]
--                                     , div []
--                                         [ button [ onClick RationalizeTrue ] [ text "Select ingredient" ]
--                                         , button [ onClick ScaleTrue ] [ text "scale" ]
--                                         , button [ onClick Back ] [ text "Back" ]
--                                         ]
--                                     ]
--                                 ]
--         False ->
--             div [ class "row" ]
--                 [ div [ class "column" ]
--                     ([ h1 [] [ text "Secret Krabby Patty Formula" ]
--                      , div [] [ text "Add ingredients!" ]
--                      , div [] [ button [ onClick AddFood ] [ text "Add another ingredient" ], button [ onClick DeleteFood ] [ text "Remove ingredient" ] ]
--                      ]
--                         ++ viewIngredients model.ingredients 0
--                         ++ [ button [ onClick RecipeDone ] [ text "Submit Formula" ] ]
--                         ++ errorMessage model
--                     )
--                 ]


buttonIngredients : List Ingredient -> Int -> List (Html Msg)
buttonIngredients lst numb =
    case lst of
        [] ->
            []

        ing :: ings ->
            div [] [ text ing.food, input [ onInput (Rationalize numb), value ing.quantity ] [], text ing.unit ] :: buttonIngredients ings (numb + 1)


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


viewIngredients : List Ingredient -> List (Html Msg)
viewIngredients lst =
    case lst of
        [] ->
            []

        food :: foods ->
            if String.isEmpty food.unit then
                [ div []
                    [ text (food.quantity ++ ", " ++ food.food)
                    ]
                ]
                    ++ viewIngredients foods

            else
                [ div []
                    [ text (food.quantity ++ " " ++ food.unit ++ " of " ++ food.food)
                    ]
                ]
                    ++ viewIngredients foods
