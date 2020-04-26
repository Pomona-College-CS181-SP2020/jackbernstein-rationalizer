module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json



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
    , newIngredients : List Ingredient
    , tempFood : String
    , tempQuant : String
    , tempUnit : String
    , optionIng : Ingredient
    , optionFood : String
    , optionNumb : String
    , emptyName : Bool
    , invalidQuant : Bool
    , sliderVal : Float
    }


type alias Model =
    Recipe


init : Model
init =
    { ingredients = []
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
    , emptyName = False
    , invalidQuant = False
    , sliderVal = 1.0
    }



-- UPDATE


type Msg
    = ChangeTempFood String
    | ChangeTempQuant String
    | ChangeTempUnit String
    | AddToList Ingredient
    | SetOption String
    | NewRationalize String
    | Scale Float
    | KeyDown Int
    | UpdateSlider String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSlider strng ->
            case String.toFloat strng of
                Just x ->
                    { model | sliderVal = x }

                Nothing ->
                    model

        KeyDown key ->
            if key == 13 then
                update (AddToList { food = model.tempFood, quantity = model.tempQuant, unit = model.tempUnit }) model

            else
                model

        NewRationalize strng ->
            case String.toFloat strng of
                Nothing ->
                    { model | optionNumb = strng }

                Just x ->
                    let
                        scalr =
                            findQuant model.newIngredients model.optionFood
                    in
                    case scalr of
                        Nothing ->
                            { model | optionNumb = strng }

                        Just y ->
                            { model | newIngredients = newMapIngredients model.newIngredients (x / y), optionNumb = strng }

        SetOption strng ->
            if strng == "Select an Ingredient" then
                { model | optionFood = "", optionNumb = "" }

            else
                { model | optionFood = strng, optionNumb = "" }

        AddToList ing ->
            verifyAdd model ing

        ChangeTempFood food ->
            { model | tempFood = food }

        ChangeTempQuant quant ->
            { model | tempQuant = quant }

        ChangeTempUnit unit ->
            { model | tempUnit = unit }

        Scale flt ->
            { model | newIngredients = newMapIngredients model.newIngredients flt }


findQuant : List Ingredient -> String -> Maybe Float
findQuant lst strng =
    case lst of
        [] ->
            Nothing

        ing :: ings ->
            if ing.food == strng then
                String.toFloat ing.quantity

            else
                findQuant ings strng


verifyAdd : Model -> Ingredient -> Model
verifyAdd model ing =
    if String.isEmpty ing.food then
        { model | emptyName = True }

    else
        case String.toFloat ing.quantity of
            Nothing ->
                { model | emptyName = False, invalidQuant = True }

            Just x ->
                { model | ingredients = model.ingredients ++ [ ing ], tempFood = "", tempQuant = "", tempUnit = "", newIngredients = model.newIngredients ++ [ ing ], emptyName = False, invalidQuant = False }


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


newMapIngredients : List Ingredient -> Float -> List Ingredient
newMapIngredients lst flt =
    case lst of
        [] ->
            []

        food :: foods ->
            case String.toFloat food.quantity of
                Nothing ->
                    food :: newMapIngredients foods flt

                Just x ->
                    { food | quantity = String.fromFloat (x * flt) } :: newMapIngredients foods flt



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "ingredientInput" ]
            [ text "Add ingredient"
            , div [ onKeyDown KeyDown ]
                [ input [ placeholder "Quantity", value model.tempQuant, onInput ChangeTempQuant ] []
                , input [ placeholder "Units (optional)", value model.tempUnit, onInput ChangeTempUnit ] []
                , input [ placeholder "Ingredient", value model.tempFood, onInput ChangeTempFood ] []
                ]
            ]
        , div [ class "slider" ]
            [ div [] [ text (String.fromFloat model.sliderVal) ]
            , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "10", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
            ]
        , div []
            [ div [ class "recipeContainer" ]
                ([ text "recipe"
                 ]
                    ++ viewIngredients model.ingredients
                )
            , div [ class "filler " ] [ text "test" ]
            , div [ class "recipeContainer" ]
                ([ text "scaled"
                 ]
                    ++ viewIngredients model.newIngredients
                )
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


ingredientOptions : List Ingredient -> List (Html Msg)
ingredientOptions ings =
    case ings of
        [] ->
            []

        food :: foods ->
            option [ value food.food ] [ text food.food ] :: ingredientOptions foods


viewIngredients : List Ingredient -> List (Html Msg)
viewIngredients lst =
    case lst of
        [] ->
            []

        food :: foods ->
            if String.isEmpty food.unit then
                [ div []
                    [ text (food.quantity ++ " " ++ food.food)
                    ]
                ]
                    ++ viewIngredients foods

            else
                [ div []
                    [ text (food.quantity ++ " " ++ food.unit ++ " of " ++ food.food)
                    ]
                ]
                    ++ viewIngredients foods
