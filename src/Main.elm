module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Round exposing (..)



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
    | Scale Float
    | KeyDown Int
    | UpdateSlider String
    | Delete Ingredient


update : Msg -> Model -> Model
update msg model =
    case msg of
        Delete food -> 
            {model | ingredients = del model.ingredients food, newIngredients = newMapIngredients ( del model.ingredients food) model.sliderVal}

        UpdateSlider strng ->
            case String.toFloat strng of
                Just x ->
                    { model | sliderVal = x, newIngredients = newMapIngredients model.ingredients x }

                Nothing ->
                    model

        KeyDown key ->
            if key == 13 then
                update (AddToList { food = model.tempFood, quantity = model.tempQuant, unit = model.tempUnit }) model

            else
                model

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


del : List Ingredient -> Ingredient -> List Ingredient 
del lst food =
    case lst of 
        [] -> 
            []
        
        ing :: ings -> 
            if food == ing
            then ings
            else ing :: del ings food 


verifyAdd : Model -> Ingredient -> Model
verifyAdd model ing =
    if String.isEmpty ing.food then
        { model | emptyName = True }

    else
        case String.toFloat ing.quantity of
            Nothing ->
                { model | emptyName = False, invalidQuant = True }

            Just x ->
                { model | ingredients = model.ingredients ++ [ ing ], tempFood = "", tempQuant = "", tempUnit = "", emptyName = False, invalidQuant = False, newIngredients = newMapIngredients (model.ingredients ++ [ ing ]) model.sliderVal }


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
                    { food | quantity = Round.round 1 (x * flt) } :: newMapIngredients foods flt



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "ingredientInput" ]
            [ div [ onKeyDown KeyDown ]
                [ input [ class "form_input", placeholder "Quantity", value model.tempQuant, onInput ChangeTempQuant ] []
                , input [ class "form_input", placeholder "Units (optional)", value model.tempUnit, onInput ChangeTempUnit ] []
                , input [ class "form_input", placeholder "Ingredient", value model.tempFood, onInput ChangeTempFood ] []
                ]
            ]
        , div [ class "slider" ]
            [ div [] [ text (String.fromFloat model.sliderVal) ]
            , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "10", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
            ]
        , div [ class "grid-divider" ]
            [ div [ class "recipeContainer" ]
                ([ h5 [] [ text "recipe" ]
                ]
                    ++ viewIngredientsLeft model.ingredients
                )
            , div [ class "recipeContainerRight" ]
                ([ h6 [] [ text "scaled" ]
                 ]
                    ++ viewIngredientsRight model.newIngredients
                )
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


viewIngredientsLeft : List Ingredient -> List (Html Msg)
viewIngredientsLeft lst =
    case lst of
        [] ->
            []

        food :: foods ->
            if String.isEmpty food.unit then
                [ div []
                    [ button [onClick (Delete food)] [text "X"]
                    , h3 [] [ text (food.quantity ++ " " ++ food.food) ]
                    ]
                ]
                    ++ viewIngredientsLeft foods

            else
                [ div []
                    [ button [onClick (Delete food)] [text "X"]
                    ,  h3 [] [ text (food.quantity ++ " " ++ food.unit ++ " of " ++ food.food) ]
                    ]
                ]
                    ++ viewIngredientsLeft foods


viewIngredientsRight : List Ingredient -> List (Html Msg)
viewIngredientsRight lst =
    case lst of
        [] ->
            []

        food :: foods ->
            if String.isEmpty food.unit then
                [ div []
                    [ h4 [] [ text (food.quantity ++ " " ++ food.food) ]
                    ]
                ]
                    ++ viewIngredientsRight foods

            else
                [ div []
                    [ h4 [] [ text (food.quantity ++ " " ++ food.unit ++ " of " ++ food.food) ]
                    ]
                ]
                    ++ viewIngredientsRight foods
