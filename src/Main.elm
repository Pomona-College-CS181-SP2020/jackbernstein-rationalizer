module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)
import Round exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias NewIngredient =
    { food : String
    , quantity : Float
    }


type alias Recipe =
    { tempFood : String
    , tempQuant : String
    , tempUnit : String
    , optionFood : String
    , optionNumb : String
    , emptyName : Bool
    , invalidQuant : Bool
    , sliderVal : Float
    , total : String
    , listNewIngredients : List NewIngredient
    , changedNewIngs : List NewIngredient
    }


type alias Model =
    Recipe


init : Model
init =
    { tempFood = ""
    , tempQuant = ""
    , tempUnit = ""
    , optionFood = ""
    , optionNumb = ""
    , emptyName = False
    , invalidQuant = False
    , sliderVal = 1.0
    , total = ""
    , listNewIngredients = []
    , changedNewIngs = []
    }



-- UPDATE


type Msg
    = ChangeTempFood String
    | ChangeTempQuant String
    | ChangeTempUnit String
    | SetOption String
    | Scale Float
    | KeyDown Int
    | UpdateSlider String
    | ChangeTotal String
    | AddNewIngredient
    | Delete NewIngredient


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNewIngredient ->
            case numberPresentHelper (String.words model.total) of
                Just x ->
                    let
                        lstOfRest =
                            removeNumber (String.words model.total)

                        restOfString =
                            String.join " " lstOfRest
                    in
                    { model | listNewIngredients = model.listNewIngredients ++ [ { food = restOfString, quantity = x } ], changedNewIngs = model.changedNewIngs ++ [ { food = restOfString, quantity = x * model.sliderVal } ], total = "" }

                Nothing ->
                    model

        ChangeTotal tot ->
            { model | total = tot }

        UpdateSlider strng ->
            case String.toFloat strng of
                Just x ->
                    { model | sliderVal = x, changedNewIngs = scale model.listNewIngredients x }

                Nothing ->
                    model

        KeyDown key ->
            if key == 13 then
                update AddNewIngredient model

            else
                model

        SetOption strng ->
            if strng == "Select an Ingredient" then
                { model | optionFood = "", optionNumb = "" }

            else
                { model | optionFood = strng, optionNumb = "" }

        ChangeTempFood food ->
            { model | tempFood = food }

        ChangeTempQuant quant ->
            { model | tempQuant = quant }

        ChangeTempUnit unit ->
            { model | tempUnit = unit }

        Scale flt ->
            model

        Delete food ->
            { model | listNewIngredients = del model.listNewIngredients food, changedNewIngs = del model.changedNewIngs food }


removeNumber : List String -> List String
removeNumber lst =
    case lst of
        [] ->
            []

        fst :: rst ->
            case Parser.run float fst of
                Ok x ->
                    rst

                _ ->
                    fst :: removeNumber rst


numberPresentHelper : List String -> Maybe Float
numberPresentHelper lst =
    case lst of
        [] ->
            Nothing

        fst :: rst ->
            case Parser.run float fst of
                Ok x ->
                    Just x

                _ ->
                    numberPresentHelper rst


del : List NewIngredient -> NewIngredient -> List NewIngredient
del lst ing =
    case lst of
        [] ->
            []

        food :: foods ->
            if food.food == ing.food then
                foods

            else
                food :: del foods ing


scale : List NewIngredient -> Float -> List NewIngredient
scale lst flt =
    case lst of
        [] ->
            []

        ing :: ings ->
            { ing | quantity = ing.quantity * flt } :: scale ings flt



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "ingredientInput", onKeyDown KeyDown ]
            [ input [ placeholder "Input", value model.total, onInput ChangeTotal ] []
            ]
        , div [ class "slider" ]
            [ div [] [ text (String.fromFloat model.sliderVal) ]
            , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "10", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
            ]
        , div []
            [ div [ class "recipeContainer" ]
                ([ h5 [] [ text "recipe" ]
                 ]
                    ++ newViewIngredientsLeft model.listNewIngredients
                )
            , div [ class "recipeContainerRight" ]
                ([ h6 [] [ text "scaled" ]
                 ]
                    ++ newViewIngredientsRight model.changedNewIngs
                )
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


newViewIngredientsLeft : List NewIngredient -> List (Html Msg)
newViewIngredientsLeft lst =
    case lst of
        [] ->
            []

        food :: foods ->
            [ div []
                [ button [ onClick (Delete food) ] [ text "X" ]
                , h3 [] [ text (String.fromFloat food.quantity ++ " " ++ food.food) ]
                ]
            ]
                ++ newViewIngredientsLeft foods


newViewIngredientsRight : List NewIngredient -> List (Html Msg)
newViewIngredientsRight lst =
    case lst of
        [] ->
            []

        food :: foods ->
            [ div []
                [ h4 [] [ text (String.fromFloat food.quantity ++ " " ++ food.food) ]
                ]
            ]
                ++ newViewIngredientsRight foods
