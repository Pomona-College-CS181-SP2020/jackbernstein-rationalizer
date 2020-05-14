module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Parser exposing ((|.), (|=), Parser, float, int, spaces, succeed, symbol)
import Round exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias NewIngredient =
    { food : String
    , quantity : String
    , quantityPresent : Bool
    }


type alias Recipe =
    {  sliderVal : Float
    , total : String
    , listNewIngredients : List NewIngredient
    , changedNewIngs : List NewIngredient
    , noQuantFound : Bool
    , inputError : Bool
    }


type alias Model =
    Recipe


init : Model
init =
    { sliderVal = 1.0
    , total = ""
    , listNewIngredients = []
    , changedNewIngs = []
    , noQuantFound = False
    , inputError = False
    }



-- UPDATE


type Msg
    = Scale Float
    | KeyDown Int
    | UpdateSlider String
    | ChangeTotal String
    | AddNewIngredient
    | Delete NewIngredient
    | ScaleIngredient NewIngredient String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScaleIngredient food strng ->
            case String.toFloat strng of
                Nothing ->
                    if String.isEmpty strng then
                        { model | changedNewIngs = replaceFood model.changedNewIngs food strng }

                    else
                        { model | changedNewIngs = replaceFood model.changedNewIngs food strng, inputError = True }

                Just x ->
                    let
                        originalQuant =
                            findFoodQuant model.listNewIngredients food
                    in
                    case originalQuant of
                        Nothing ->
                            { model | changedNewIngs = replaceFood model.changedNewIngs food strng }

                        Just y ->
                            let
                                scalr =
                                    x / y
                            in
                            { model | changedNewIngs = replaceFood (scaleIngs model.listNewIngredients scalr food) food strng, inputError = False }

        AddNewIngredient ->
            case numberPresentHelper (String.words model.total) of
                Just x ->
                    let
                        lstOfRest =
                            removeNumber (String.words model.total)

                        restOfString =
                            String.join " " lstOfRest
                    in
                    { model
                        | listNewIngredients = model.listNewIngredients ++ [ { food = restOfString, quantity = String.fromFloat x, quantityPresent = True } ]
                        , changedNewIngs = model.changedNewIngs ++ [ { food = restOfString, quantity = String.fromFloat (x * model.sliderVal), quantityPresent = True } ]
                        , total = ""
                        , noQuantFound = False
                    }

                Nothing ->
                    if String.isEmpty model.total then
                        model

                    else
                        { model
                            | listNewIngredients = model.listNewIngredients ++ [ { food = model.total, quantity = "", quantityPresent = False } ]
                            , changedNewIngs = model.changedNewIngs ++ [ { food = model.total, quantity = "", quantityPresent = False } ]
                            , total = ""
                            , noQuantFound = True
                        }

        ChangeTotal tot ->
            { model | total = tot }

        UpdateSlider strng ->
            case String.toFloat strng of
                Just x ->
                    { model | sliderVal = x, changedNewIngs = scale model.listNewIngredients x, inputError = False }

                Nothing ->
                    model

        KeyDown key ->
            if key == 13 then
                update AddNewIngredient model

            else
                model

        Scale flt ->
            model

        Delete food ->
            { model | listNewIngredients = del model.listNewIngredients food, changedNewIngs = del model.changedNewIngs food }


scaleIngs : List NewIngredient -> Float -> NewIngredient -> List NewIngredient
scaleIngs lst flt fd =
    case lst of
        [] ->
            []

        ing :: ings ->
            if fd == ing then
                ing :: scaleIngs ings flt fd

            else
                case String.toFloat ing.quantity of
                    Nothing ->
                        ing :: scaleIngs ings flt fd

                    Just x ->
                        case Parser.run int (String.fromFloat (x * flt)) of
                            Ok y ->
                                { ing | quantity = String.fromInt y } :: scaleIngs ings flt fd

                            _ ->
                                { ing | quantity = Round.round 2 (x * flt) } :: scaleIngs ings flt fd


findFoodQuant : List NewIngredient -> NewIngredient -> Maybe Float
findFoodQuant lst match =
    case lst of
        [] ->
            Nothing

        ing :: ings ->
            if ing.food == match.food then
                String.toFloat ing.quantity

            else
                findFoodQuant ings match


replaceFood : List NewIngredient -> NewIngredient -> String -> List NewIngredient
replaceFood originalLst food newQuant =
    case originalLst of
        [] ->
            []

        ing :: ings ->
            if ing.food == food.food then
                { ing | quantity = newQuant } :: ings

            else
                ing :: replaceFood ings food newQuant


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
            case String.toFloat ing.quantity of
                Just x ->
                    case Parser.run int (String.fromFloat (x * flt)) of
                        Ok y -> 
                            { ing | quantity = String.fromInt y } :: scale ings flt
                        _ ->
                            { ing | quantity = Round.round 2 (x * flt)} :: scale ings flt

                Nothing ->
                    ing :: scale ings flt



-- VIEW


view : Model -> Html Msg
view model =
    if model.inputError then
        if model.noQuantFound then
            div []
                [ div [ class "ingredientInput2", onKeyDown KeyDown ]
                    [ div [ class "bigFont" ] [ text "Input an ingredient with a quantity, then hit enter" ]
                    , div [] [ input [ placeholder "3 teaspoons of sugar", value model.total, onInput ChangeTotal ] [] ]
                    , div [] [ text "Ingredient will not be scaled because no quantity was found" ]
                    ]
                , div [ class "slider" ]
                    [ div [] [ text (String.fromFloat model.sliderVal) ]
                    , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "20", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
                    , div [] [ text "Invalid quantity input" ]
                    ]
                , div []
                    [ div [ class "recipeContainer" ]
                        ([ h5 [] [ text "Original Recipe" ]
                         ]
                            ++ newViewIngredientsLeft model.listNewIngredients
                        )
                    , div [ class "recipeContainerRight" ]
                        ([ h6 [] [ text "Scaled Recipe" ]
                         ]
                            ++ newViewIngredientsRight model.changedNewIngs
                        )
                    ]
                ]

        else
            div []
                [ div [ class "ingredientInput", onKeyDown KeyDown ]
                    [ div [ class "bigFont" ] [ text "Input an ingredient with a quantity, then hit enter" ]
                    , input [ placeholder "3 teaspoons of sugar", value model.total, onInput ChangeTotal ] []
                    ]
                , div [ class "slider" ]
                    [ div [] [ text (String.fromFloat model.sliderVal) ]
                    , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "20", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
                    , div [] [ text "Invalid quantity input" ]
                    ]
                , div []
                    [ div [ class "recipeContainer" ]
                        ([ h5 [] [ text "Original Recipe" ]
                         ]
                            ++ newViewIngredientsLeft model.listNewIngredients
                        )
                    , div [ class "recipeContainerRight" ]
                        ([ h6 [] [ text "Scaled Recipe" ]
                         ]
                            ++ newViewIngredientsRight model.changedNewIngs
                        )
                    ]
                ]

    else if model.noQuantFound then
        div []
            [ div [ class "ingredientInput2", onKeyDown KeyDown ]
                [ div [ class "bigFont" ] [ text "Input an ingredient with a quantity, then hit enter" ]
                , div [] [ input [ placeholder "3 teaspoons of sugar", value model.total, onInput ChangeTotal ] [] ]
                , div [] [ text "Ingredient will not be scaled because no quantity was found" ]
                ]
            , div [ class "slider" ]
                [ div [] [ text (String.fromFloat model.sliderVal) ]
                , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "20", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
                ]
            , div []
                [ div [ class "recipeContainer" ]
                    ([ h5 [] [ text "Original Recipe" ]
                     ]
                        ++ newViewIngredientsLeft model.listNewIngredients
                    )
                , div [ class "recipeContainerRight" ]
                    ([ h6 [] [ text "Scaled Recipe" ]
                     ]
                        ++ newViewIngredientsRight model.changedNewIngs
                    )
                ]
            ]

    else
        div []
            [ div [ class "ingredientInput", onKeyDown KeyDown ]
                [ div [ class "bigFont" ] [ text "Input an ingredient with a quantity, then hit enter" ]
                , input [ placeholder "3 teaspoons of sugar", value model.total, onInput ChangeTotal ] []
                ]
            , div [ class "slider" ]
                [ div [] [ text (String.fromFloat model.sliderVal) ]
                , input [ type_ "range", Html.Attributes.min ".1", Html.Attributes.max "20", value (String.fromFloat model.sliderVal), step ".1", class "sliderConfig", onInput UpdateSlider ] []
                ]
            , div []
                [ div [ class "recipeContainer" ]
                    ([ h5 [] [ text "Original Recipe" ]
                     ]
                        ++ newViewIngredientsLeft model.listNewIngredients
                    )
                , div [ class "recipeContainerRight" ]
                    ([ h6 [] [ text "Scaled Recipe" ]
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
            case String.toFloat food.quantity of
                Just y ->
                    case Parser.run int (String.fromFloat y) of
                        Ok x ->
                            [ div []
                                [ button [ onClick (Delete food) ] [ text "X" ]
                                , h3 [] [ text (food.quantity ++ " " ++ food.food) ]
                                ]
                            ]
                                ++ newViewIngredientsLeft foods

                        _ ->
                            [ div []
                                [ button [ onClick (Delete food) ] [ text "X" ]
                                , h3 [] [ text (Round.round 2 y ++ " " ++ food.food) ]
                                ]
                            ]
                                ++ newViewIngredientsLeft foods

                Nothing ->
                    [ div []
                        [ button [ onClick (Delete food) ] [ text "X" ]
                        , h3 [] [ text food.food ]
                        ]
                    ]
                        ++ newViewIngredientsLeft foods


newViewIngredientsRight : List NewIngredient -> List (Html Msg)
newViewIngredientsRight lst =
    case lst of
        [] ->
            []

        food :: foods ->
            if food.quantityPresent then
                case Parser.run int food.quantity of
                    Ok x ->
                        [ div []
                            [ input [ class "indivIng", value food.quantity, onInput (ScaleIngredient food) ] []
                            , h4 [] [ text (" " ++ food.food) ]
                            ]
                        ]
                            ++ newViewIngredientsRight foods

                    _ ->
                        [ div []
                            [ input [ class "indivIng", value food.quantity, onInput (ScaleIngredient food) ] []
                            , h4 [] [ text (" " ++ food.food) ]
                            ]
                        ]
                            ++ newViewIngredientsRight foods

            else
                [ div []
                    [ h4 [] [ text food.food ]
                    ]
                ]
                    ++ newViewIngredientsRight foods
