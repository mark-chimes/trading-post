module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, li, text, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import String



---- MODEL ----
-- pc stands for player character


type alias Model =
    { pcOfferInt : Int
    , pcGold : Int
    , itemWorth : Int
    , customerMaxPrice : Int
    , conversation : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { pcOfferInt = 0, pcGold = 0, itemWorth = 20, customerMaxPrice = 50, conversation = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | PcOffer String
    | ModifyPcOffer Int
    | SubmitOffer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PcOffer newOffer ->
            ( makeOffer model newOffer, Cmd.none )

        ModifyPcOffer amount ->
            ( modifyOffer model amount, Cmd.none )

        SubmitOffer ->
            ( sellItem <| updateConvoWithOffer <| model, Cmd.none )


makeOffer : Model -> String -> Model
makeOffer model newOffer =
    { model | pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }


modifyOffer : Model -> Int -> Model
modifyOffer model amount =
    { model | pcOfferInt = max 0 (model.pcOfferInt + amount) }


updateConvoWithOffer : Model -> Model
updateConvoWithOffer model =
    { model
        | conversation =
            model.conversation
                ++ [ "You offered the sword for: " ++ String.fromInt model.pcOfferInt ++ "gp."
                   , "The customer bought 1 sword at " ++ String.fromInt model.pcOfferInt ++ "gp (cost price " ++ String.fromInt model.itemWorth ++ "gp)"
                   ]
    }


sellItem : Model -> Model
sellItem model =
    { model | pcGold = model.pcGold + model.pcOfferInt }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]
        , text ("Your gold: " ++ String.fromInt model.pcGold)
        , br [] []
        , div []
            [ button [ onClick (ModifyPcOffer -100) ] [ text "-100" ]
            , button [ onClick (ModifyPcOffer -10) ] [ text "-10" ]
            , input [ Attr.type_ "number", Attr.min "0", Attr.max "50000", placeholder "Your Offer", value (String.fromInt model.pcOfferInt), onInput PcOffer ] []
            , button [ onClick (ModifyPcOffer 10) ] [ text "+10" ]
            , button [ onClick (ModifyPcOffer 100) ] [ text "+100" ]
            ]
        , div [] [ text ("Your Offer: " ++ String.fromInt model.pcOfferInt) ]
        , button [ onClick SubmitOffer ] [ text "Submit Offer" ]
        , div [] []
        , br [] []
        , div [] [ text "The story thus far: " ]
        , ul [] (List.map (\x -> li [] [ x ]) (List.map text model.conversation))
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
