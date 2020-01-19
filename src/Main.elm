module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, br, button, div, h1, img, input, li, text, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import String



---- MODEL ----
-- pc stands for player character


type alias Model =
    { pcOfferString : String
    , pcOfferInt : Int
    , pcGold : Int
    , itemWorth : Int
    , conversation : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { pcOfferString = "0", pcOfferInt = 0, pcGold = 0, itemWorth = 20, conversation = [] }, Cmd.none )



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
            ( { model | pcOfferString = newOffer, pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }, Cmd.none )

        ModifyPcOffer amount ->
            ( { model | pcOfferString = String.fromInt (max 0 (model.pcOfferInt + amount)), pcOfferInt = max 0 (model.pcOfferInt + amount) }, Cmd.none )

        SubmitOffer ->
            ( sellItem <| updateConvoWithOffer <| model, Cmd.none )


updateConvoWithOffer : Model -> Model
updateConvoWithOffer model =
    { model | conversation = model.conversation ++ [ "You offered: " ++ String.fromInt model.pcOfferInt ] }


sellItem : Model -> Model
sellItem model =
    { model | pcGold = model.pcGold + model.pcOfferInt }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is still working!" ]
        , text ("Your gold: " ++ String.fromInt model.pcGold)
        , br [] []
        , div []
            [ button [ onClick (ModifyPcOffer -100) ] [ text "-100" ]
            , button [ onClick (ModifyPcOffer -10) ] [ text "-10" ]
            , input [ Attr.type_ "number", Attr.min "0", Attr.max "50000", placeholder "Your Offer", value model.pcOfferString, onInput PcOffer ] []
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
