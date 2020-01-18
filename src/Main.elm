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
    , conversation : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { pcOfferString = "0", pcOfferInt = 0, conversation = [] }, Cmd.none )



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
            ( { model | pcOfferString = newOffer, pcOfferInt = Maybe.withDefault model.pcOfferInt (String.toInt newOffer) }, Cmd.none )

        ModifyPcOffer amount ->
            ( { model | pcOfferString = String.fromInt (model.pcOfferInt + amount), pcOfferInt = model.pcOfferInt + amount }, Cmd.none )

        SubmitOffer ->
            ( { model | conversation = model.conversation ++ [ "You offered: " ++ String.fromInt model.pcOfferInt ] }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App still working!" ]
        , div []
            [ button [ onClick (ModifyPcOffer -100) ] [ text "-100" ]
            , button [ onClick (ModifyPcOffer -10) ] [ text "-10" ]
            , input [ Attr.type_ "number", Attr.min "-50000", Attr.max "50000", placeholder "Your Offer", value model.pcOfferString, onInput PcOffer ] []
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
