module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onInput)



---- MODEL ----
-- pc stands for player character


type alias Model =
    { pcOfferString : String
    , pcOfferInt : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { pcOfferString = "0", pcOfferInt = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | PcOffer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PcOffer newOffer ->
            ( { model | pcOfferString = newOffer, pcOfferInt = Maybe.withDefault model.pcOfferInt (String.toInt newOffer) }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App still working!" ]
        , input [ Attr.type_ "number", Attr.min "-50000", Attr.max "50000", placeholder "Your Offer", value model.pcOfferString, onInput PcOffer ] []
        , div [] [ text ("Your Offer: " ++ String.fromInt model.pcOfferInt) ]
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
