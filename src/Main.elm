module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, br, button, div, h1, h2, h3, img, input, li, text, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import String



---- MODEL ----
-- pc stands for player character


type alias Model =
    { time : Time
    , pcOfferInt : Int
    , pcGold : Int
    , itemWorth : Int
    , customerMaxPrice : Int
    , customerName : String
    , conversation : List String
    }


type alias Time =
    { hour : Int
    , minute : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { time = { hour = 8, minute = 0 }, pcOfferInt = 0, pcGold = 0, itemWorth = 20, customerMaxPrice = 50, customerName = "Susan", conversation = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | PcOffer String
    | ModifyPcOffer Int
    | SubmitOffer
    | ClearStory


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
            ( submitOffer <| model, Cmd.none )

        ClearStory ->
            ( { model | conversation = [] }, Cmd.none )


makeOffer : Model -> String -> Model
makeOffer model newOffer =
    { model | pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }


modifyOffer : Model -> Int -> Model
modifyOffer model amount =
    { model | pcOfferInt = max 0 (model.pcOfferInt + amount) }


submitOffer : Model -> Model
submitOffer model =
    if model.pcOfferInt < model.customerMaxPrice then
        updateConvoWithOffer1 <| updateGold model

    else
        updateConvoWithOffer2 model


updateConvoWithOffer1 : Model -> Model
updateConvoWithOffer1 model =
    { model
        | conversation =
            model.conversation
                ++ [ offerString model
                   , purchaseString model
                   , ""
                   ]
    }


updateConvoWithOffer2 : Model -> Model
updateConvoWithOffer2 model =
    { model
        | conversation =
            model.conversation
                ++ [ offerString model
                   , rejectString model
                   , ""
                   ]
    }


updateGold : Model -> Model
updateGold model =
    { model | pcGold = model.pcGold + model.pcOfferInt }



-- Strings --


offerString : Model -> String
offerString model =
    "You offered the sword for: " ++ String.fromInt model.pcOfferInt ++ "gp."


purchaseString : Model -> String
purchaseString model =
    "The customer, " ++ model.customerName ++ ", bought 1 sword at " ++ String.fromInt model.pcOfferInt ++ "gp (cost price " ++ String.fromInt model.itemWorth ++ "gp)"


rejectString : Model -> String
rejectString model =
    "The customer, " ++ model.customerName ++ ", rejected the offer."



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]
        , h2 [] [ text "Debug" ]
        , text
            ("Customer Max Price: " ++ String.fromInt model.customerMaxPrice)
        , h2 [] [ text "Game" ]
        , div [] [ text ("Time: " ++ displayTime model.time) ]
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
        , h3 [] [ text "The story thus far: " ]
        , button [ onClick ClearStory ] [ text "Clear Story" ]
        , ul [] (List.map (\x -> li [] [ x ]) (List.map text model.conversation))
        ]


displayTime : Time -> String
displayTime time =
    (if time.hour < 10 then
        "0"

     else
        ""
    )
        ++ String.fromInt time.hour
        ++ ":"
        ++ (if time.minute < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt time.minute



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
