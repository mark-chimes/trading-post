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
    , kickTime : Int
    , maxCustomers : Int
    , customerIndex : Int
    , customer : Customer
    , itemWorth : Int
    , conversation : List String
    }


type alias Customer =
    { name : String
    , maxPrice : Int
    , minTakenOnSuccess : Int
    , minTakenOnFail : Int
    }


type alias Time =
    { hour : Int
    , minute : Int
    }


init : ( Model, Cmd Msg )
init =
    ( updateConvoWithCustomerEntry
        { time = { hour = 8, minute = 0 }
        , pcOfferInt = 0
        , pcGold = 0
        , kickTime = 2
        , maxCustomers = 6
        , customerIndex = 0
        , customer = generateCustomer 0
        , itemWorth = 20
        , conversation = []
        }
    , Cmd.none
    )


generateCustomer : Int -> Customer
generateCustomer index =
    case index of
        0 ->
            { name = "Susan"
            , maxPrice = 50
            , minTakenOnSuccess = 5
            , minTakenOnFail = 10
            }

        1 ->
            { name = "Jeremy"
            , maxPrice = 60
            , minTakenOnSuccess = 5
            , minTakenOnFail = 15
            }

        2 ->
            { name = "Samantha"
            , maxPrice = 30
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            }

        3 ->
            { name = "Gertrude"
            , maxPrice = 80
            , minTakenOnSuccess = 5
            , minTakenOnFail = 20
            }

        4 ->
            { name = "Samson"
            , maxPrice = 25
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            }

        _ ->
            { name = "Pink"
            , maxPrice = 1200
            , minTakenOnSuccess = 5
            , minTakenOnFail = 60
            }



---- UPDATE ----


minutesInHour =
    60


hoursInDay =
    24


type Msg
    = NoOp
    | PcOffer String
    | ModifyPcOffer Int
    | SubmitOffer
    | ClearStory
    | KickOutCustomer


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

        KickOutCustomer ->
            ( kickOutCustomer model, Cmd.none )


makeOffer : Model -> String -> Model
makeOffer model newOffer =
    { model | pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }


modifyOffer : Model -> Int -> Model
modifyOffer model amount =
    { model | pcOfferInt = max 0 (model.pcOfferInt + amount) }



-- TODO modify the next function (including its types) to be more functional


submitOffer : Model -> Model
submitOffer model =
    if model.pcOfferInt <= model.customer.maxPrice then
        succeedOnSale model

    else
        failOnSale model


succeedOnSale : Model -> Model
succeedOnSale model =
    callNextCustomer <|
        updateConvoWithSuccessOffer <|
            updateGold <|
                updateTimeSuccess model


failOnSale : Model -> Model
failOnSale model =
    updateConvoWithFailureOffer <| updateTimeFailure model


kickOutCustomer : Model -> Model
kickOutCustomer model =
    callNextCustomer <| updateTimeKickout <| updateConvoWithCustomerKickOut <| model


callNextCustomer : Model -> Model
callNextCustomer model =
    updateConvoWithCustomerEntry <|
        incrementCustomer <|
            model


incrementCustomer : Model -> Model
incrementCustomer model =
    { model | customer = generateCustomer <| getNewCustomerIndex model, customerIndex = getNewCustomerIndex model }


getNewCustomerIndex : Model -> Int
getNewCustomerIndex model =
    remainderBy model.maxCustomers (model.customerIndex + 1)


updateTimeKickout : Model -> Model
updateTimeKickout model =
    { model | time = incrementTimeWithMin model.time model.kickTime }


updateConvoWithCustomerKickOut : Model -> Model
updateConvoWithCustomerKickOut model =
    { model
        | conversation =
            model.conversation
                ++ [ displayTime model.time
                   , "You tell "
                        ++ model.customer.name
                        ++ " to fuckk off. They leave in a huff taking "
                        ++ String.fromInt model.kickTime
                        ++ " minutes"
                   , ""
                   ]
    }


updateConvoWithCustomerEntry : Model -> Model
updateConvoWithCustomerEntry model =
    { model
        | conversation =
            model.conversation
                ++ [ displayTime model.time
                   , "A new customer called "
                        ++ model.customer.name
                        ++ " enters the store."
                   , ""
                   ]
    }


updateConvoWithSuccessOffer : Model -> Model
updateConvoWithSuccessOffer model =
    { model
        | conversation =
            model.conversation
                ++ [ displayTime model.time
                   , offerString model
                   , purchaseString model
                   , ""
                   ]
    }


updateConvoWithFailureOffer : Model -> Model
updateConvoWithFailureOffer model =
    { model
        | conversation =
            model.conversation
                ++ [ displayTime model.time
                   , offerString model
                   , rejectString model
                   , ""
                   ]
    }


updateTimeSuccess : Model -> Model
updateTimeSuccess model =
    { model | time = incrementTimeWithMin model.time model.customer.minTakenOnSuccess }


updateTimeFailure : Model -> Model
updateTimeFailure model =
    { model | time = incrementTimeWithMin model.time model.customer.minTakenOnFail }


updateGold : Model -> Model
updateGold model =
    { model | pcGold = model.pcGold + model.pcOfferInt }


minToTime : Int -> Time
minToTime mins =
    { hour = remainderBy hoursInDay (mins // minutesInHour), minute = remainderBy minutesInHour mins }


timeToMin : Time -> Int
timeToMin time =
    time.hour * minutesInHour + time.minute


incrementTimeWithMin : Time -> Int -> Time
incrementTimeWithMin time mins =
    minToTime <| mins + (timeToMin <| time)



-- Strings --


offerString : Model -> String
offerString model =
    "You offered the sword for: "
        ++ String.fromInt model.pcOfferInt
        ++ "gp."


purchaseString : Model -> String
purchaseString model =
    "The customer, "
        ++ model.customer.name
        ++ ", bought 1 sword at "
        ++ String.fromInt model.pcOfferInt
        ++ "gp (cost price "
        ++ String.fromInt model.itemWorth
        ++ "gp)"
        ++ ", taking "
        ++ String.fromInt model.customer.minTakenOnSuccess
        ++ " minutes."


rejectString : Model -> String
rejectString model =
    "The customer, "
        ++ model.customer.name
        ++ ", rejected the offer, taking "
        ++ String.fromInt model.customer.minTakenOnFail
        ++ " minutes."



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]

        --        , h2 [] [ text "Debug" ]
        --        , text
        --            ("Customer Max Price: " ++ String.fromInt model.customer.maxPrice)
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
        , button [ onClick KickOutCustomer ] [ text "Fuckk Off" ]
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
