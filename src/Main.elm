module Main exposing (Model, Msg(..), hoursInDay, init, main, update, view)

import Browser
import Clientele
import Html exposing (Html, br, button, div, h1, h2, h3, h4, img, input, li, text, textarea, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import String



---- MODEL ----
-- pc stands for player character


type alias Model =
    { time : Time
    , pcOfferInt : Int
    , pcGold : Int
    , customers : Clientele.Customers
    , itemWorth : Int
    , isConvoReverse : Bool
    , conversation : List (List String)
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
        , customers = Clientele.initCustomers
        , itemWorth = 20
        , isConvoReverse = False
        , conversation = []
        }
    , Cmd.none
    )



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
    | ReverseStory
    | SchmoozeCustomer
    | CustomerEntry Clientele.Customer


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

        ReverseStory ->
            ( { model | isConvoReverse = not model.isConvoReverse }, Cmd.none )

        SchmoozeCustomer ->
            ( schmoozeCustomer model, Cmd.none )

        CustomerEntry customer ->
            ( callNextCustomer model customer, Cmd.none )


makeOffer : Model -> String -> Model
makeOffer model newOffer =
    { model | pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }


modifyOffer : Model -> Int -> Model
modifyOffer model amount =
    { model | pcOfferInt = max 0 (model.pcOfferInt + amount) }



-- TODO modify the next function (including its types) to be more functional


submitOffer : Model -> Model
submitOffer model =
    if model.pcOfferInt <= model.customers.currentCustomer.maxPrice then
        succeedOnSale model

    else
        failOnSale model


succeedOnSale : Model -> Model
succeedOnSale model =
    updateConvoWithSuccessOffer <|
        updateGold <|
            updateTimeSuccess model


failOnSale : Model -> Model
failOnSale model =
    updateConvoWithFailureOffer <| updateTimeFailure model


kickOutCustomer : Model -> Model
kickOutCustomer model =
    updateTimeKickout <| updateConvoWithCustomerKickOut <| model


schmoozeCustomer : Model -> Model
schmoozeCustomer model =
    (\mdl -> { mdl | customers = Clientele.schmoozeCurrentCustomer mdl.customers }) <| updateTimeSchmooze <| updateConvoWithCustomerSchmooze model


callNextCustomer : Model -> Clientele.Customer -> Model
callNextCustomer model customer =
    updateConvoWithCustomerEntry <|
        { model | customers = Clientele.callCustomer model.customers customer }


updateTimeKickout : Model -> Model
updateTimeKickout model =
    { model | time = incrementTimeWithMin model.time model.customers.kickTime }


updateConvoWithAction : Model -> String -> Model
updateConvoWithAction model message =
    { model
        | conversation =
            model.conversation
                ++ [ [ displayTime model.time
                     , message
                     , ""
                     ]
                   ]
    }


updateConvoWithCustomerSchmooze : Model -> Model
updateConvoWithCustomerSchmooze model =
    updateConvoWithAction model (Clientele.schmoozeCustomerMessage model.customers.currentCustomer)


updateConvoWithCustomerKickOut : Model -> Model
updateConvoWithCustomerKickOut model =
    updateConvoWithAction model (Clientele.customerKickOutMessage model.customers)


updateConvoWithCustomerEntry : Model -> Model
updateConvoWithCustomerEntry model =
    updateConvoWithAction model (Clientele.customerEntryMessage model.customers)


updateConvoWithSuccessOffer : Model -> Model
updateConvoWithSuccessOffer model =
    updateConvoWithAction model (offerString model ++ "\n" ++ purchaseString model)


updateConvoWithFailureOffer : Model -> Model
updateConvoWithFailureOffer model =
    updateConvoWithAction model
        (offerString model
            ++ "\n"
            ++ rejectString model
        )


updateTimeSchmooze : Model -> Model
updateTimeSchmooze model =
    { model | time = incrementTimeWithMin model.time model.customers.currentCustomer.minTakenOnSchmooze }


updateTimeSuccess : Model -> Model
updateTimeSuccess model =
    { model | time = incrementTimeWithMin model.time model.customers.currentCustomer.minTakenOnSuccess }


updateTimeFailure : Model -> Model
updateTimeFailure model =
    { model | time = incrementTimeWithMin model.time model.customers.currentCustomer.minTakenOnFail }


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
        ++ model.customers.currentCustomer.name
        ++ ", bought 1 sword at "
        ++ String.fromInt model.pcOfferInt
        ++ "gp (cost price "
        ++ String.fromInt model.itemWorth
        ++ "gp)"
        ++ ", taking "
        ++ String.fromInt model.customers.currentCustomer.minTakenOnSuccess
        ++ " minutes."


rejectString : Model -> String
rejectString model =
    "The customer, "
        ++ model.customers.currentCustomer.name
        ++ ", rejected the offer, taking "
        ++ String.fromInt model.customers.currentCustomer.minTakenOnFail
        ++ " minutes."



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]

        --       , h2 [] [ text "Debug" ]
        --      , text
        --         ("Customer Max Price: " ++ String.fromInt model.customers.customer.maxPrice)
        , h2 [] [ text "Game" ]
        , h3 [] [ text ("Time: " ++ displayTime model.time) ]
        , h3 [] [ text "Customers" ]
        , div []
            (Clientele.customerEntryButtons
                (\c -> onClick (CustomerEntry c))
                model.customers
            )
        , h3 [] [ text "Actions" ]
        , div []
            [ button [ onClick SchmoozeCustomer ] [ text "Schmooze Customer" ]
            , button [ onClick KickOutCustomer ] [ text "Fuckk Off" ]
            ]
        , h4 [] [ text "Offer" ]
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
        , button [ onClick ReverseStory ] [ text "Reverse Story" ]
        , div []
            []
        , textarea [ Attr.id "convoText", Attr.cols 80, Attr.rows 20 ]
            ((if model.isConvoReverse then
                List.reverse

              else
                \l -> l
             )
                (List.map (\s -> text <| s ++ "\n") <| flattenStringListList model.conversation)
            )
        ]


flattenStringListList : List (List String) -> List String
flattenStringListList sll =
    List.map (\sl -> String.join "\n" sl) sll


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
