module Main exposing (Model, Msg(..), hoursInDay, init, main, purchaseString, update, view)

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
    , cleanTime : Int
    , customers : Clientele.ClienteleDetails
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
        (Clientele.generateCustomer
            0
        )
        { time = { hour = 8, minute = 0 }
        , pcOfferInt = 0
        , pcGold = 0
        , cleanTime = 10
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
    | CleanStore
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
            ( fuckOffCustomer model, Cmd.none )

        CleanStore ->
            ( cleanStore model, Cmd.none )

        ReverseStory ->
            ( { model | isConvoReverse = not model.isConvoReverse }, Cmd.none )

        SchmoozeCustomer ->
            ( schmoozeCustomer model, Cmd.none )

        CustomerEntry customer ->
            ( callNextCustomer customer model, Cmd.none )


makeOffer : Model -> String -> Model
makeOffer model newOffer =
    { model | pcOfferInt = max 0 (Maybe.withDefault model.pcOfferInt (String.toInt newOffer)) }


modifyOffer : Model -> Int -> Model
modifyOffer model amount =
    { model | pcOfferInt = max 0 (model.pcOfferInt + amount) }



-- TODO modify the next function (including its types) to be more functional


submitOffer : Model -> Model
submitOffer model =
    case model.customers.currentCustomer of
        Just customer ->
            if model.pcOfferInt <= customer.maxPrice then
                succeedOnSale customer model

            else
                failOnSale customer model

        Nothing ->
            updateConvoWithAction model "There is no customer in store to whom to submit that offer."


succeedOnSale : Clientele.Customer -> Model -> Model
succeedOnSale customer model =
    kickOutCurrentCustomer <|
        updateConvoWithSuccessOffer customer <|
            updateGold <|
                updateTimeSuccess customer model


failOnSale : Clientele.Customer -> Model -> Model
failOnSale customer model =
    updateConvoWithFailureOffer customer <| updateTimeFailure customer <| model


fuckOffCustomer : Model -> Model
fuckOffCustomer model =
    kickOutCurrentCustomer <| updateConvoWithCustomerFuckOff <| updateTimeFuckOff <| model


cleanStore : Model -> Model
cleanStore model =
    updateConvoWithCleanStore <| updateTimeCleanStore <| model


schmoozeCustomer : Model -> Model
schmoozeCustomer model =
    case model.customers.currentCustomer of
        Just customer ->
            (\mdl -> { mdl | customers = Clientele.schmoozeCurrentCustomer mdl.customers }) <| updateConvoWithCustomerSchmooze customer <| updateTimeSchmooze customer <| model

        Nothing ->
            updateConvoWithAction model "Who are you trying to schmooze?"


callNextCustomer : Clientele.Customer -> Model -> Model
callNextCustomer customer model =
    updateConvoWithCustomerEntry customer <|
        { model | customers = Clientele.callCustomer model.customers customer }


updateTimeFuckOff : Model -> Model
updateTimeFuckOff model =
    case model.customers.currentCustomer of
        Just _ ->
            { model | time = incrementTimeWithMin model.time model.customers.kickTime }

        Nothing ->
            model


updateTimeCleanStore : Model -> Model
updateTimeCleanStore model =
    { model | time = incrementTimeWithMin model.time model.cleanTime }


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


updateConvoWithCustomerSchmooze : Clientele.Customer -> Model -> Model
updateConvoWithCustomerSchmooze customer model =
    updateConvoWithAction model (Clientele.schmoozeCustomerMessage customer)


updateConvoWithCustomerFuckOff : Model -> Model
updateConvoWithCustomerFuckOff model =
    updateConvoWithAction model (Clientele.customerFuckOffMessage model.customers)


updateConvoWithCleanStore : Model -> Model
updateConvoWithCleanStore model =
    updateConvoWithAction model (cleanStoreMessage model)


updateConvoWithCustomerEntry : Clientele.Customer -> Model -> Model
updateConvoWithCustomerEntry customer model =
    updateConvoWithAction model (Clientele.customerCallMessage customer)


updateConvoWithSuccessOffer : Clientele.Customer -> Model -> Model
updateConvoWithSuccessOffer customer model =
    updateConvoWithAction model (offerString model ++ "\n" ++ purchaseString model customer)


updateConvoWithFailureOffer : Clientele.Customer -> Model -> Model
updateConvoWithFailureOffer customer model =
    updateConvoWithAction model
        (offerString model
            ++ "\n"
            ++ rejectString customer
        )


kickOutCurrentCustomer : Model -> Model
kickOutCurrentCustomer model =
    { model | customers = Clientele.kickOutCurrentCustomer model.customers }


updateTimeSchmooze : Clientele.Customer -> Model -> Model
updateTimeSchmooze customer model =
    { model | time = incrementTimeWithMin model.time customer.minTakenOnSchmooze }


updateTimeSuccess : Clientele.Customer -> Model -> Model
updateTimeSuccess customer model =
    { model | time = incrementTimeWithMin model.time customer.minTakenOnSuccess }


updateTimeFailure : Clientele.Customer -> Model -> Model
updateTimeFailure customer model =
    { model | time = incrementTimeWithMin model.time customer.minTakenOnFail }


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


cleanStoreMessage : Model -> String
cleanStoreMessage model =
    "You clean the store for " ++ String.fromInt model.cleanTime ++ " minutes."


offerString : Model -> String
offerString model =
    "You offered the sword for: "
        ++ String.fromInt model.pcOfferInt
        ++ "gp."


purchaseString : Model -> Clientele.Customer -> String
purchaseString model customer =
    "The customer, "
        ++ customer.name
        ++ ", bought 1 sword at "
        ++ String.fromInt model.pcOfferInt
        ++ "gp (cost price "
        ++ String.fromInt model.itemWorth
        ++ "gp)"
        ++ ", and leaves happy, taking "
        ++ String.fromInt customer.minTakenOnSuccess
        ++ " minutes."


rejectString : Clientele.Customer -> String
rejectString customer =
    "The customer, "
        ++ customer.name
        ++ ", rejected the offer, taking "
        ++ String.fromInt customer.minTakenOnFail
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
        , div []
            [ text
                ("You are speaking to: "
                    ++ (case model.customers.currentCustomer of
                            Just customer ->
                                customer.name

                            Nothing ->
                                "No-one"
                       )
                    ++ "."
                )
            ]
        , h3 [] [ text "Actions" ]
        , div []
            [ button [ onClick SchmoozeCustomer ] [ text "Schmooze Customer" ]
            , button [ onClick KickOutCustomer ] [ text "Fuckk Off" ]
            , button [ onClick CleanStore ] [ text "Clean Store" ]
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
