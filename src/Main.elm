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
    , offerInfo : OfferInfo
    , pcGold : Int
    , cleanTime : Int
    , customers : Clientele.ClienteleDetails
    , isConvoReverse : Bool
    , conversation : List (List String)
    }


type alias OfferInfo =
    { pcOfferInt : Int
    , itemWorth : Int
    }


type alias Time =
    { hour : Int
    , minute : Int
    }


init : ( Model, Cmd Msg )
init =
    ( updateConversationWithActionMessage
        (Clientele.customerCallMessage
            (Clientele.generateCustomer
                0
            )
        )
        { time = { hour = 8, minute = 0 }
        , offerInfo = { pcOfferInt = 0, itemWorth = 20 }
        , pcGold = 0
        , cleanTime = 10
        , customers = Clientele.initCustomers
        , isConvoReverse = False
        , conversation = []
        }
    , Cmd.none
    )



---- UPDATE ----


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
            ( updateOffer newOffer model, Cmd.none )

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


updateOffer : String -> Model -> Model
updateOffer newOfferString model =
    { model | offerInfo = calculateUpdatedOffer newOfferString model.offerInfo }


calculateUpdatedOffer : String -> OfferInfo -> OfferInfo
calculateUpdatedOffer newOfferString info =
    { info | pcOfferInt = max 0 (Maybe.withDefault info.pcOfferInt (String.toInt newOfferString)) }


modifyOffer : Model -> Int -> Model
modifyOffer model increaseAmount =
    { model | offerInfo = calculateModifiedOffer increaseAmount model.offerInfo }


calculateModifiedOffer : Int -> OfferInfo -> OfferInfo
calculateModifiedOffer increaseAmount offerInfo =
    { offerInfo | pcOfferInt = max 0 (offerInfo.pcOfferInt + increaseAmount) }



-- TODO modify the next function (including its types) to be more functional


submitOffer : Model -> Model
submitOffer model =
    case model.customers.currentCustomer of
        Just customer ->
            if model.offerInfo.pcOfferInt <= customer.maxPrice then
                succeedOnSale customer model

            else
                failOnSale customer model

        Nothing ->
            updateConversationWithActionMessage "There is no customer in store to whom to submit that offer." model


updateConversationWithActionMessage : String -> Model -> Model
updateConversationWithActionMessage message model =
    { model | conversation = model.conversation ++ [ [ displayTime model.time, message, "" ] ] }


succeedOnSale : Clientele.Customer -> Model -> Model
succeedOnSale customer model =
    kickOutCurrentCustomer <|
        updateConversationWithActionMessage (bar model.offerInfo.pcOfferInt model.offerInfo.itemWorth customer) <|
            updateGold <|
                updateTimeSuccess customer model


bar : Int -> Int -> Clientele.Customer -> String
bar offerInt itemWorth customer =
    offerString offerInt ++ "\n" ++ purchaseString offerInt itemWorth customer


failOnSale : Clientele.Customer -> Model -> Model
failOnSale customer model =
    updateConversationWithActionMessage (rejectString customer) <| updateTimeFailure customer <| model


fuckOffCustomer : Model -> Model
fuckOffCustomer model =
    updateConversationWithActionMessage (Clientele.customerFuckOffMessage model.customers) <| kickOutCurrentCustomer <| updateTimeFuckOff <| model


cleanStore : Model -> Model
cleanStore model =
    updateConversationWithActionMessage (cleanStoreMessage model.cleanTime) <| updateTimeCleanStore <| model


schmoozeCustomer : Model -> Model
schmoozeCustomer model =
    case model.customers.currentCustomer of
        Just customer ->
            (\mdl -> { mdl | customers = Clientele.schmoozeCurrentCustomer mdl.customers }) <| updateConversationWithActionMessage (Clientele.schmoozeCustomerMessage customer) <| updateTimeSchmooze customer <| model

        Nothing ->
            updateConversationWithActionMessage "Who are you trying to schmooze?" model


callNextCustomer : Clientele.Customer -> Model -> Model
callNextCustomer customer model =
    updateConversationWithActionMessage (Clientele.customerCallMessage customer) <|
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
    { model | pcGold = model.pcGold + model.offerInfo.pcOfferInt }


minutesInHour : Int
minutesInHour =
    60


hoursInDay : Int
hoursInDay =
    24


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


cleanStoreMessage : Int -> String
cleanStoreMessage cleaningTimeMin =
    "You clean the store for " ++ String.fromInt cleaningTimeMin ++ " minutes."


offerString : Int -> String
offerString pcOfferInt =
    "You offered the sword for: "
        ++ String.fromInt pcOfferInt
        ++ "gp."


purchaseString : Int -> Int -> Clientele.Customer -> String
purchaseString pcOfferInt itemWorth customer =
    "The customer, "
        ++ customer.name
        ++ ", bought 1 sword at "
        ++ String.fromInt pcOfferInt
        ++ "gp (cost price "
        ++ String.fromInt itemWorth
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
            , input [ Attr.type_ "number", Attr.min "0", Attr.max "50000", placeholder "Your Offer", value (String.fromInt model.offerInfo.pcOfferInt), onInput PcOffer ] []
            , button [ onClick (ModifyPcOffer 10) ] [ text "+10" ]
            , button [ onClick (ModifyPcOffer 100) ] [ text "+100" ]
            ]
        , div [] [ text ("Your Offer: " ++ String.fromInt model.offerInfo.pcOfferInt) ]
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
