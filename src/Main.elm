module Main exposing (Model, Msg(..), calculateTimeOfNextCustomer, dayOfYear, failOnSaleNoMoney, hourOfDay, hoursInDay, incrementTimeWithMinOpen, init, main, purchaseString, totalSaleValueOfBasket, update, view)

import Browser
import Clientele
import Html exposing (Html, br, button, div, h1, h2, h3, h4, hr, img, input, li, text, textarea, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Stock exposing (..)
import String



---- MODEL ----
-- pc stands for player character


type alias Flags =
    { windowWidth : Int
    }


type alias Model =
    { time : Time
    , offerInfo : OfferInfo
    , pcGold : Int
    , cleanTime : Int
    , customers : Clientele.ClienteleDetails
    , isConvoReverse : Bool
    , conversation : List (List String)
    , prepState : PrepState
    , windowWidth : Int
    , waitTime : Int
    , timeOfNextCustomer : Time
    , storeState : StoreState
    , statsTracker : StatsTracker
    }


type alias StatsTracker =
    { profit : Int
    , turnover : Int
    , itemsOffered : Int
    , itemsSold : Int
    , customersSold : Int
    , customersKicked : Int
    , customersLeft : Int
    }


type alias Time =
    Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( updateConversationWithActionMessage
        (Clientele.customerCallMessage
            Clientele.initFirstCustomer
        )
        { time = openHour * minutesInHour
        , offerInfo = { pcOffer = 20, item = swordItem }
        , pcGold = 0
        , cleanTime = 10
        , customers = Clientele.initCustomers
        , isConvoReverse = True
        , conversation = []
        , prepState = Sale
        , windowWidth = flags.windowWidth
        , waitTime = 0
        , timeOfNextCustomer = (openHour * 60) + timeBetweenCustomersMins
        , storeState = Open
        , statsTracker = initStatsTracker
        }
    , Cmd.none
    )


initStatsTracker : StatsTracker
initStatsTracker =
    { profit = 0
    , turnover = 0
    , itemsOffered = 0
    , itemsSold = 0
    , customersSold = 0
    , customersKicked = 0
    , customersLeft = 0
    }


type Msg
    = NoOp
    | PrepSubmitOffer
    | PcOffer String
    | SubmitAddToBasket
    | SubmitConfirmOffer
    | ClearStory
    | PrepKickOutCustomer
    | KickOutCustomer
    | PrepCleanStore
    | CleanStore
    | ReverseStory
    | PrepSchmoozeCustomer
    | SchmoozeCustomer
    | CustomerEntry Clientele.Customer
    | ResetPrice
    | UpdateWaitTime String
    | PrepWaitAwhile
    | WaitAwhile
    | PrepInspectCustomer
    | InspectCustomer
    | PrepOpenStore
    | OpenStore
    | OfferItem Item


type StoreState
    = Open
    | Closed


type PrepState
    = Clean
    | Kick
    | Schmooze
    | Inspect
    | Wait
    | Sale
    | PrepOpen



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PrepWaitAwhile ->
            ( prepWaitAwhile model, Cmd.none )

        PrepSubmitOffer ->
            ( prepSubmitOffer model, Cmd.none )

        PcOffer newOffer ->
            ( updateOffer newOffer model, Cmd.none )

        SubmitAddToBasket ->
            ( submitAddToBasket <| model, Cmd.none )

        SubmitConfirmOffer ->
            ( submitConfirmOffer <| model, Cmd.none )

        ClearStory ->
            ( { model | conversation = [] }, Cmd.none )

        PrepKickOutCustomer ->
            ( prepFuckOffCustomer model, Cmd.none )

        KickOutCustomer ->
            ( fuckOffCustomer model, Cmd.none )

        PrepCleanStore ->
            ( prepCleanStore model, Cmd.none )

        CleanStore ->
            ( cleanStore model, Cmd.none )

        ReverseStory ->
            ( { model | isConvoReverse = not model.isConvoReverse }, Cmd.none )

        PrepInspectCustomer ->
            ( prepInspectCustomer model, Cmd.none )

        InspectCustomer ->
            ( inspectCustomer model, Cmd.none )

        PrepSchmoozeCustomer ->
            ( prepSchmoozeCustomer model, Cmd.none )

        SchmoozeCustomer ->
            ( schmoozeCustomer model, Cmd.none )

        CustomerEntry customer ->
            ( callNextCustomer customer model, Cmd.none )

        ResetPrice ->
            ( updatePriceReset model, Cmd.none )

        UpdateWaitTime waitTimeStr ->
            ( updateWaitTime waitTimeStr model, Cmd.none )

        WaitAwhile ->
            ( waitAwhile model, Cmd.none )

        PrepOpenStore ->
            ( prepOpenStore model, Cmd.none )

        OpenStore ->
            ( openStore model, Cmd.none )

        OfferItem item ->
            ( offerItem item model, Cmd.none )


updateOffer : String -> Model -> Model
updateOffer newOfferString model =
    { model | offerInfo = calculateUpdatedOffer newOfferString model.offerInfo }


updateWaitTime : String -> Model -> Model
updateWaitTime newWaitString model =
    { model | waitTime = max 0 (Maybe.withDefault model.waitTime (String.toInt newWaitString)) }


calculateUpdatedOffer : String -> OfferInfo -> OfferInfo
calculateUpdatedOffer newOfferString info =
    { info | pcOffer = max 0 (Maybe.withDefault info.pcOffer (String.toInt newOfferString)) }


updatePriceReset : Model -> Model
updatePriceReset model =
    { model | offerInfo = resetPrice model.offerInfo }


resetPrice : OfferInfo -> OfferInfo
resetPrice offerInfo =
    { offerInfo
        | pcOffer = offerInfo.item.itemWorth
    }


prepSubmitOffer : Model -> Model
prepSubmitOffer model =
    { model | prepState = Sale }


prepWaitAwhile : Model -> Model
prepWaitAwhile model =
    { model | prepState = Wait }


prepOpenStore : Model -> Model
prepOpenStore model =
    { model | prepState = PrepOpen }


prepFuckOffCustomer : Model -> Model
prepFuckOffCustomer model =
    { model | prepState = Kick }


prepSchmoozeCustomer : Model -> Model
prepSchmoozeCustomer model =
    { model | prepState = Schmooze }


prepInspectCustomer : Model -> Model
prepInspectCustomer model =
    { model | prepState = Inspect }


prepCleanStore : Model -> Model
prepCleanStore model =
    { model | prepState = Clean }


offerItem : Item -> Model -> Model
offerItem item model =
    { model | offerInfo = itemToOffer item model.offerInfo }


itemToOffer : Item -> OfferInfo -> OfferInfo
itemToOffer item offer =
    { offer | item = item }



-- TODO modify the next function (including its types) to be more functional


submitAddToBasket : Model -> Model
submitAddToBasket model =
    case model.customers.currentCustomer of
        Just customer ->
            case determineIfSale customer model of
                Success ->
                    addToBasket customer model

                BadDeal ->
                    failOnSaleBadDeal customer model.offerInfo model

                NoMoney ->
                    failOnSaleNoMoney customer model.offerInfo model

        Nothing ->
            updateConversationWithActionMessage "Please address a customer before submitting an offer." model


submitConfirmOffer : Model -> Model
submitConfirmOffer model =
    case model.customers.currentCustomer of
        Just customer ->
            confirmOffer customer model

        Nothing ->
            updateConversationWithActionMessage "Please address a customer before confirming an offer." model


type CustomerSaleSuccess
    = Success
    | BadDeal
    | NoMoney


determineIfSale : Clientele.Customer -> Model -> CustomerSaleSuccess
determineIfSale customer model =
    let
        offer =
            model.offerInfo.pcOffer
    in
    if model.offerInfo.pcOffer > customer.moneyInPurse then
        NoMoney

    else if offer > Clientele.maxPrice model.offerInfo.item.itemWorth customer then
        BadDeal

    else
        Success


updateConversationWithActionMessage : String -> Model -> Model
updateConversationWithActionMessage message model =
    { model | conversation = model.conversation ++ [ [ displayTime model.time, message, "" ] ] }


addToBasket : Clientele.Customer -> Model -> Model
addToBasket customer model =
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnSuccess model.time then
        closeStore "The customer looked just about ready to buy the item! But unfortunately, what with the curfew and all, you have to tell them to come back tomorrow." model

    else
        updateModelStatsTrackerWithOffer <|
            updateBasket <|
                updateCustomerGold <|
                    updateConversationWithActionMessage (offerAndPurchaseString customer model.offerInfo) <|
                        incrementTimeWithMinOpen Clientele.constants.minTakenOnSuccess <|
                            model


confirmOffer : Clientele.Customer -> Model -> Model
confirmOffer customer model =
    updateModelStatsTrackerWithConfirmSale customer <|
        exitCurrentCustomer <|
            updateGold customer <|
                updateConversationWithActionMessage (confirmSaleString customer) <|
                    model


updateBasket : Model -> Model
updateBasket model =
    { model | customers = Clientele.updateCurrentCustomerBasket model.offerInfo model.customers }


updateCustomerGold : Model -> Model
updateCustomerGold model =
    { model | customers = Clientele.updateCurrentCustomerGold model.offerInfo.pcOffer model.customers }


updateModelStatsTrackerWithConfirmSale : Clientele.Customer -> Model -> Model
updateModelStatsTrackerWithConfirmSale customer model =
    { model | statsTracker = updateStatsTrackerWithConfirmSale customer model.statsTracker }


updateStatsTrackerWithConfirmSale : Clientele.Customer -> StatsTracker -> StatsTracker
updateStatsTrackerWithConfirmSale customer stats =
    { stats
        | customersSold = stats.customersSold + 1
        , itemsSold = stats.itemsSold + List.length customer.basket
        , turnover = stats.turnover + totalSaleValueOfBasket customer
        , profit = stats.profit + totalSaleValueOfBasket customer - totalCostPriceOfBasket customer
    }


updateModelStatsTrackerWithOffer : Model -> Model
updateModelStatsTrackerWithOffer model =
    { model | statsTracker = updateStatsTrackerWithOffer model.statsTracker }


updateStatsTrackerWithOffer : StatsTracker -> StatsTracker
updateStatsTrackerWithOffer statsTracker =
    { statsTracker | itemsOffered = statsTracker.itemsOffered + 1 }


failOnSaleNoMoney : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSaleNoMoney customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateModelStatsTrackerWithOffer <|
            updateConversationWithActionMessage (rejectStringNoMoney customer offer) <|
                incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <|
                    model


failOnSaleBadDeal : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSaleBadDeal customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateModelStatsTrackerWithOffer <|
            updateConversationWithActionMessage (rejectStringBadDeal customer offer) <|
                incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <|
                    model


fuckOffCustomer : Model -> Model
fuckOffCustomer model =
    updateModelStatsTrackerWithFuckoff <| updateConversationWithActionMessage (Clientele.customerFuckOffMessage model.customers) <| exitCurrentCustomer <| updateTimeFuckOff <| model


updateModelStatsTrackerWithFuckoff : Model -> Model
updateModelStatsTrackerWithFuckoff model =
    { model | statsTracker = updateStatsTrackerWithKickout model.statsTracker }


updateStatsTrackerWithKickout : StatsTracker -> StatsTracker
updateStatsTrackerWithKickout statsTracker =
    { statsTracker | customersKicked = statsTracker.customersKicked + 1 }


cleanStore : Model -> Model
cleanStore model =
    if (model.storeState == Open) && wouldStoreClose model.cleanTime model.time then
        closeStore "The store closes whilst you are busy cleanings." model

    else
        updateConversationWithActionMessage (cleanStoreMessage model.cleanTime) <| incrementTimeWithMinOpen model.cleanTime <| model


waitAwhile : Model -> Model
waitAwhile model =
    if (model.storeState == Open) && wouldStoreClose model.waitTime model.time then
        closeStore "Whilst you are busy sitting on your ass, you lose track of the time, and next thing you know, the store is closed and the customers have left." model

    else
        updateConversationWithActionMessage (waitAwhileMessage model.waitTime) <| incrementTimeWithMinOpen model.waitTime <| model


schmoozeCustomer : Model -> Model
schmoozeCustomer model =
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnSchmooze model.time then
        closeStore "Whilst you are busy schmoozing the customer, the store closes, and you are forced to schmoo them out." model

    else
        case model.customers.currentCustomer of
            Just customer ->
                (\mdl -> { mdl | customers = Clientele.schmoozeCurrentCustomer mdl.customers }) <|
                    updateConversationWithActionMessage (Clientele.schmoozeCustomerMessage customer) <|
                        incrementTimeWithMinOpen Clientele.constants.minTakenOnSchmooze <|
                            model

            Nothing ->
                updateConversationWithActionMessage "Who are you trying to schmooze?" model


inspectCustomer : Model -> Model
inspectCustomer model =
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnInspect model.time then
        closeStore "Whilst you are busy staring at the customer, the store closes, and they leave before you can get a better look." model

    else
        case model.customers.currentCustomer of
            Just customer ->
                markCurrentCustomerAsInspected <|
                    updateConversationWithActionMessage (Clientele.inspectCustomerMessage customer) <|
                        incrementTimeWithMinOpen Clientele.constants.minTakenOnInspect <|
                            model

            Nothing ->
                updateConversationWithActionMessage "Who are you trying to inspeect?" model


markCurrentCustomerAsInspected : Model -> Model
markCurrentCustomerAsInspected model =
    { model | customers = Clientele.markCurrentCustomerAsInspected model.customers }


callNextCustomer : Clientele.Customer -> Model -> Model
callNextCustomer customer model =
    updateConversationWithActionMessage (Clientele.customerCallMessage customer) <|
        incrementTimeWithMinOpen Clientele.constants.minTakenOnSpeakingTo <|
            { model | customers = Clientele.callCustomer model.customers customer }


updateTimeFuckOff : Model -> Model
updateTimeFuckOff model =
    if (model.storeState == Open) && wouldStoreClose model.customers.kickTime model.time then
        closeStore "Whilst you tell them to fuck off, the store closes anyway" model

    else
        case model.customers.currentCustomer of
            Just _ ->
                incrementTimeWithMinOpen model.customers.kickTime model

            Nothing ->
                model


exitCurrentCustomer : Model -> Model
exitCurrentCustomer model =
    { model | customers = Clientele.exitCurrentCustomer model.customers }


updateGold : Clientele.Customer -> Model -> Model
updateGold customer model =
    { model | pcGold = model.pcGold + totalSaleValueOfBasket customer }


totalSaleValueOfBasket : Clientele.Customer -> Int
totalSaleValueOfBasket customer =
    List.foldl (+) 0 <| List.map (\o -> o.pcOffer) customer.basket


totalCostPriceOfBasket : Clientele.Customer -> Int
totalCostPriceOfBasket customer =
    List.foldl (+) 0 <| List.map (\o -> o.item.itemWorth) customer.basket


minutesInHour : Int
minutesInHour =
    60


hoursInDay : Int
hoursInDay =
    24


closeMinute : Int
closeMinute =
    closeHour * 60


minutesInDay : Int
minutesInDay =
    minutesInHour * hoursInDay


minuteOfDay : Time -> Int
minuteOfDay time =
    modBy minutesInDay time


calculateClosingTime : Time -> Time
calculateClosingTime time =
    dayOfYear time * minutesInDay + closeMinute


openStore : Model -> Model
openStore model =
    updateConversationWithActionMessage
        (Clientele.customerCallMessage
            Clientele.initFirstCustomer
        )
    <|
        (\mdl ->
            updateConversationWithActionMessage
                ("You open your doors on day "
                    ++ String.fromInt (dayOfYear mdl.time)
                    ++ " of many!"
                )
                mdl
        )
        <|
            incTimeAndOpenStore <|
                model


incTimeAndOpenStore : Model -> Model
incTimeAndOpenStore model =
    { model
        | storeState = Open
        , time = ((dayOfYear model.time + 1) * minutesInDay) + (openHour * minutesInHour)
        , timeOfNextCustomer = ((dayOfYear model.time + 1) * minutesInDay) + (openHour * 60) + timeBetweenCustomersMins
        , prepState = Sale
        , customers = Clientele.callCustomerFromPool model.customers
        , statsTracker = initStatsTracker
    }


closeStore : String -> Model -> Model
closeStore closeMessage model =
    (\mdl -> updateConversationWithActionMessage (statsModelMessage mdl) mdl) <|
        (\mdl ->
            { mdl
                | storeState = Closed
                , customers = Clientele.exitAllCustomers model.customers
            }
        )
        <|
            recordNeglectedCustomers <|
                updateConversationWithActionMessage closeMessage <|
                    incrementTimeToTimeWhilstOpen (calculateClosingTime model.time) model


recordNeglectedCustomers : Model -> Model
recordNeglectedCustomers model =
    { model
        | statsTracker =
            neglectedCustomers
                (List.length model.customers.waitingCustomers
                    + (case model.customers.currentCustomer of
                        Just _ ->
                            1

                        Nothing ->
                            0
                      )
                )
                model.statsTracker
    }


neglectedCustomers : Int -> StatsTracker -> StatsTracker
neglectedCustomers numCust statsTracker =
    { statsTracker | customersLeft = statsTracker.customersLeft + numCust }


statsModelMessage : Model -> String
statsModelMessage model =
    statsTrackMessage model.statsTracker


statsTrackMessage : StatsTracker -> String
statsTrackMessage stats =
    let
        turnover =
            stats.turnover

        profit =
            stats.profit

        offered =
            stats.itemsOffered

        itemsSold =
            stats.itemsSold

        customersSold =
            stats.customersSold

        kicked =
            stats.customersKicked

        left =
            stats.customersLeft
    in
    "Stats for the day: "
        ++ (if offered > 0 then
                "You made a total of "
                    ++ String.fromInt offered
                    ++ " offers to customers. "
                    ++ (if itemsSold > 0 then
                            "You sold a total of "
                                ++ String.fromInt itemsSold
                                ++ " items to "
                                ++ String.fromInt customersSold
                                ++ " customer(s). This netted you a total turnover of "
                                ++ String.fromInt turnover
                                ++ " gp, "
                                ++ (if profit > 0 then
                                        "and a profit of "
                                            ++ String.fromInt profit
                                            ++ " gp. "

                                    else if profit == 0 then
                                        "but you didn't make any profit! You should sell items for more than you bought them. "

                                    else
                                        "but you made a loss of " ++ String.fromInt profit ++ " gp! You should sell items for more than you bought them. "
                                   )

                        else
                            "You didn't sell items to anyone, which is a bit worrying considering that you're running a store. "
                       )

            else
                "You didn't so much as offer an item to anyone. That's no way to run a business! "
           )
        ++ (if kicked > 0 then
                "You told "
                    ++ String.fromInt stats.customersKicked
                    ++ " customer(s) to fuck right off. "

            else
                ""
           )
        ++ (if left > 0 then
                "Your negligence resulted in "
                    ++ String.fromInt stats.customersLeft
                    ++ " customer(s) leaving of their own accord."

            else
                "You managed to attend to every customer that entered your store, one way or another."
           )



-- TODO Should do this with minute of day rather than hour


wouldStoreClose : Int -> Time -> Bool
wouldStoreClose mins oldTime =
    let
        newTime =
            oldTime + mins
    in
    -- past closing time
    minuteOfDay newTime
        >= closeMinute
        -- new day
        || minuteOfDay newTime
        < minuteOfDay oldTime
        -- long wait
        || newTime
        - oldTime
        > minutesInDay


incrementTimeWithMinOpen : Int -> Model -> Model
incrementTimeWithMinOpen mins model =
    incrementTimeToTimeWhilstOpen (mins + model.time) model


incrementTimeToTimeWhilstOpen : Time -> Model -> Model
incrementTimeToTimeWhilstOpen newTime model =
    let
        ( lastTimeOfNextCustomer, newClienteleDetails ) =
            addCustomers newTime model.timeOfNextCustomer model.customers
    in
    { model
        | time = newTime
        , timeOfNextCustomer = lastTimeOfNextCustomer
        , customers = newClienteleDetails
    }


hourOfDay : Time -> Int
hourOfDay minutesSinceZero =
    modBy hoursInDay (minutesSinceZero // minutesInHour)


dayOfYear : Time -> Int
dayOfYear minutesSinceZero =
    minutesSinceZero // (minutesInHour * hoursInDay)


timeBetweenCustomersMins : Int
timeBetweenCustomersMins =
    30


addCustomers : Time -> Time -> Clientele.ClienteleDetails -> ( Time, Clientele.ClienteleDetails )
addCustomers newTime oldTimeOfNextCust customers =
    let
        ( newTimeOfNextCust, numRounds ) =
            calculateTimeOfNextCustomer newTime oldTimeOfNextCust
    in
    ( newTimeOfNextCust, loopClienteleNewWaitingCustomer customers numRounds )


loopClienteleNewWaitingCustomer : Clientele.ClienteleDetails -> Int -> Clientele.ClienteleDetails
loopClienteleNewWaitingCustomer customers roundNum =
    if roundNum > 0 then
        loopClienteleNewWaitingCustomer (Clientele.newWaitingCustomer customers) (roundNum - 1)

    else
        customers


calculateTimeOfNextCustomer : Time -> Time -> ( Time, Int )
calculateTimeOfNextCustomer newTime oldTimeOfNextCust =
    let
        rounds =
            if newTime >= oldTimeOfNextCust then
                ((newTime - oldTimeOfNextCust) // timeBetweenCustomersMins) + 1

            else
                0

        newTimeOfNextCust =
            oldTimeOfNextCust + rounds * timeBetweenCustomersMins
    in
    ( newTimeOfNextCust, rounds )


waitAwhileMessage : Int -> String
waitAwhileMessage waitingTimeMin =
    "You sit on your ass for " ++ String.fromInt waitingTimeMin ++ " minutes."



-- Strings --


cleanStoreMessage : Int -> String
cleanStoreMessage cleaningTimeMin =
    "You clean the store for " ++ String.fromInt cleaningTimeMin ++ " minutes."


offerAndPurchaseString : Clientele.Customer -> OfferInfo -> String
offerAndPurchaseString customer offerInfo =
    offerString offerInfo ++ "\n" ++ purchaseString customer offerInfo


confirmSaleString : Clientele.Customer -> String
confirmSaleString customer =
    "You sell "
        ++ String.fromInt (List.length customer.basket)
        ++ " item(s) to "
        ++ customer.name
        ++ " for a total of "
        ++ String.fromInt (totalSaleValueOfBasket customer)
        ++ " gp. Cost price was "
        ++ String.fromInt (totalCostPriceOfBasket customer)
        ++ " gp, netting you a profit of "
        ++ String.fromInt (totalSaleValueOfBasket customer - totalCostPriceOfBasket customer)
        ++ " gp.\n"
        ++ customer.name
        ++ ": \"Goodbye, and thanks for everything!\"\n"


offerString : OfferInfo -> String
offerString info =
    "You offered the "
        ++ info.item.itemName
        ++ " for a price of "
        ++ String.fromInt info.pcOffer
        ++ " gp. (Item cost: "
        ++ String.fromInt info.item.itemWorth
        ++ " gp)."


purchaseString : Clientele.Customer -> OfferInfo -> String
purchaseString customer offerInfo =
    customer.name
        ++ ": \"A "
        ++ offerInfo.item.itemName
        ++ " for "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gold sounds like a good deal!\" \nThey add the "
        ++ offerInfo.item.itemName
        ++ " to their basket.\n"
        ++ "This took: "
        ++ String.fromInt Clientele.constants.minTakenOnSuccess
        ++ " minutes."


rejectStringBadDeal : Clientele.Customer -> OfferInfo -> String
rejectStringBadDeal customer offer =
    offerString offer
        ++ "\n"
        ++ customer.name
        ++ ": \""
        ++ String.fromInt offer.pcOffer
        ++ " gold is too expensive for a "
        ++ offer.item.itemName
        ++ "!\". \nThis exchange took "
        ++ String.fromInt Clientele.constants.minTakenOnFail
        ++ " minutes."


rejectStringNoMoney : Clientele.Customer -> OfferInfo -> String
rejectStringNoMoney customer offer =
    offerString offer
        ++ "\n"
        ++ customer.name
        ++ ": \"I'm afraid I just don't have another "
        ++ String.fromInt offer.pcOffer
        ++ " gold for a "
        ++ offer.item.itemName
        ++ ".\" \nThis exchange took "
        ++ String.fromInt Clientele.constants.minTakenOnFail
        ++ " minutes."



---- Opening and Closing ----


openHour : Int
openHour =
    9


closeHour : Int
closeHour =
    17



---- VIEW ----


borderStyle : Html.Attribute msg
borderStyle =
    Attr.style "border" "2px solid black"


halfThick : String
halfThick =
    "5px"


fullThick : String
fullThick =
    "10px"


halfBlock : List (Html msg) -> Html msg
halfBlock theHtml =
    div
        [ Attr.style "float" "left"
        , Attr.style "width" "50%"
        ]
        [ div
            [ borderStyle
            , Attr.style "margin-top" halfThick
            , Attr.style "margin-bottom" "0px"
            , Attr.style "margin-left" halfThick
            , Attr.style "margin-right" halfThick
            , Attr.style "padding-top" "0px"
            , Attr.style "padding-bottom" fullThick
            , Attr.style "padding-left" "0px"
            , Attr.style "padding-right" "0px"
            ]
            theHtml
        ]


topBlock : List (Html msg) -> Html msg
topBlock theHtml =
    div
        [ Attr.style "width" "100%"
        ]
        [ div
            [ borderStyle
            , Attr.style "margin-bottom" halfThick
            , Attr.style "margin-left" fullThick
            , Attr.style "margin-right" fullThick
            , Attr.style "padding-bottom" fullThick
            ]
            theHtml
        ]


blockOfBlocks : List (Html msg) -> Html msg
blockOfBlocks theHtml =
    div
        [ Attr.style "width" "100%"
        , Attr.style "clear" "both"
        , Attr.style "display" "table"
        ]
        [ div
            [ Attr.style "margin-top" halfThick
            , Attr.style "margin-bottom" fullThick
            , Attr.style "margin-left" halfThick
            , Attr.style "margin-right" halfThick
            ]
            theHtml
        ]


oneBlock : List (Html msg) -> Html msg
oneBlock theHtml =
    div
        [ Attr.style "width" "100%"
        , Attr.style "clear" "both"
        , Attr.style "display" "table"
        ]
        [ div
            [ borderStyle
            , Attr.style "margin-top" fullThick
            , Attr.style "margin-bottom" halfThick
            , Attr.style "margin-left" fullThick
            , Attr.style "margin-right" fullThick
            , Attr.style "padding-bottom" fullThick
            ]
            theHtml
        ]



-- Main view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]
        , text "https://mark-chimes.github.io/trading-post/"
        , h2 []
            [ text "Debug" ]
        , text
            ("Time of next customer " ++ displayTime model.timeOfNextCustomer)
        , div [] []
        , text ("minutesSinceZero: " ++ String.fromInt model.time)
        , div [] []
        , div []
            [ text <|
                "Customer max price: "
                    ++ (case model.customers.currentCustomer of
                            Just customer ->
                                String.fromInt <| Clientele.maxPrice model.offerInfo.item.itemWorth customer

                            Nothing ->
                                "No Customer"
                       )
            ]
        , topBlock <| storeInfo model
        , uiBasedOnStoreState model.storeState model
        , oneBlock <| storyBlock model
        ]


uiBasedOnStoreState : StoreState -> Model -> Html Msg
uiBasedOnStoreState storeState model =
    case storeState of
        Open ->
            blockOfBlocks
                [ halfBlock <| stockBlock model
                , halfBlock <| actionsBlockOpen
                , halfBlock <| currentSituationBlockOpen model
                , halfBlock <| customersBlockOpen model
                , halfBlock <| customerInfoPanelOpen model
                ]

        Closed ->
            blockOfBlocks
                [ halfBlock <| stockBlock model
                , halfBlock <| actionsBlockClosed
                , halfBlock <| currentSituationBlockClosed model
                , halfBlock <| customersBlockClosed
                , halfBlock <| customerInfoPanelClosed
                ]


storeInfo : Model -> List (Html Msg)
storeInfo model =
    [ h2 [] [ text "Store" ]
    , div [] [ text ("Time: " ++ displayTime model.time) ]
    , div [] [ text ("Day: " ++ (String.fromInt <| dayOfYear model.time)) ]
    , div [] [ text ("Your gold: " ++ String.fromInt model.pcGold ++ " gp") ]
    ]


nooneMessage : String
nooneMessage =
    "Select a customer to address."


storeClosedMessage : String
storeClosedMessage =
    "The store is now closed."


currentSituationBlockOpen : Model -> List (Html Msg)
currentSituationBlockOpen model =
    [ h3 [] [ text "Current Action" ]
    , case model.prepState of
        Clean ->
            div []
                [ basicButton [ onClick CleanStore ] [ text "Clean Store" ]
                ]

        Kick ->
            case model.customers.currentCustomer of
                Nothing ->
                    div [] [ text nooneMessage ]

                Just customer ->
                    div []
                        [ basicButton [ onClick KickOutCustomer ] [ text <| "Kick out " ++ customer.name ]
                        ]

        Schmooze ->
            case model.customers.currentCustomer of
                Nothing ->
                    div [] [ text nooneMessage ]

                Just customer ->
                    div []
                        [ basicButton [ onClick SchmoozeCustomer ] [ text <| "Schmooze " ++ customer.name ]
                        ]

        Inspect ->
            case model.customers.currentCustomer of
                Nothing ->
                    div [] [ text nooneMessage ]

                Just customer ->
                    div []
                        [ basicButton [ onClick InspectCustomer ] [ text <| "Inspect " ++ customer.name ]
                        ]

        Sale ->
            case model.customers.currentCustomer of
                Nothing ->
                    div [] [ text nooneMessage ]

                Just customer ->
                    div [] [ priceBox customer model, br [] [], basketBox customer ]

        Wait ->
            div []
                [ basicButton [ Attr.attribute "aria-label" "Reset waiting time to 0", onClick <| UpdateWaitTime <| String.fromInt 0 ] [ text "Reset" ]
                , modifyWaitButton -60 model
                , modifyWaitButton -10 model
                , input
                    [ Attr.attribute "aria-label" "Time to wait"
                    , Attr.style "margin" "2px"
                    , Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.max "1440"
                    , value (String.fromInt model.waitTime)
                    , onInput UpdateWaitTime
                    ]
                    []
                , modifyWaitButton 10 model
                , modifyWaitButton 60 model
                , div [] []
                , basicButton
                    [ onClick WaitAwhile ]
                    [ text <| "Wait for " ++ String.fromInt model.waitTime ++ " minutes" ]
                ]

        PrepOpen ->
            div []
                [ text "The store must be closed for you to be able to open it."
                ]
    ]


priceBox : Clientele.Customer -> Model -> Html Msg
priceBox customer model =
    div []
        [ div [ Attr.style "margin-bottom" halfThick ]
            [ div []
                [ basicButton [ Attr.attribute "aria-label" "Reset offer to cost price", onClick ResetPrice ] [ text "Cost" ]
                , modifyOfferButton -100 model
                , modifyOfferButton -10 model
                , input
                    [ Attr.attribute "aria-label" "Price in gold"
                    , Attr.style "margin" "2px"
                    , Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.max "50000"
                    , value (String.fromInt model.offerInfo.pcOffer)
                    , onInput PcOffer
                    ]
                    []
                , modifyOfferButton 10 model
                , modifyOfferButton 100 model
                ]
            , br [] []
            , div [] []
            , basicButton [ onClick SubmitAddToBasket ]
                [ text <| currentSituationString customer model.offerInfo
                ]
            ]
        ]


basketBox : Clientele.Customer -> Html Msg
basketBox customer =
    div [] <|
        [ basicButton [ onClick SubmitConfirmOffer ] [ text <| "Confirm Sale of " ++ String.fromInt (List.length customer.basket) ++ " items and Say Goodbye" ] ]
            ++ (List.map
                    (\s -> div [] [ text s ])
                <|
                    [ "Items in basket:"
                    ]
                        ++ List.map itemDisplay customer.basket
               )


itemDisplay : OfferInfo -> String
itemDisplay offerInfo =
    offerInfo.item.itemName
        ++ " ("
        ++ String.fromInt offerInfo.item.itemWorth
        ++ " gp) "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gp"


currentSituationBlockClosed : Model -> List (Html Msg)
currentSituationBlockClosed model =
    [ h3 [] [ text "Current Action" ]
    , case model.prepState of
        Clean ->
            div [] [ text storeClosedMessage ]

        Kick ->
            div [] [ text storeClosedMessage ]

        Schmooze ->
            div [] [ text storeClosedMessage ]

        Inspect ->
            div [] [ text storeClosedMessage ]

        Sale ->
            div [] [ text storeClosedMessage ]

        Wait ->
            div [] [ text storeClosedMessage ]

        PrepOpen ->
            div []
                [ basicButton [ onClick OpenStore ] [ text <| "Skip until tomorrow and open store at " ++ String.fromInt openHour ++ " o Clock." ]
                ]
    ]


customerInfoPanelOpen : Model -> List (Html Msg)
customerInfoPanelOpen model =
    [ h3 [] [ text "Customer Info" ]
    , div []
        [ case model.customers.currentCustomer of
            Just customer ->
                div [] <| List.map (\s -> div [] [ text s ]) <| Clientele.customerDisplay customer

            Nothing ->
                div [] [ text "No customer" ]
        ]
    ]


customerInfoPanelClosed : List (Html Msg)
customerInfoPanelClosed =
    [ h3 [] [ text "Customer Info" ]
    , div []
        [ text storeClosedMessage ]
    ]


currentSituationString : Clientele.Customer -> OfferInfo -> String
currentSituationString customer offerInfo =
    "Offer "
        ++ offerInfo.item.itemName
        ++ " (cost "
        ++ String.fromInt offerInfo.item.itemWorth
        ++ " gp) to "
        ++ customer.name
        ++ " for "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gp"


basicButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
basicButton attributes messages =
    button (Attr.style "margin" "2px" :: attributes) messages


modifyOfferButton : Int -> Model -> Html Msg
modifyOfferButton offerDiff model =
    basicButton
        [ Attr.attribute "aria-label" (String.fromInt offerDiff ++ " offer price modify")
        , onClick <| PcOffer <| String.fromInt <| model.offerInfo.pcOffer + offerDiff
        ]
        [ text
            ((if offerDiff > 0 then
                "+"

              else
                ""
             )
                ++ String.fromInt offerDiff
            )
        ]


modifyWaitButton : Int -> Model -> Html Msg
modifyWaitButton timeDiff model =
    basicButton
        [ Attr.attribute "aria-label" (String.fromInt timeDiff ++ " waiting time modify")
        , onClick <| UpdateWaitTime <| String.fromInt <| model.waitTime + timeDiff
        ]
        [ text
            ((if timeDiff > 0 then
                "+"

              else
                ""
             )
                ++ String.fromInt timeDiff
            )
        ]


stockBlock : Model -> List (Html Msg)
stockBlock model =
    [ h3 [] [ text "Stock" ]
    , basicButton [ Attr.attribute "aria-label" "Sword offer", onClick <| OfferItem swordItem ] [ text "Sword" ]
    , basicButton [ Attr.attribute "aria-label" "Bag of trail mix offer", onClick <| OfferItem trailMixItem ] [ text "Bag of trail mix" ]
    ]


customersBlockOpen : Model -> List (Html Msg)
customersBlockOpen model =
    [ h3 [] [ text "Customers" ]
    , div []
        (Clientele.customerEntryButtons
            (\c -> onClick (CustomerEntry c))
            model.customers
        )
    , br [] []
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
    ]


customersBlockClosed : List (Html Msg)
customersBlockClosed =
    [ h3 [] [ text "Customers" ]
    , div []
        [ text storeClosedMessage ]
    ]


actionsBlockOpen : List (Html Msg)
actionsBlockOpen =
    [ h3 [] [ text "Actions" ]
    , div []
        [ basicButton [ onClick PrepSubmitOffer ] [ text "Sale" ]
        , basicButton [ onClick PrepSchmoozeCustomer ] [ text "Schmooze" ]
        , basicButton [ onClick PrepInspectCustomer ] [ text "Inspect" ]
        , basicButton [ onClick PrepKickOutCustomer ] [ text "Kick Out" ]
        , basicButton [ onClick PrepCleanStore ] [ text "Clean" ]
        , basicButton [ onClick PrepWaitAwhile ] [ text "Wait" ]
        ]
    ]


actionsBlockClosed : List (Html Msg)
actionsBlockClosed =
    [ h3 [] [ text "Actions" ]
    , div []
        [ basicButton [ onClick PrepOpenStore ] [ text "Open Store" ]
        ]
    ]


storyBlock : Model -> List (Html Msg)
storyBlock model =
    [ h3 [] [ text "The story thus far: " ]
    , button [ onClick ClearStory ] [ text "Clear Story" ]
    , button [ onClick ReverseStory ]
        [ text <|
            if model.isConvoReverse then
                "Story reversed"

            else
                "Story forwards"
        ]
    , div []
        []
    , textarea [ Attr.attribute "aria-live" "assertive", Attr.readonly True, Attr.cols (model.windowWidth // 10), Attr.rows 15 ]
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


timeToHoursMinutes : Time -> ( Int, Int )
timeToHoursMinutes time =
    let
        hour =
            remainderBy hoursInDay (time // minutesInHour)

        minute =
            remainderBy minutesInHour time
    in
    ( hour, minute )


displayTime : Time -> String
displayTime time =
    let
        ( hour, minute ) =
            timeToHoursMinutes time
    in
    (if hour < 10 then
        "0"

     else
        ""
    )
        ++ String.fromInt hour
        ++ ":"
        ++ (if minute < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt minute


swordItem : Item
swordItem =
    { itemName = "sword", itemWorth = 20 }


trailMixItem : Item
trailMixItem =
    { itemName = "packet of trail mix", itemWorth = 5 }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
