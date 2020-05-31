module Main exposing (Model, Msg(..), calculateTimeOfNextCustomer, dayOfYear, failOnSaleNoMoney, hourOfDay, hoursInDay, incrementTimeWithMinOpen, init, main, purchaseString, storyBlock, totalSaleValueOfBasket, update, view)

import Browser
import Clientele exposing (Customer)
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, h2, h3, h4, hr, img, input, li, text, textarea, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Stock exposing (..)
import String



---- MODEL ----
-- pc stands for player character


type alias Flags =
    { windowWidth : Int }


type alias Model =
    { time : Time
    , pcGold : Int
    , cleanTime : Int
    , customers : Clientele.ClienteleDetails
    , isConvoReverse : Bool
    , conversation : List (List String)
    , lastMessage : String
    , hintMessage : String
    , windowWidth : Int
    , waitTime : Int
    , timeOfNextCustomer : Time
    , storeState : StoreState
    , statsTracker : StatsTracker
    , stockQty : StockQties
    , priceExpandedState : ExpansionState
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


type ExpansionState
    = Expanded
    | Unexpanded


type alias Time =
    Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( updateConversationWithActionMessage
        (Clientele.customerCallMessage
            Clientele.initFirstCustomer
        )
        { time = openHour * minutesInHour
        , pcGold = 0
        , cleanTime = 10
        , customers = Clientele.initCustomers
        , isConvoReverse = True
        , lastMessage = ""
        , hintMessage = "Click the question-mark next to the item if you need a hint."
        , conversation = []
        , windowWidth = flags.windowWidth
        , waitTime = 0
        , timeOfNextCustomer = (openHour * 60) + timeBetweenCustomersMins
        , storeState = Open
        , statsTracker = initStatsTracker
        , stockQty = initStockQty
        , priceExpandedState = Unexpanded
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


type alias StockQties =
    Dict String Int


initStockQty : StockQties
initStockQty =
    Dict.fromList
        [ ( Stock.swordItem.uniqueName, 2 )
        , ( Stock.axeItem.uniqueName, 5 )
        , ( Stock.daggerItem.uniqueName, 10 )
        , ( Stock.fancyChocolateItem.uniqueName, 2 )
        , ( Stock.trailMixItem.uniqueName, 5 )
        , ( Stock.cabbageItem.uniqueName, 10 )
        ]


type Msg
    = NoOp
    | SubmitConfirmSale
    | ClearStory
    | KickOutCustomer
    | CleanStore
    | ReverseStory
    | SchmoozeCustomer
    | CustomerEntry Clientele.Customer
    | UpdateWaitTime String
    | WaitAwhile
    | InspectCustomer
    | OpenStore
    | OfferAtOptimalPrice Clientele.Customer Int Item
    | GetHintForItem Customer Item


type StoreState
    = Open
    | Closed



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitConfirmSale ->
            ( submitConfirmSale <| model, Cmd.none )

        ClearStory ->
            ( { model | conversation = [] }, Cmd.none )

        KickOutCustomer ->
            ( fuckOffCustomer model, Cmd.none )

        CleanStore ->
            ( cleanStore model, Cmd.none )

        ReverseStory ->
            ( reverseStory model, Cmd.none )

        InspectCustomer ->
            ( inspectCustomer model, Cmd.none )

        SchmoozeCustomer ->
            ( schmoozeCustomer model, Cmd.none )

        CustomerEntry customer ->
            ( callNextCustomer customer model, Cmd.none )

        UpdateWaitTime waitTimeStr ->
            ( updateWaitTime waitTimeStr model, Cmd.none )

        WaitAwhile ->
            ( waitAwhile model, Cmd.none )

        OpenStore ->
            ( openStore model, Cmd.none )

        OfferAtOptimalPrice customer offer item ->
            ( offerItemAtPrice customer offer item model, Cmd.none )

        GetHintForItem customer item ->
            ( getHintForItem customer item model, Cmd.none )


reverseStory : Model -> Model
reverseStory model =
    (\mdl ->
        updateConversationWithActionMessage
            ("Story display is now "
                ++ (if mdl.isConvoReverse then
                        "reversed."

                    else
                        "forwards."
                   )
            )
            mdl
    )
    <|
        { model | isConvoReverse = not model.isConvoReverse }


optimalOfferInfo : Maybe Clientele.Customer -> PcOfferInfo -> PcOfferInfo
optimalOfferInfo maybeCustomer offerInfo =
    case maybeCustomer of
        Just customer ->
            case offerInfo.maybeItem of
                Just item ->
                    { offerInfo | pcOffer = Clientele.optimalPrice item customer }

                Nothing ->
                    offerInfo

        Nothing ->
            offerInfo


updateWaitTime : String -> Model -> Model
updateWaitTime newWaitString model =
    { model | waitTime = max 0 (Maybe.withDefault model.waitTime (String.toInt newWaitString)) }


calculateUpdatedOffer : String -> PcOfferInfo -> PcOfferInfo
calculateUpdatedOffer newOfferString info =
    { info | pcOffer = max 0 (Maybe.withDefault info.pcOffer (String.toInt newOfferString)) }


optimalPrice : Maybe Clientele.Customer -> PcOfferInfo -> PcOfferInfo
optimalPrice currentCustomer offerInfo =
    case offerInfo.maybeItem of
        Just item ->
            case currentCustomer of
                Just customer ->
                    case customer.inspectedState of
                        Clientele.Inspected ->
                            { offerInfo
                                | pcOffer = Clientele.optimalPrice item customer
                            }

                        Clientele.Uninspected ->
                            { offerInfo
                                | pcOffer = item.itemWorth
                            }

                Nothing ->
                    { offerInfo
                        | pcOffer = 0
                    }

        Nothing ->
            { offerInfo
                | pcOffer = 0
            }


offerItemAtPrice : Clientele.Customer -> Int -> Item -> Model -> Model
offerItemAtPrice customer offer item model =
    submitAddToBasketWithCustomerAndItem customer { pcOffer = offer, item = item } model


itemToOffer : Item -> PcOfferInfo -> PcOfferInfo
itemToOffer item offer =
    { offer | maybeItem = Just item }



-- TODO modify the next function (including its types) to be more functional


submitAddToBasket : PcOfferInfo -> Model -> Model
submitAddToBasket pcOfferInfo model =
    case model.customers.currentCustomer of
        Just customer ->
            case pcOfferInfo.maybeItem of
                Just item ->
                    let
                        offerInfo =
                            { pcOffer = pcOfferInfo.pcOffer, item = item }
                    in
                    submitAddToBasketWithCustomerAndItem customer offerInfo model

                Nothing ->
                    updateConversationWithActionMessage "There is no item on offer." model

        Nothing ->
            updateConversationWithActionMessage "Please address a customer before submitting an offer." model


type CustomerSaleSuccess
    = Success
    | BadDeal
    | TooManyItems
    | NoMoney


getHintForItem : Customer -> Item -> Model -> Model
getHintForItem customer item model =
    updateHintWithMessage ("Hint for " ++ item.displayName ++ ": " ++ questionSaleString customer item) model


updateHintWithMessage : String -> Model -> Model
updateHintWithMessage message model =
    updateConversationWithActionMessage message <|
        { model
            | hintMessage = message
        }



-- TODO Update question strings


questionSaleString : Customer -> Item -> String
questionSaleString customer item =
    let
        name =
            customer.name

        purse =
            customer.moneyInPurse

        cap =
            Clientele.priceCapForItemType item.itemType customer

        schmoozes =
            customer.schmoozeCount

        willingPay =
            toFloat item.itemWorth * Clientele.paymentForItemType item.itemType customer

        maxPrice =
            round <| min cap willingPay
    in
    if purse < maxPrice then
        name ++ " only has " ++ String.fromInt purse ++ " gp left in their purse."

    else if cap <= willingPay then
        (if maxPrice > item.itemWorth then
            "Selling the " ++ item.displayName ++ " would make you " ++ String.fromInt (maxPrice - item.itemWorth) ++ " gp profit. "

         else
            "This is not profitable. "
        )
            ++ name
            ++ " won't pay more than "
            ++ String.fromInt (round cap)
            ++ " gp for a single "
            ++ Stock.toString item.itemType
            ++ " item. "
            ++ (if schmoozes < Clientele.constants.maxSchmoozes then
                    "You could try schmoozing them again, but if you have a cheaper item of the same type, you'd probably make more profit selling that. "

                else
                    "If you have a cheaper item of the same type, you'd probably make more profit selling that. "
               )

    else if schmoozes < Clientele.constants.maxSchmoozes then
        "Try schmoozing " ++ name ++ " again! "

    else if round willingPay <= item.itemWorth then
        name
            ++ " isn't willing to buy more "
            ++ Stock.toString item.itemType
            ++ " items at a profitable price. "
            ++ "Cut your losses and sell an item of a different type or conclude the sale. "

    else
        "You can sell this item to "
            ++ name
            ++ " for a profit. They simply aren't interested in buying it for more than "
            ++ String.fromInt maxPrice
            ++ "gp. "
            ++ "Either offer the item to them, or switch to a richer customer. "


submitAddToBasketWithCustomerAndItem : Clientele.Customer -> OfferInfo -> Model -> Model
submitAddToBasketWithCustomerAndItem customer offerInfo model =
    case determineIfSale customer offerInfo model of
        Success ->
            addToBasket customer offerInfo model

        BadDeal ->
            failOnSaleBadDeal customer offerInfo model

        NoMoney ->
            failOnSaleNoMoney customer offerInfo model

        TooManyItems ->
            failOnSaleTooManyItems customer offerInfo model


submitConfirmSale : Model -> Model
submitConfirmSale model =
    case model.customers.currentCustomer of
        Just customer ->
            confirmSale customer model

        Nothing ->
            updateConversationWithActionMessage "Please address a customer before confirming an offer." model


determineIfSale : Clientele.Customer -> OfferInfo -> Model -> CustomerSaleSuccess
determineIfSale customer offerInfo model =
    let
        offer =
            offerInfo.pcOffer
    in
    if offer > customer.moneyInPurse then
        NoMoney

    else if offer > Clientele.maxPrice offerInfo.item customer then
        if customer.numItemsInBasket offerInfo.item.itemType == 0 then
            BadDeal

        else
            TooManyItems

    else
        Success


updateConversationWithActionMessage : String -> Model -> Model
updateConversationWithActionMessage message model =
    { model
        | conversation = model.conversation ++ [ [ displayTime model.time, message, "" ] ]
        , lastMessage = message
    }


addToBasket : Clientele.Customer -> OfferInfo -> Model -> Model
addToBasket customer offerInfo model =
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnSuccess model.time then
        closeStore "The customer looked just about ready to buy the item! But unfortunately, what with the curfew and all, you have to tell them to come back tomorrow." model

    else
        updateModelStatsTrackerWithOffer <|
            updateBasketAndStock offerInfo <|
                updateCustomerGold offerInfo.pcOffer <|
                    updateConversationWithActionMessage (offerAndPurchaseString customer offerInfo) <|
                        incrementTimeWithMinOpen Clientele.constants.minTakenOnSuccess <|
                            model


confirmSale : Clientele.Customer -> Model -> Model
confirmSale customer model =
    updateModelStatsTrackerWithConfirmSale customer <|
        exitCurrentCustomerSuccessfulSale <|
            (\mdl -> updateConversationWithActionMessage (confirmSaleString customer mdl.pcGold) mdl) <|
                updateGoldWithSale customer <|
                    model


updateBasketAndStock : OfferInfo -> Model -> Model
updateBasketAndStock offerInfo model =
    { model
        | customers = Clientele.updateCurrentCustomerBasket offerInfo model.customers
        , stockQty = decrementDict offerInfo.item.uniqueName model.stockQty
    }


decrementDict : comparable -> Dict comparable number -> Dict comparable number
decrementDict x dict =
    Dict.update
        x
        (\maybeQty ->
            case maybeQty of
                Just qty ->
                    if qty == 1 then
                        Nothing

                    else
                        Just (qty - 1)

                Nothing ->
                    Nothing
        )
        dict


updateCustomerGold : Int -> Model -> Model
updateCustomerGold pcOffer model =
    { model | customers = Clientele.updateCurrentCustomerGold pcOffer model.customers }


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



-- TODO extract common code of the lower functions up by one function


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


failOnSaleTooManyItems : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSaleTooManyItems customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateModelStatsTrackerWithOffer <|
            updateConversationWithActionMessage (rejectStringTooManyItems customer offer) <|
                incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <|
                    model


fuckOffCustomer : Model -> Model
fuckOffCustomer model =
    updateModelStatsTrackerWithFuckoff <| updateConversationWithActionMessage (Clientele.customerFuckOffMessage model.customers) <| exitCurrentCustomerClearBasket <| updateTimeFuckOff <| model


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
                case customer.inspectedState of
                    Clientele.Inspected ->
                        updateConversationWithActionMessage ("You've already inspected " ++ customer.name ++ ".") model

                    Clientele.Uninspected ->
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
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnSpeakingTo model.time then
        closeStore "Whilst you were changing your attention from one customer to another, the store closed, and they all left." model

    else
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


exitCurrentCustomerClearBasket : Model -> Model
exitCurrentCustomerClearBasket model =
    { model
        | customers = Clientele.exitCurrentCustomer model.customers
        , stockQty =
            restockWith model.stockQty <|
                case model.customers.currentCustomer of
                    Just customer ->
                        customer.basket

                    Nothing ->
                        []
    }


exitCurrentCustomerSuccessfulSale : Model -> Model
exitCurrentCustomerSuccessfulSale model =
    { model
        | customers = Clientele.exitCurrentCustomer model.customers
    }


restockWith : StockQties -> Clientele.BasketInfo -> StockQties
restockWith stockQty basket =
    updateDictionaryWithList stockQty <| List.map (\offer -> offer.item.uniqueName) basket


updateDictionaryWithList : Dict comparable number -> List comparable -> Dict comparable number
updateDictionaryWithList dict list =
    case list of
        [] ->
            dict

        x :: xs ->
            updateDictionaryWithList
                (Dict.update
                    x
                    (\maybeQty ->
                        case maybeQty of
                            Just qty ->
                                Just (qty + 1)

                            Nothing ->
                                Just 1
                    )
                    dict
                )
                xs


updateGoldWithSale : Clientele.Customer -> Model -> Model
updateGoldWithSale customer model =
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
        , customers = Clientele.callCustomerFromPool model.customers
        , statsTracker = initStatsTracker
    }


closeStore : String -> Model -> Model
closeStore closeMessage model =
    (\mdl -> updateConversationWithActionMessage (statsModelMessage mdl) mdl) <|
        takeRent <|
            repurchaseItems <|
                (\mdl ->
                    { mdl
                        | storeState = Closed
                        , customers = Clientele.exitAllCustomers model.customers
                        , stockQty =
                            case mdl.customers.currentCustomer of
                                Just customer ->
                                    restockWith mdl.stockQty customer.basket

                                Nothing ->
                                    mdl.stockQty
                    }
                )
                <|
                    recordNeglectedCustomers <|
                        updateConversationWithActionMessage closeMessage <|
                            incrementTimeToTimeWhilstOpen (calculateClosingTime model.time) model


takeRent : Model -> Model
takeRent model =
    let
        rent =
            50
    in
    updateConversationWithActionMessage ("Rent of " ++ String.fromInt rent ++ " is due.") <|
        { model | pcGold = model.pcGold - rent }


repurchaseItems : Model -> Model
repurchaseItems model =
    updateConversationWithActionMessage "You restock on all your items." <|
        { model | stockQty = initStockQty }


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


confirmSaleString : Clientele.Customer -> Int -> String
confirmSaleString customer pcGold =
    "You sell "
        ++ String.fromInt (List.length customer.basket)
        ++ " item(s) to "
        ++ customer.name
        ++ " for a total of "
        ++ String.fromInt (totalSaleValueOfBasket customer)
        ++ " gp. Cost price was "
        ++ String.fromInt (totalCostPriceOfBasket customer)
        ++ " gp. "
        ++ (let
                profit =
                    totalSaleValueOfBasket customer - totalCostPriceOfBasket customer
            in
            if profit > 0 then
                "This nets you a profit of " ++ String.fromInt profit ++ "gp. "

            else if profit == 0 then
                "No profit. "

            else
                "This LOST you " ++ String.fromInt -profit ++ " gp."
           )
        ++ "\n"
        ++ customer.name
        ++ ": \"Goodbye, and thanks for everything!\"\n"
        ++ "You now have a total of "
        ++ String.fromInt pcGold
        ++ " gold."


offerString : OfferInfo -> String
offerString info =
    "You offered the "
        ++ info.item.displayName
        ++ " for a price of "
        ++ String.fromInt info.pcOffer
        ++ " gp. (Item cost: "
        ++ String.fromInt info.item.itemWorth
        ++ " gp)."


purchaseString : Clientele.Customer -> OfferInfo -> String
purchaseString customer offerInfo =
    customer.name
        ++ ": \"A "
        ++ offerInfo.item.displayName
        ++ " for "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gold sounds like a good deal!\" \nThey add the "
        ++ offerInfo.item.displayName
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
        ++ offer.item.displayName
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
        ++ offer.item.displayName
        ++ ".\" \nThis exchange took "
        ++ String.fromInt Clientele.constants.minTakenOnFail
        ++ " minutes."


rejectStringTooManyItems : Clientele.Customer -> OfferInfo -> String
rejectStringTooManyItems customer offer =
    offerString offer
        ++ "\n"
        ++ customer.name
        ++ ": \""
        ++ "I don't really need another "
        ++ offer.item.displayName
        ++ ". You'd have to give me a really good deal!\"\nThis exchange took "
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


gridElement : List (Html msg) -> Html msg
gridElement theHtml =
    div
        [ Attr.class "grid-block"
        ]
        theHtml


grid : List (Html msg) -> Html msg
grid theHtml =
    div [ Attr.class "grid-of-blocks" ] theHtml


oneBlock : List (Html msg) -> Html msg
oneBlock theHtml =
    div
        [ Attr.class "full-block"
        ]
        theHtml



-- Main view


view : Model -> Html Msg
view model =
    div []
        [ div [ Attr.class "heading-box" ] [ h1 [] [ text "Trading Post" ] ]
        , Html.a [ Attr.href "https://github.com/mark-chimes/trading-post", Attr.target "_blank" ] [ text "https://github.com/mark-chimes/trading-post" ]

        {-
           , h2 []
               [ text "Debug" ]
           , text
               ("Time of next customer " ++ displayTime model.timeOfNextCustomer)
           , div [] []
           , text ("minutesSinceZero: " ++ String.fromInt model.time)
        -}
        , div [] []
        , oneBlock <| storeInfo model
        , uiBasedOnStoreState model.storeState model
        , oneBlock <| storyBlock model
        ]


uiBasedOnStoreState : StoreState -> Model -> Html Msg
uiBasedOnStoreState storeState model =
    case storeState of
        Open ->
            grid
                [ gridElement <| stockAndOfferBlock model
                , gridElement <| basketBlockOpen model
                , gridElement <| customerInfoPanelOpen model
                , gridElement <| customersBlockOpen model
                , gridElement <| currentSituationBlockOpen model
                , gridElement <| lastMessagePanel model
                ]

        Closed ->
            grid
                [ gridElement <| stockAndOfferBlock model
                , gridElement <| basketBlockClosed
                , gridElement <| customerInfoPanelClosed
                , gridElement <| customersBlockClosed
                , gridElement <| currentSituationBlockClosed model
                , gridElement <| lastMessagePanel model
                ]


storeInfo : Model -> List (Html Msg)
storeInfo model =
    [ h2 [] [ text "Store" ]
    , div [ Attr.class "date" ] [ text ("Day " ++ (String.fromInt <| dayOfYear model.time)) ]
    , div [ Attr.class "time" ] [ text (displayTime model.time) ]
    , div [ Attr.class "gold" ] [ text (String.fromInt model.pcGold ++ " gold") ]
    ]


nooneMessage : String
nooneMessage =
    "Select a customer to address."


storeClosedMessage : String
storeClosedMessage =
    "The store is now closed."


currentSituationBlockOpen : Model -> List (Html Msg)
currentSituationBlockOpen model =
    [ h3 [] [ text "Wait" ]
    , div []
        [ basicButton [ onClick CleanStore ] [ text "Clean Store" ]
        , div [] []
        , basicButton [ Attr.attribute "aria-label" "Reset waiting time to 0", onClick <| UpdateWaitTime <| String.fromInt 0 ] [ text "Reset" ]
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
    ]



-- offer > Clientele.maxPrice offerInfo.item customer


basketBox : Clientele.Customer -> Html Msg
basketBox customer =
    div [ Attr.class "basket-box" ] <|
        if List.length customer.basket == 0 then
            [ basicButton [ onClick KickOutCustomer ] [ text <| "Kick out " ++ customer.name ] ]

        else
            [ basicButton [ onClick SubmitConfirmSale ] [ text <| "Confirm Sale of " ++ String.fromInt (List.length customer.basket) ++ " items and Say Goodbye" ]
            , div [] <|
                List.map
                    (\s -> div [ Attr.class "basket" ] [ text s ])
                <|
                    [ "Items in basket:"
                    ]
                        ++ List.map itemDisplay customer.basket
            ]


itemDisplay : OfferInfo -> String
itemDisplay offerInfo =
    offerInfo.item.displayName
        ++ " ("
        ++ String.fromInt offerInfo.item.itemWorth
        ++ " gp) "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gp"


currentSituationBlockClosed : Model -> List (Html Msg)
currentSituationBlockClosed _ =
    [ h3 [] [ text "Wait" ]
    , basicButton [ onClick OpenStore ] [ text <| "Skip until tomorrow and open store at " ++ String.fromInt openHour ++ " o Clock." ]
    ]


customerInfoPanelOpen : Model -> List (Html Msg)
customerInfoPanelOpen model =
    [ div []
        (case model.customers.currentCustomer of
            Just customer ->
                [ h3 [] [ text <| customer.name ]
                , h4 [] [ text <| Clientele.mainInfo customer ]
                , div [] <|
                    [ schmoozeButton customer
                    , div [] <| List.map (\s -> div [] [ text s ]) <| Clientele.customerDisplay customer
                    ]
                        ++ (case customer.inspectedState of
                                Clientele.Inspected ->
                                    []

                                Clientele.Uninspected ->
                                    [ basicButton [ onClick InspectCustomer ] [ text <| "Inspect " ++ customer.name ] ]
                           )
                ]

            Nothing ->
                [ h3 [] [ text "Customer Info" ]
                , div [] [ text "No customer" ]
                ]
        )
    ]


schmoozeButton : Customer -> Html Msg
schmoozeButton customer =
    if customer.schmoozeCount >= Clientele.constants.maxSchmoozes then
        basicButton [ Attr.disabled True ] [ text <| schmoozeButtonText customer ]

    else
        basicButton [ onClick SchmoozeCustomer ] [ text <| schmoozeButtonText customer ]


schmoozeButtonText : Customer -> String
schmoozeButtonText customer =
    "Schmooze ("
        ++ String.fromInt customer.schmoozeCount
        ++ (if customer.schmoozeCount == Clientele.constants.maxSchmoozes then
                " MAX"

            else
                ""
           )
        ++ ") "
        ++ customer.name


customerInfoPanelClosed : List (Html Msg)
customerInfoPanelClosed =
    [ h3 [] [ text "Customer Info" ]
    , div []
        [ text storeClosedMessage ]
    ]


basicButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
basicButton attributes messages =
    button (Attr.style "margin" "2px" :: attributes) messages


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


stockAndOfferBlock : Model -> List (Html Msg)
stockAndOfferBlock model =
    [ h3 [] [ text "Stock and Offer" ]

    -- , div [] (List.map stockItemButton <| Dict.toList model.stockQty)
    , br [] []
    , case model.customers.currentCustomer of
        Nothing ->
            div [] [ text nooneMessage ]

        Just customer ->
            div [] <| priceBoxes customer model
    , h4 [] [ text "Hints" ]
    , Html.pre [] [ text model.hintMessage ]
    ]


priceBoxes : Customer -> Model -> List (Html Msg)
priceBoxes customer model =
    List.concatMap (\itemType -> priceBoxByType customer itemType model) Stock.itemTypesEnum


priceBoxByType : Customer -> Stock.ItemType -> Model -> List (Html Msg)
priceBoxByType customer itemType model =
    h4 [] [ text <| Stock.toString itemType ]
        :: itemListHtmlByType
            customer
            itemType
            model


itemListHtmlByType : Customer -> Stock.ItemType -> Model -> List (Html Msg)
itemListHtmlByType customer itemType model =
    List.map (priceBox customer) <|
        List.reverse <|
            List.sortBy (\( item, _ ) -> item.itemWorth) <|
                List.filter (\( item, _ ) -> item.itemType == itemType) <|
                    List.filterMap
                        (\( maybeItem, qty ) ->
                            case maybeItem of
                                Just item ->
                                    Just ( item, qty )

                                Nothing ->
                                    Nothing
                        )
                    <|
                        List.map
                            (\( itemName, qty ) ->
                                ( Stock.itemForName itemName, qty )
                            )
                        <|
                            Dict.toList model.stockQty


priceBox : Clientele.Customer -> ( Item, Int ) -> Html Msg
priceBox customer ( item, quantity ) =
    let
        price =
            Clientele.optimalPrice item customer
    in
    div []
        [ text <|
            item.displayName
                ++ " x"
                ++ String.fromInt quantity
                ++ " "
        , basicButton [ Attr.attribute "aria-label" "Hint", onClick <| GetHintForItem customer item ] [ text "?" ]
        , basicButton [ onClick <| OfferAtOptimalPrice customer price item ]
            [ text <|
                " Offer "
                    ++ item.displayName
                    ++ " for "
                    ++ String.fromInt price
                    ++ " gp"
                    ++ " (cost "
                    ++ String.fromInt item.itemWorth
                    ++ " gp)"
            ]
        ]


customersBlockOpen : Model -> List (Html Msg)
customersBlockOpen model =
    [ h3 []
        [ text "Customers" ]
    , div
        []
        [ div [] <|
            text "Rich"
                :: Clientele.customerEntryButtons
                    (\c -> onClick (CustomerEntry c))
                    Clientele.Rich
                    model.customers
        , div [] <|
            text "Well-Off"
                :: Clientele.customerEntryButtons
                    (\c -> onClick (CustomerEntry c))
                    Clientele.WellOff
                    model.customers
        , div [] <|
            text "Average"
                :: Clientele.customerEntryButtons
                    (\c -> onClick (CustomerEntry c))
                    Clientele.Average
                    model.customers
        , div [] <|
            text "Poor"
                :: Clientele.customerEntryButtons
                    (\c -> onClick (CustomerEntry c))
                    Clientele.Poor
                    model.customers
        , div [] <|
            text "Destitute"
                :: Clientele.customerEntryButtons
                    (\c -> onClick (CustomerEntry c))
                    Clientele.Destitute
                    model.customers
        ]
    , br [] []
    , div []
        (case model.customers.currentCustomer of
            Just customer ->
                [ text ("You are speaking to: " ++ customer.name ++ ".")
                , div [] []
                , basicButton [ onClick KickOutCustomer ] [ text <| "Kick out " ++ customer.name ]
                ]

            Nothing ->
                []
        )
    ]


customersBlockClosed : List (Html Msg)
customersBlockClosed =
    [ h3 [] [ text "Customers" ]
    , div []
        [ text storeClosedMessage ]
    ]


basketBlockOpen : Model -> List (Html Msg)
basketBlockOpen model =
    [ h3 [] [ text "Basket" ]
    , case model.customers.currentCustomer of
        Nothing ->
            div [] [ text nooneMessage ]

        Just customer ->
            div [] [ basketBox customer ]
    ]


basketBlockClosed : List (Html Msg)
basketBlockClosed =
    [ h3 [] [ text "Sale" ]
    , text storeClosedMessage
    ]


lastMessagePanel : Model -> List (Html Msg)
lastMessagePanel model =
    [ h3 [] [ text "Last Event" ]
    , Html.pre [] [ text model.lastMessage ]
    ]


storyBlock : Model -> List (Html Msg)
storyBlock model =
    [ h3 [] [ text "The story thus far: " ]

    -- , button [ onClick ClearStory ] [ text "Clear Story" ]
    , div []
        [ text <|
            if model.isConvoReverse then
                "Story reversed "

            else
                "Story forwards "
        , button [ onClick ReverseStory ] [ text <| "Reverse" ]
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



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
