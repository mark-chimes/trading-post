module Main exposing (Model, Msg(..), calculateTimeOfNextCustomer, dayOfYear, failOnSaleNoMoney, hourOfDay, hoursInDay, incrementTimeWithMinOpen, init, main, purchaseItem, purchaseString, storyBlock, totalSaleValueOfBasket, update, view)

import Browser
import Clientele exposing (Customer)
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, h2, h3, h4, hr, img, input, li, text, textarea, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Item exposing (Item)
import ItemType exposing (ItemType)
import String



---- MODEL ----
-- pc stands for player character


type alias Flags =
    { windowWidth : Int }


type alias MainModel =
    { playerName : String
    , storeName : String
    , model : Maybe Model
    , windowWidth : Int
    }


type alias Model =
    { playerName : String
    , storeName : String
    , windowWidth : Int
    , time : Time
    , pcGold : Int
    , rent : Int
    , cleanTime : Int
    , customers : Clientele.ClienteleDetails
    , isConvoReverse : Bool
    , conversation : List (List String)
    , lastMessage : String
    , hintMessage : String
    , waitTime : Int
    , timeOfNextCustomer : Time
    , storeState : StoreState
    , statsTracker : StatsTracker
    , stockQty : StockQties
    }


type GameState
    = Intro
    | Started


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


init : Flags -> ( MainModel, Cmd Msg )
init flags =
    ( { playerName = "Biloe Celhai"
      , storeName = "Trading Post"
      , windowWidth = flags.windowWidth
      , model = Nothing
      }
    , Cmd.none
    )


initModel : String -> String -> Int -> Model
initModel playerName storeName windowWidth =
    updateConversationWithActionMessage
        (Clientele.customerCallMessage <|
            Clientele.initFirstCustomer playerName storeName
        )
        { playerName = playerName
        , storeName = storeName
        , windowWidth = windowWidth
        , time = openHour * minutesInHour
        , pcGold = 0
        , rent = 300
        , cleanTime = 10
        , customers = Clientele.initCustomers playerName storeName
        , isConvoReverse = True
        , lastMessage = ""
        , hintMessage = "Click the hint button (question-mark) next to the item if you need a hint."
        , conversation = []
        , waitTime = 0
        , timeOfNextCustomer = (openHour * 60) + timeBetweenCustomersMins
        , storeState = Open
        , statsTracker = initStatsTracker
        , stockQty = initStockQty
        }


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
        [ ( Item.sword.uniqueName, 1 )
        , ( Item.shortsword.uniqueName, 2 )
        , ( Item.axe.uniqueName, 5 )
        , ( Item.dagger.uniqueName, 7 )
        , ( Item.club.uniqueName, 10 )
        , ( Item.porterhouse.uniqueName, 1 )
        , ( Item.fancyChocolate.uniqueName, 2 )
        , ( Item.trailMix.uniqueName, 5 )
        , ( Item.vegetables.uniqueName, 7 )
        , ( Item.cabbage.uniqueName, 10 )
        ]


type MainMessageType
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
    | OfferAtOptimalPrice Clientele.Customer Int Item.Item
    | GetHintForItem Customer Item.Item
    | PurchaseItem Item.Item


type OtherMessageType
    = UpdateYourName String
    | UpdateStoreName String
    | StartGame


type Msg
    = MainMsg MainMessageType
    | OtherMsg OtherMessageType


type StoreState
    = Open
    | Closed



---- UPDATE ----


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg mainModel =
    case msg of
        OtherMsg otherMsg ->
            case otherMsg of
                UpdateYourName name ->
                    ( updateName name mainModel, Cmd.none )

                UpdateStoreName name ->
                    ( updateStoreName name mainModel, Cmd.none )

                StartGame ->
                    ( startGame mainModel, Cmd.none )

        MainMsg mainMsg ->
            case mainModel.model of
                Nothing ->
                    ( mainModel, Cmd.none )

                Just model ->
                    setModel mainModel <|
                        case mainMsg of
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

                            PurchaseItem item ->
                                ( purchaseItem item model, Cmd.none )


setModel : MainModel -> ( Model, Cmd Msg ) -> ( MainModel, Cmd Msg )
setModel mainModel ( model, cmd ) =
    ( { mainModel | model = Just model }, cmd )


startGame : MainModel -> MainModel
startGame mainModel =
    { mainModel | model = Just <| initModel mainModel.playerName mainModel.storeName mainModel.windowWidth }


updateName : String -> MainModel -> MainModel
updateName name model =
    { model | playerName = name }


updateStoreName : String -> MainModel -> MainModel
updateStoreName name model =
    { model | storeName = name }


purchaseItem : Item -> Model -> Model
purchaseItem item model =
    if model.pcGold < item.itemWorth then
        updateConversationWithActionMessage
            ("Can't afford to buy "
                ++ item.displayName
                ++ ". It costs "
                ++ String.fromInt item.itemWorth
                ++ " gp, but you only have "
                ++ String.fromInt model.pcGold
                ++ " gp. "
            )
            model

    else
        (\mdl ->
            updateConversationWithActionMessage
                ("Purchased "
                    ++ item.displayName
                    ++ " for "
                    ++ String.fromInt item.itemWorth
                    ++ " gp. "
                    ++ " You now have "
                    ++ String.fromInt (Maybe.withDefault -1 (Dict.get item.uniqueName mdl.stockQty))
                    ++ " of this item and a total of "
                    ++ String.fromInt model.pcGold
                    ++ " gp. "
                )
                mdl
        )
        <|
            { model | stockQty = incrementDict item.uniqueName model.stockQty, pcGold = model.pcGold - item.itemWorth }


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


updateWaitTime : String -> Model -> Model
updateWaitTime newWaitString model =
    { model | waitTime = max 0 (Maybe.withDefault model.waitTime (String.toInt newWaitString)) }


optimalPrice : Maybe Clientele.Customer -> Item.PcOfferInfo -> Item.PcOfferInfo
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
            ++ ItemType.toString item.itemType
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
            ++ ItemType.toString item.itemType
            ++ " items at a profitable price. "
            ++ "Cut your losses and sell an item of a different type or conclude the sale. "

    else
        "You can sell this item to "
            ++ name
            ++ " for a profit. They simply aren't interested in buying it for more than "
            ++ String.fromInt maxPrice
            ++ "gp. "
            ++ "Either offer the item to them, or switch to a richer customer. "


submitAddToBasketWithCustomerAndItem : Clientele.Customer -> Item.OfferInfo -> Model -> Model
submitAddToBasketWithCustomerAndItem customer offerInfo model =
    case determineIfSale customer offerInfo of
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


determineIfSale : Clientele.Customer -> Item.OfferInfo -> CustomerSaleSuccess
determineIfSale customer offerInfo =
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


addToBasket : Clientele.Customer -> Item.OfferInfo -> Model -> Model
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


updateBasketAndStock : Item.OfferInfo -> Model -> Model
updateBasketAndStock offerInfo model =
    { model
        | customers = Clientele.updateCurrentCustomerBasket offerInfo model.customers
        , stockQty = decrementDict offerInfo.item.uniqueName model.stockQty
    }


incrementDict : comparable -> Dict comparable number -> Dict comparable number
incrementDict x dict =
    Dict.update
        x
        (\maybeQty ->
            case maybeQty of
                Just qty ->
                    Just (qty + 1)

                Nothing ->
                    Just 1
        )
        dict


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


failOnSaleNoMoney : Clientele.Customer -> Item.OfferInfo -> Model -> Model
failOnSaleNoMoney customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateModelStatsTrackerWithOffer <|
            updateConversationWithActionMessage (rejectStringNoMoney customer offer) <|
                incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <|
                    model


failOnSaleBadDeal : Clientele.Customer -> Item.OfferInfo -> Model -> Model
failOnSaleBadDeal customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateModelStatsTrackerWithOffer <|
            updateConversationWithActionMessage (rejectStringBadDeal customer offer) <|
                incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <|
                    model


failOnSaleTooManyItems : Clientele.Customer -> Item.OfferInfo -> Model -> Model
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
        | customers = Clientele.exitCurrentCustomer model.playerName model.storeName model.customers
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
        | customers = Clientele.exitCurrentCustomer model.playerName model.storeName model.customers
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
        (Clientele.customerCallMessage <|
            Clientele.initFirstCustomer model.playerName model.storeName
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
            (\mdl ->
                { mdl
                    | storeState = Closed
                    , customers = Clientele.exitAllCustomers model.playerName model.storeName model.customers
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
    updateConversationWithActionMessage ("Rent of " ++ String.fromInt model.rent ++ " gp is due.") <|
        { model | pcGold = model.pcGold - model.rent }


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
        ++ " After rent of "
        ++ String.fromInt model.rent
        ++ " You ended the day with a total of "
        ++ String.fromInt model.pcGold
        ++ " gp"


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


offerAndPurchaseString : Clientele.Customer -> Item.OfferInfo -> String
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


offerString : Item.OfferInfo -> String
offerString info =
    "You offered the "
        ++ info.item.displayName
        ++ " for a price of "
        ++ String.fromInt info.pcOffer
        ++ " gp. (Item cost: "
        ++ String.fromInt info.item.itemWorth
        ++ " gp)."


purchaseString : Clientele.Customer -> Item.OfferInfo -> String
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


rejectStringBadDeal : Clientele.Customer -> Item.OfferInfo -> String
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


rejectStringNoMoney : Clientele.Customer -> Item.OfferInfo -> String
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


rejectStringTooManyItems : Clientele.Customer -> Item.OfferInfo -> String
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


view : MainModel -> Html Msg
view mainModel =
    div [] <|
        case mainModel.model of
            Nothing ->
                startGameView mainModel

            Just model ->
                activeGameView model


startGameView : MainModel -> List (Html Msg)
startGameView mainModel =
    [ div [ Attr.class "heading-box" ] [ h1 [] [ text mainModel.storeName ] ]
    , Html.a [ Attr.href "https://github.com/mark-chimes/trading-post", Attr.target "_blank" ] [ text "https://github.com/mark-chimes/trading-post" ]
    , oneBlock <|
        [ h2 [] [ text "Welcome to Trading Post!" ]
        , div [ Attr.class "start-game-container" ]
            [ div [ Attr.class "start-game-element" ]
                [ text "Your Name: "
                , input
                    [ Attr.attribute "aria-label" "Your name"
                    , value mainModel.playerName
                    , onInput <| \s -> OtherMsg <| UpdateYourName s
                    ]
                    []
                ]
            , div [ Attr.class "start-game-element" ]
                [ text "Store Name: "
                , input
                    [ Attr.attribute "aria-label" "Store name"
                    , value mainModel.storeName
                    , onInput <| \s -> OtherMsg <| UpdateStoreName s
                    ]
                    []
                ]
            ]
        , div [ Attr.class "start-game-button-container" ]
            [ basicButton [ Attr.class "start-game-button", onClick (OtherMsg StartGame) ] [ text "Start Game" ]
            ]
        ]
    ]


activeGameView : Model -> List (Html Msg)
activeGameView model =
    [ div [ Attr.class "heading-box" ] [ h1 [] [ text model.storeName ] ]
    , uiBasedOnStoreState model.storeState model
    , oneBlock <| storyBlock model
    ]


uiBasedOnStoreState : StoreState -> Model -> Html Msg
uiBasedOnStoreState storeState model =
    case storeState of
        Open ->
            grid
                [ gridElement <| stockAndOfferBlock model
                , gridElement <| customerInfoPanel model
                , gridElement <| storeBlock model
                , gridElement <| customerConversationBlock model
                , gridElement <| waitBlock model
                , gridElement <| lastMessagePanel model
                ]

        Closed ->
            div []
                [ grid
                    [ gridElement <| stockAndOfferBlock model
                    , gridElement <| lastMessagePanel model
                    ]
                , oneBlock <| skipTomorrowOpenStoreBlock model
                ]


storeInfo : Model -> Html Msg
storeInfo model =
    div [ Attr.class "store-info-box" ]
        [ div []
            [ div [ Attr.class "store-info-element" ] [ text "Day" ]
            , div [ Attr.class "store-info-element" ] [ text (String.fromInt <| dayOfYear model.time) ]
            ]
        , div
            []
            [ div [ Attr.class "store-info-element" ] [ text "Time" ]
            , div [ Attr.class "store-info-element" ] [ text (displayTime model.time) ]
            ]
        , div []
            [ div
                [ Attr.class "store-info-element" ]
                [ text "Gold" ]
            , div [ Attr.class "store-info-element" ] [ text <| String.fromInt model.pcGold ++ " gp" ]
            ]
        ]


waitBlock : Model -> List (Html Msg)
waitBlock model =
    [ h3 [] [ text "Wait" ]
    , div []
        [ basicButton [ onClick <| MainMsg <| CleanStore ] [ text "Clean Store" ]
        , div [] []
        , basicButton [ Attr.attribute "aria-label" "Reset waiting time to 0", onClick <| MainMsg <| UpdateWaitTime <| String.fromInt 0 ] [ text "Reset" ]
        , modifyWaitButton -60 model
        , modifyWaitButton -10 model
        , input
            [ Attr.attribute "aria-label" "Time to wait"
            , Attr.style "margin" "2px"
            , Attr.type_ "number"
            , Attr.min "0"
            , Attr.max "1440"
            , value (String.fromInt model.waitTime)
            , onInput <| \s -> MainMsg <| UpdateWaitTime s
            ]
            []
        , modifyWaitButton 10 model
        , modifyWaitButton 60 model
        , div [] []
        , basicButton
            [ onClick <| MainMsg <| WaitAwhile ]
            [ text <| "Wait for " ++ String.fromInt model.waitTime ++ " minutes" ]
        ]
    ]



-- offer > Clientele.maxPrice offerInfo.item customer


basketBox : Clientele.Customer -> Html Msg
basketBox customer =
    div [ Attr.class "basket-box" ] <|
        if List.length customer.basket == 0 then
            [ basicButton [ onClick <| MainMsg <| KickOutCustomer ] [ text <| "Kick out " ++ customer.name ] ]

        else
            [ basicButton [ onClick <| MainMsg <| SubmitConfirmSale ] [ text <| "Confirm Sale of " ++ String.fromInt (List.length customer.basket) ++ " items and Say Goodbye" ]
            , div [] <|
                List.map
                    (\s -> div [ Attr.class "basket" ] [ text s ])
                <|
                    [ "Items in basket:"
                    ]
                        ++ List.map itemDisplay customer.basket
            ]


itemDisplay : Item.OfferInfo -> String
itemDisplay offerInfo =
    offerInfo.item.displayName
        ++ " ("
        ++ String.fromInt offerInfo.item.itemWorth
        ++ " gp) "
        ++ String.fromInt offerInfo.pcOffer
        ++ " gp"


skipTomorrowOpenStoreBlock : Model -> List (Html Msg)
skipTomorrowOpenStoreBlock _ =
    [ h3 [] [ text "Wait" ]
    , basicButton [ onClick <| MainMsg <| OpenStore ] [ text <| "Skip until tomorrow and open store at " ++ String.fromInt openHour ++ " o Clock." ]
    ]


customerInfoPanel : Model -> List (Html Msg)
customerInfoPanel model =
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
                                    [ basicButton [ onClick <| MainMsg <| InspectCustomer ] [ text <| "Inspect " ++ customer.name ] ]
                           )
                , h4 [] [ text "Basket" ]
                , basketBox customer
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
        basicButton [ onClick <| MainMsg <| SchmoozeCustomer ] [ text <| schmoozeButtonText customer ]


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


basicButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
basicButton attributes messages =
    button (Attr.style "margin" "2px" :: attributes) messages


modifyWaitButton : Int -> Model -> Html Msg
modifyWaitButton timeDiff model =
    basicButton
        [ Attr.attribute "aria-label" (String.fromInt timeDiff ++ " waiting time modify")
        , onClick <| MainMsg <| UpdateWaitTime <| String.fromInt <| model.waitTime + timeDiff
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


customerConversationBlock : Model -> List (Html Msg)
customerConversationBlock model =
    h3 [] [ text "Conversation" ]
        :: (case model.customers.currentCustomer of
                Just customer ->
                    [ div [ Attr.class "grid-of-convo-headings" ]
                        [ div [] [ text "You" ]
                        , div [] [ text "Them" ]
                        ]
                    , div
                        [ Attr.class "grid-of-convo" ]
                      <|
                        convertMessageListToGrid
                            customer.conversation
                    ]

                Nothing ->
                    [ div [] [ text "No customer in store" ] ]
           )


convertMessageListToGrid : List ( String, String ) -> List (Html Msg)
convertMessageListToGrid listOfPairs =
    List.concatMap
        (\( x, y ) ->
            [ div [ Attr.class "convo-left" ] [ text x ]
            , div [ Attr.class "convoEmpty" ] []
            , div [ Attr.class "convoEmpty" ] []
            , div [ Attr.class "convo-right " ] [ text y ]
            , div [ Attr.class "convoEmpty" ] []
            , div [ Attr.class "convoEmpty" ] []
            ]
        )
        listOfPairs


stockAndOfferBlock : Model -> List (Html Msg)
stockAndOfferBlock model =
    [ h3 [] [ text "Stock and Offer" ]
    , div [] <| priceBoxes model.customers.currentCustomer model
    , div [] <|
        case model.storeState of
            Closed ->
                h4 [] [ text "Purchase" ]
                    :: List.map purchaseItemButton Item.itemsList

            Open ->
                []
    ]


purchaseItemButton : Item -> Html Msg
purchaseItemButton item =
    basicButton [ onClick <| MainMsg <| PurchaseItem item ]
        [ text <|
            item.displayName
                ++ " for "
                ++ String.fromInt item.itemWorth
                ++ " gp"
        ]


priceBoxes : Maybe Customer -> Model -> List (Html Msg)
priceBoxes maybeCust model =
    (div [ Attr.class "stock-table" ] <| List.concatMap (\itemType -> priceBoxByType maybeCust itemType model) ItemType.itemTypesEnum)
        :: (case maybeCust of
                Just _ ->
                    [ h4 [] [ text "Hints" ]
                    , Html.pre [] [ text model.hintMessage ]
                    ]

                Nothing ->
                    []
           )


priceBoxByType : Maybe Customer -> ItemType -> Model -> List (Html Msg)
priceBoxByType maybeCust itemType model =
    h4 [ Attr.class "stock-heading" ] [ text <| ItemType.toString itemType ]
        :: itemListHtmlByType
            maybeCust
            itemType
            model


itemListHtmlByType : Maybe Customer -> ItemType -> Model -> List (Html Msg)
itemListHtmlByType maybeCust itemType model =
    (case maybeCust of
        Nothing ->
            List.map priceBoxNoone

        Just customer ->
            List.concatMap (priceBoxCustomer customer)
    )
    <|
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
                                ( Item.itemForName itemName, qty )
                            )
                        <|
                            Dict.toList model.stockQty


priceBoxNoone : ( Item.Item, Int ) -> Html Msg
priceBoxNoone ( item, quantity ) =
    div []
        [ text <|
            item.displayName
                ++ " x"
                ++ String.fromInt quantity
                ++ " (cost "
                ++ String.fromInt item.itemWorth
                ++ " gp)"
        ]


priceBoxCustomer : Clientele.Customer -> ( Item.Item, Int ) -> List (Html Msg)
priceBoxCustomer customer ( item, quantity ) =
    let
        price =
            Clientele.optimalPrice item customer
    in
    [ basicButton [ Attr.class "item-hint", Attr.attribute "aria-label" "Hint", onClick <| MainMsg <| GetHintForItem customer item ] [ text "?" ]
    , div [ Attr.class "item-name" ] [ text item.displayName ]
    , div [ Attr.class "item-qty" ]
        [ text <| String.fromInt quantity ]
    , basicButton
        [ Attr.class "item-purchase"
        , onClick <| MainMsg <| OfferAtOptimalPrice customer price item
        , Attr.attribute "aria-label" <|
            " Offer "
                ++ item.displayName
                ++ " for "
                ++ String.fromInt price
                ++ " gp"
                ++ " (cost "
                ++ String.fromInt item.itemWorth
                ++ " gp)"
        ]
        [ text <|
            String.fromInt price
                ++ " gp"
        ]
    , div [ Attr.attribute "aria-label" <| "cost " ++ String.fromInt item.itemWorth ++ " gp", Attr.class "item-cost" ] [ text <| String.fromInt item.itemWorth ++ " gp" ]
    ]


storeBlock : Model -> List (Html Msg)
storeBlock model =
    [ h3 [] [ text "Store" ]
    , storeInfo model
    , h4 [] [ text "Customers" ]
    , div
        [ Attr.class "customers-box" ]
      <|
        customersOfWealthLevel "Rich" Clientele.Rich model.customers
            ++ customersOfWealthLevel "Well-Off" Clientele.WellOff model.customers
            ++ customersOfWealthLevel "Average" Clientele.Average model.customers
            ++ customersOfWealthLevel "Poor" Clientele.Poor model.customers
            ++ customersOfWealthLevel "Destitute" Clientele.Destitute model.customers
    ]


customersOfWealthLevel : String -> Clientele.WealthLevel -> Clientele.ClienteleDetails -> List (Html Msg)
customersOfWealthLevel levelString level customers =
    [ div [ Attr.class "customers-name-element" ] [ h4 [ Attr.class "customer-name-element-inner" ] [ text levelString ] ]
    , div [ Attr.class "customers-buttons-element" ] <|
        let
            customersButtons =
                Clientele.customerEntryButtons (\c -> onClick (MainMsg <| CustomerEntry c)) level customers
        in
        case customersButtons of
            [] ->
                [ div [] [ text "None" ] ]

            xs ->
                xs
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
        , button [ onClick <| MainMsg <| ReverseStory ] [ text <| "Reverse" ]
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


main : Program Flags MainModel Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
