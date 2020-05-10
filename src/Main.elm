module Main exposing (Model, Msg(..), calculateTimeOfNextCustomer, dayOfYear, hourOfDay, hoursInDay, incrementTimeWithMinOpen, init, main, purchaseString, update, view)

import Browser
import Clientele
import Html exposing (Html, br, button, div, h1, h2, h3, h4, hr, img, input, li, text, textarea, ul)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
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
    , customersSold : Int
    , customersKicked : Int
    , customersLeft : Int
    }


type alias OfferInfo =
    { pcOffer : Int
    , itemName : String
    , itemWorth : Int
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
        , offerInfo = { pcOffer = 20, itemName = "sword", itemWorth = 20 }
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
    , customersSold = 0
    , customersKicked = 0
    , customersLeft = 0
    }



---- UPDATE ----


type Msg
    = NoOp
    | PrepSubmitOffer
    | PcOffer String
    | SubmitOffer
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

        SubmitOffer ->
            ( submitOffer <| model, Cmd.none )

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
        | pcOffer = offerInfo.itemWorth
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



-- TODO modify the next function (including its types) to be more functional


submitOffer : Model -> Model
submitOffer model =
    case model.customers.currentCustomer of
        Just customer ->
            if model.offerInfo.pcOffer <= customer.maxPrice then
                succeedOnSale customer model

            else
                failOnSale customer model.offerInfo model

        Nothing ->
            updateConversationWithActionMessage "Please address a customer before submitting an offer." model


updateConversationWithActionMessage : String -> Model -> Model
updateConversationWithActionMessage message model =
    { model | conversation = model.conversation ++ [ [ displayTime model.time, message, "" ] ] }


succeedOnSale : Clientele.Customer -> Model -> Model
succeedOnSale customer model =
    if (model.storeState == Open) && wouldStoreClose Clientele.constants.minTakenOnSuccess model.time then
        closeStore "The customer looked just about ready to buy the item! But unfortunately, what with the curfew and all, you have to tell them to come back tomorrow." model

    else
        updateModelStatsTrackerWithSale <|
            exitCurrentCustomer <|
                updateModelStatsTrackerWithGold model.offerInfo.pcOffer model.offerInfo.itemWorth <|
                    updateGold <|
                        updateConversationWithActionMessage (offerAndPurchaseString customer model.offerInfo) <|
                            incrementTimeWithMinOpen Clientele.constants.minTakenOnSuccess <|
                                model


updateModelStatsTrackerWithGold : Int -> Int -> Model -> Model
updateModelStatsTrackerWithGold sale cost model =
    { model | statsTracker = updateStatsTrackerWithProfitTurnover sale cost model.statsTracker }


updateStatsTrackerWithProfitTurnover : Int -> Int -> StatsTracker -> StatsTracker
updateStatsTrackerWithProfitTurnover sale cost statsTracker =
    { statsTracker
        | turnover = statsTracker.turnover + sale
        , profit = statsTracker.profit + (sale - cost)
    }


updateModelStatsTrackerWithSale : Model -> Model
updateModelStatsTrackerWithSale model =
    { model | statsTracker = updateStatsTrackerWithSale model.statsTracker }


updateStatsTrackerWithSale : StatsTracker -> StatsTracker
updateStatsTrackerWithSale statsTracker =
    { statsTracker | customersSold = statsTracker.customersSold + 1 }


failOnSale : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSale customer offer model =
    if wouldStoreClose Clientele.constants.minTakenOnFail model.time then
        closeStore "Unfortunately, before the client can reject your offer, the store closes, and you are forced to shoo them out." model

    else
        updateConversationWithActionMessage (rejectString customer offer) <| incrementTimeWithMinOpen Clientele.constants.minTakenOnFail <| model


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
                updateConversationWithActionMessage (Clientele.inspectCustomerMessage customer) <|
                    incrementTimeWithMinOpen Clientele.constants.minTakenOnInspect <|
                        model

            Nothing ->
                updateConversationWithActionMessage "Who are you trying to inspeect?" model


callNextCustomer : Clientele.Customer -> Model -> Model
callNextCustomer customer model =
    updateConversationWithActionMessage (Clientele.customerCallMessage customer) <|
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


updateGold : Model -> Model
updateGold model =
    { model | pcGold = model.pcGold + model.offerInfo.pcOffer }


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
    "Stats for the day: You managed to make a turnover of "
        ++ String.fromInt stats.turnover
        ++ "gp, and a profit of "
        ++ String.fromInt stats.profit
        ++ "gp. You sold items to "
        ++ String.fromInt stats.customersSold
        ++ " customers. You told "
        ++ String.fromInt stats.customersKicked
        ++ " customers to fuck right off, and your negligence resulted in "
        ++ String.fromInt stats.customersLeft
        ++ " customers leaving of their own accord."



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


offerString : OfferInfo -> String
offerString info =
    "You offered the "
        ++ info.itemName
        ++ " for a price of "
        ++ String.fromInt info.pcOffer
        ++ "gp."


purchaseString : Clientele.Customer -> OfferInfo -> String
purchaseString customer offerInfo =
    "The customer, "
        ++ customer.name
        ++ ", bought 1 "
        ++ offerInfo.itemName
        ++ " at "
        ++ String.fromInt offerInfo.pcOffer
        ++ "gp (cost price "
        ++ String.fromInt offerInfo.itemWorth
        ++ "gp)"
        ++ ", and leaves happy, taking "
        ++ String.fromInt Clientele.constants.minTakenOnSuccess
        ++ " minutes."


rejectString : Clientele.Customer -> OfferInfo -> String
rejectString customer offer =
    "The customer, "
        ++ customer.name
        ++ ", rejected the sales price of "
        ++ String.fromInt offer.pcOffer
        ++ "gp for the "
        ++ offer.itemName
        ++ ", taking "
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
        , text
            ("Time: " ++ displayTime model.time)
        , div [] []
        , text
            ("Day: " ++ (String.fromInt <| dayOfYear model.time))
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
                , halfBlock <| customersBlockOpen model
                , halfBlock <| currentSituationBlockOpen model
                ]

        Closed ->
            blockOfBlocks
                [ halfBlock <| stockBlock model
                , halfBlock <| actionsBlockClosed
                , halfBlock <| customersBlockClosed
                , halfBlock <| currentSituationBlockClosed model
                ]


storeInfo : Model -> List (Html Msg)
storeInfo model =
    [ h2 [] [ text "Store" ]
    , div [] [ text ("Time: " ++ displayTime model.time) ]
    , div [] [ text ("Your gold: " ++ String.fromInt model.pcGold ++ "gp") ]
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
                    div []
                        [ div [ Attr.style "margin-bottom" halfThick ]
                            [ div []
                                [ basicButton [ onClick ResetPrice ] [ text "Reset" ]
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
                            , basicButton [ onClick SubmitOffer ]
                                [ text <| currentSituationString customer model.offerInfo
                                ]
                            ]
                        ]

        Wait ->
            div []
                [ basicButton [ onClick <| UpdateWaitTime <| String.fromInt 0 ] [ text "Reset" ]
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
                    [ text <| "Wait for " ++ String.fromInt model.waitTime ++ " minutes." ]
                ]

        PrepOpen ->
            div []
                [ text "The store must be closed for you to be able to open it."
                ]
    ]


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


currentSituationString : Clientele.Customer -> OfferInfo -> String
currentSituationString customer offerInfo =
    "Sell "
        ++ offerInfo.itemName
        ++ " (cost "
        ++ String.fromInt offerInfo.itemWorth
        ++ "gp) to "
        ++ customer.name
        ++ " for "
        ++ String.fromInt offerInfo.pcOffer
        ++ "gp."


basicButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
basicButton attributes messages =
    button (Attr.style "margin" "2px" :: attributes) messages


modifyOfferButton : Int -> Model -> Html Msg
modifyOfferButton offerDiff model =
    basicButton [ onClick <| PcOffer <| String.fromInt <| model.offerInfo.pcOffer + offerDiff ]
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
    basicButton [ onClick <| UpdateWaitTime <| String.fromInt <| model.waitTime + timeDiff ]
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
    , div [] [ text "Infinite swords" ]
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
    , button [ onClick ReverseStory ] [ text "Reverse Story" ]
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
