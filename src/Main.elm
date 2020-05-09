module Main exposing (Model, Msg(..), calculateTimeOfNextCustomer, hoursInDay, incrementTimeWithMin, init, main, purchaseString, update, view)

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
        { time = 480
        , offerInfo = { pcOffer = 20, itemName = "sword", itemWorth = 20 }
        , pcGold = 0
        , cleanTime = 10
        , customers = Clientele.initCustomers
        , isConvoReverse = True
        , conversation = []
        , prepState = Sale
        , windowWidth = flags.windowWidth
        , waitTime = 0
        , timeOfNextCustomer = 540
        }
    , Cmd.none
    )



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


type PrepState
    = Clean
    | Kick
    | Schmooze
    | Inspect
    | Wait
    | Sale


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
    exitCurrentCustomer <|
        updateGold <|
            updateConversationWithActionMessage (offerAndPurchaseString customer model.offerInfo) <|
                incrementTimeWithMin Clientele.constants.minTakenOnSuccess <|
                    model


failOnSale : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSale customer offer model =
    updateConversationWithActionMessage (rejectString customer offer) <| incrementTimeWithMin Clientele.constants.minTakenOnFail <| model


fuckOffCustomer : Model -> Model
fuckOffCustomer model =
    updateConversationWithActionMessage (Clientele.customerFuckOffMessage model.customers) <| exitCurrentCustomer <| updateTimeFuckOff <| model


cleanStore : Model -> Model
cleanStore model =
    updateConversationWithActionMessage (cleanStoreMessage model.cleanTime) <| incrementTimeWithMin model.cleanTime <| model


waitAwhile : Model -> Model
waitAwhile model =
    updateConversationWithActionMessage (waitAwhileMessage model.waitTime) <| incrementTimeWithMin model.waitTime <| model


schmoozeCustomer : Model -> Model
schmoozeCustomer model =
    case model.customers.currentCustomer of
        Just customer ->
            (\mdl -> { mdl | customers = Clientele.schmoozeCurrentCustomer mdl.customers }) <|
                updateConversationWithActionMessage (Clientele.schmoozeCustomerMessage customer) <|
                    incrementTimeWithMin Clientele.constants.minTakenOnSchmooze <|
                        model

        Nothing ->
            updateConversationWithActionMessage "Who are you trying to schmooze?" model


inspectCustomer : Model -> Model
inspectCustomer model =
    case model.customers.currentCustomer of
        Just customer ->
            updateConversationWithActionMessage (Clientele.inspectCustomerMessage customer) <|
                incrementTimeWithMin Clientele.constants.minTakenOnInspect <|
                    model

        Nothing ->
            updateConversationWithActionMessage "Who are you trying to inspeect?" model


callNextCustomer : Clientele.Customer -> Model -> Model
callNextCustomer customer model =
    updateConversationWithActionMessage (Clientele.customerCallMessage customer) <|
        { model | customers = Clientele.callCustomer model.customers customer }


updateTimeFuckOff : Model -> Model
updateTimeFuckOff model =
    case model.customers.currentCustomer of
        Just _ ->
            incrementTimeWithMin model.customers.kickTime model

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


incrementTimeWithMin : Int -> Model -> Model
incrementTimeWithMin mins model =
    let
        newtime =
            mins + model.time

        ( lastTimeOfNextCustomer, newClienteleDetails ) =
            addCustomers newtime model.timeOfNextCustomer model.customers
    in
    { model
        | time = newtime
        , timeOfNextCustomer = lastTimeOfNextCustomer
        , customers = newClienteleDetails
    }


timeBetweenCustomersMins : Int
timeBetweenCustomersMins =
    60


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

        --        , h2 []
        --            [ text "Debug" ]
        , text
            ("Time of next customer " ++ displayTime model.timeOfNextCustomer)
        , div [] []
        , text
            ("Time: " ++ displayTime model.time)
        , topBlock <| storeInfo model
        , blockOfBlocks
            [ halfBlock <| stockBlock model
            , halfBlock <| actionsBlock
            , halfBlock <| customersBlock model
            , halfBlock <| currentSituationBlock model
            ]
        , oneBlock <| storyBlock model
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


currentSituationBlock : Model -> List (Html Msg)
currentSituationBlock model =
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


customersBlock : Model -> List (Html Msg)
customersBlock model =
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


actionsBlock : List (Html Msg)
actionsBlock =
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
