module Main exposing (Model, Msg(..), hoursInDay, init, main, purchaseString, update, view)

import Browser
import Clientele
import Html exposing (Html, br, button, div, h1, h2, h3, h4, hr, img, input, li, text, textarea, ul)
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
    { pcOffer : Int
    , itemName : String
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
        , offerInfo = { pcOffer = 0, itemName = "sword", itemWorth = 20 }
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

        ModifyPcOffer increaseAmount ->
            ( modifyOffer model increaseAmount, Cmd.none )

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
    { info | pcOffer = max 0 (Maybe.withDefault info.pcOffer (String.toInt newOfferString)) }


modifyOffer : Model -> Int -> Model
modifyOffer model increaseAmount =
    { model | offerInfo = calculateModifiedOffer increaseAmount model.offerInfo }


calculateModifiedOffer : Int -> OfferInfo -> OfferInfo
calculateModifiedOffer increaseAmount offerInfo =
    { offerInfo | pcOffer = max 0 (offerInfo.pcOffer + increaseAmount) }



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
            updateConversationWithActionMessage "There is no customer in store to whom to submit that offer." model


updateConversationWithActionMessage : String -> Model -> Model
updateConversationWithActionMessage message model =
    { model | conversation = model.conversation ++ [ [ displayTime model.time, message, "" ] ] }


succeedOnSale : Clientele.Customer -> Model -> Model
succeedOnSale customer model =
    kickOutCurrentCustomer <|
        updateConversationWithActionMessage (offerAndPurchaseString customer model.offerInfo) <|
            updateGold <|
                updateTimeSuccess customer model


offerAndPurchaseString : Clientele.Customer -> OfferInfo -> String
offerAndPurchaseString customer offerInfo =
    offerString offerInfo ++ "\n" ++ purchaseString customer offerInfo


failOnSale : Clientele.Customer -> OfferInfo -> Model -> Model
failOnSale customer offer model =
    updateConversationWithActionMessage (rejectString customer offer) <| updateTimeFailure customer <| model


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
    { model | pcGold = model.pcGold + model.offerInfo.pcOffer }


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
        ++ String.fromInt customer.minTakenOnSuccess
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
        ++ String.fromInt customer.minTakenOnFail
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
            , Attr.style "margin-bottom" halfThick
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
            , Attr.style "margin-top" halfThick
            , Attr.style "margin-bottom" halfThick
            , Attr.style "margin-left" fullThick
            , Attr.style "margin-right" fullThick
            , Attr.style "padding-bottom" fullThick
            ]
            theHtml
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Trading Post" ]

        --       , h2 [] [ text "Debug" ]
        --      , text
        --         ("Customer Max Price: " ++ String.fromInt model.customers.customer.maxPrice)
        , topBlock <| storeInfo model
        , blockOfBlocks
            [ halfBlock <| stockBlock model
            , halfBlock <| actionsBlock model
            , halfBlock <| saleBlock model
            , halfBlock <| customersBlock model
            ]
        , br [] []
        , oneBlock <| storyBlock model
        ]


storeInfo : Model -> List (Html Msg)
storeInfo model =
    [ h2 [] [ text "Store" ]
    , div [] [ text ("Time: " ++ displayTime model.time) ]
    , div [] [ text ("Your gold: " ++ String.fromInt model.pcGold ++ "gp") ]
    ]


saleBlock : Model -> List (Html Msg)
saleBlock model =
    [ h4 [] [ text "Sale" ]
    , div [] [ text ("Selling item: " ++ model.offerInfo.itemName) ]
    , div [] [ text ("Your sales price: " ++ String.fromInt model.offerInfo.pcOffer ++ "gp") ]
    , br [] []
    , div []
        [ button [ onClick (ModifyPcOffer -100) ] [ text "-100" ]
        , button [ onClick (ModifyPcOffer -10) ] [ text "-10" ]
        , input [ Attr.type_ "number", Attr.min "0", Attr.max "50000", placeholder "Your Offer", value (String.fromInt model.offerInfo.pcOffer), onInput PcOffer ] []
        , button [ onClick (ModifyPcOffer 10) ] [ text "+10" ]
        , button [ Attr.style "margin" "5px", onClick (ModifyPcOffer 100) ] [ text "+100" ]
        ]
    , br [] []
    , button [ onClick SubmitOffer ] [ text "Offer sale" ]
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


actionsBlock : Model -> List (Html Msg)
actionsBlock model =
    [ h3 [] [ text "Actions" ]
    , div []
        [ button [ onClick SchmoozeCustomer ] [ text "Schmooze Customer" ]
        , button [ onClick KickOutCustomer ] [ text "Fuckk Off" ]
        , button [ onClick CleanStore ] [ text "Clean Store" ]
        ]
    ]


storyBlock : Model -> List (Html Msg)
storyBlock model =
    [ h3 [] [ text "The story thus far: " ]
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
