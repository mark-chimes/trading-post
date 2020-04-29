module Clientele exposing (..)

import Html exposing (Attribute, Html, button, text)
import Html.Attributes



---- MODEL ----


type alias ClienteleDetails =
    { maxCustomers : Int
    , customerIndex : Int
    , waitingCustomers : List Customer
    , currentCustomer : Maybe Customer
    , kickTime : Int
    }


type alias Customer =
    { name : String
    , maxPrice : Int
    , minTakenOnSuccess : Int
    , minTakenOnFail : Int
    , minTakenOnSchmooze : Int
    , schmoozeCount : Int
    , maxSchmoozes : Int
    }


numWaitingCustomers : Int
numWaitingCustomers =
    2


initCustomers : ClienteleDetails
initCustomers =
    { maxCustomers = 6
    , customerIndex = numWaitingCustomers
    , waitingCustomers = List.map (\n -> generateCustomer n) <| List.range 1 numWaitingCustomers
    , currentCustomer = Just (generateCustomer 0)
    , kickTime = 2
    }



---- UPDATE ----


schmoozeCustomerMessage : Customer -> String
schmoozeCustomerMessage customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They are impressed and are willing to pay 10% more for the item. This takes "
            ++ String.fromInt customer.minTakenOnSchmooze
            ++ " minutes"

    else
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They snap at you that you've said that "
            ++ String.fromInt customer.maxSchmoozes
            ++ " times already. They seem annoyed. This takes "
            ++ String.fromInt customer.minTakenOnSchmooze
            ++ " minutes"


schmoozeCurrentCustomer : ClienteleDetails -> ClienteleDetails
schmoozeCurrentCustomer clientele =
    { clientele | currentCustomer = Maybe.map schmoozeCustomer clientele.currentCustomer }


kickOutCurrentCustomer : ClienteleDetails -> ClienteleDetails
kickOutCurrentCustomer clientele =
    { clientele | currentCustomer = Nothing, customerIndex = getNewCustomerIndex clientele }


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        { customer | maxPrice = customer.maxPrice + (customer.maxPrice // 10), schmoozeCount = customer.schmoozeCount + 1 }

    else
        { customer | schmoozeCount = customer.schmoozeCount + 1 }


getNewCustomerIndex : ClienteleDetails -> Int
getNewCustomerIndex clientele =
    remainderBy clientele.maxCustomers (clientele.customerIndex + 1)


generateCustomer : Int -> Customer
generateCustomer index =
    case index of
        0 ->
            { name = "Abby"
            , maxPrice = 50
            , minTakenOnSuccess = 5
            , minTakenOnFail = 10
            , minTakenOnSchmooze = 20
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        1 ->
            { name = "Bob"
            , maxPrice = 60
            , minTakenOnSuccess = 5
            , minTakenOnFail = 15
            , minTakenOnSchmooze = 30
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        2 ->
            { name = "Carol"
            , maxPrice = 30
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            , minTakenOnSchmooze = 10
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        3 ->
            { name = "Dennis"
            , maxPrice = 80
            , minTakenOnSuccess = 5
            , minTakenOnFail = 20
            , minTakenOnSchmooze = 40
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        4 ->
            { name = "Erica"
            , maxPrice = 25
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            , minTakenOnSchmooze = 10
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        _ ->
            { name = "Frank"
            , maxPrice = 1200
            , minTakenOnSuccess = 5
            , minTakenOnFail = 60
            , minTakenOnSchmooze = 120
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }


customerFuckOffMessage : ClienteleDetails -> String
customerFuckOffMessage clientele =
    case clientele.currentCustomer of
        Nothing ->
            "Who are you telling to fuckk off?"

        Just customer ->
            "You tell "
                ++ customer.name
                ++ " to fuckk off. They get angry, and leave the store, taking "
                ++ String.fromInt clientele.kickTime
                ++ " minutes."


customerCallMessage : Customer -> String
customerCallMessage customer =
    "You begin speaking to a customer named "
        ++ customer.name
        ++ "."


callCustomer : ClienteleDetails -> Customer -> ClienteleDetails
callCustomer clientele customer =
    { clientele
        | currentCustomer = Just customer
        , waitingCustomers = calculateWaitingCustomers clientele.waitingCustomers clientele.currentCustomer customer clientele.customerIndex
    }


calculateWaitingCustomers : List Customer -> Maybe Customer -> Customer -> Int -> List Customer
calculateWaitingCustomers waitingCustomers maybeCurrentCustomer calledCustomer newCustomerIndex =
    List.filter (\x -> x /= calledCustomer) waitingCustomers
        ++ (case maybeCurrentCustomer of
                Just currentCustomer ->
                    [ currentCustomer ]

                Nothing ->
                    [ generateCustomer newCustomerIndex ]
           )



---- VIEW ----


customerEntryButtons :
    (Customer -> Attribute msg)
    -> ClienteleDetails
    -> List (Html msg)
customerEntryButtons command customers =
    List.map
        (\c ->
            button
                [ command c
                ]
                [ text c.name ]
        )
        customers.waitingCustomers
