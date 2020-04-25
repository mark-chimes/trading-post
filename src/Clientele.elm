module Clientele exposing (..)

import Html exposing (Attribute, Html, button, text)
import Html.Attributes



---- MODEL ----


type alias Customers =
    { maxCustomers : Int
    , customerIndex : Int
    , waitingCustomers : List Customer
    , currentCustomer : Customer
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


initCustomers : Customers
initCustomers =
    { maxCustomers = 6
    , customerIndex = numWaitingCustomers
    , waitingCustomers = List.map (\n -> generateCustomer n) <| List.range 1 numWaitingCustomers
    , currentCustomer = generateCustomer 0
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


schmoozeCurrentCustomer : Customers -> Customers
schmoozeCurrentCustomer customers =
    { customers | currentCustomer = schmoozeCustomer customers.currentCustomer }


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        { customer | maxPrice = customer.maxPrice + (customer.maxPrice // 10), schmoozeCount = customer.schmoozeCount + 1 }

    else
        { customer | schmoozeCount = customer.schmoozeCount + 1 }


getNewCustomerIndex : Customers -> Int
getNewCustomerIndex customers =
    remainderBy customers.maxCustomers (customers.customerIndex + 1)


generateCustomer : Int -> Customer
generateCustomer index =
    case index of
        0 ->
            { name = "Susan"
            , maxPrice = 50
            , minTakenOnSuccess = 5
            , minTakenOnFail = 10
            , minTakenOnSchmooze = 20
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        1 ->
            { name = "Jeremy"
            , maxPrice = 60
            , minTakenOnSuccess = 5
            , minTakenOnFail = 15
            , minTakenOnSchmooze = 30
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        2 ->
            { name = "Samantha"
            , maxPrice = 30
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            , minTakenOnSchmooze = 10
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        3 ->
            { name = "Gertrude"
            , maxPrice = 80
            , minTakenOnSuccess = 5
            , minTakenOnFail = 20
            , minTakenOnSchmooze = 40
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        4 ->
            { name = "Samson"
            , maxPrice = 25
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            , minTakenOnSchmooze = 10
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        _ ->
            { name = "Pink"
            , maxPrice = 1200
            , minTakenOnSuccess = 5
            , minTakenOnFail = 60
            , minTakenOnSchmooze = 120
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }


customerKickOutMessage : Customers -> String
customerKickOutMessage customers =
    "You tell "
        ++ customers.currentCustomer.name
        ++ " to fuckk off. They leave in a huff taking "
        ++ String.fromInt customers.kickTime
        ++ " minutes"


customerEntryMessage : Customers -> String
customerEntryMessage customers =
    "A new customer called "
        ++ customers.currentCustomer.name
        ++ " enters the store."


callCustomer : Customers -> Customer -> Customers
callCustomer customers customer =
    { customers | currentCustomer = customer, waitingCustomers = updateCustomerList customers customer, customerIndex = getNewCustomerIndex customers }


updateCustomerList : Customers -> Customer -> List Customer
updateCustomerList customers customer =
    List.filter (\x -> x /= customer) customers.waitingCustomers ++ [ generateCustomer <| getNewCustomerIndex customers ]



---- VIEW ----


customerEntryButtons :
    (Customer -> Attribute msg)
    -> Customers
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
