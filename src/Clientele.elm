module Clientele exposing (..)

import Html exposing (Attribute, Html, button, text)



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


type alias TimingConstants =
    { minTakenOnSuccess : Int
    , minTakenOnFail : Int
    , minTakenOnSchmooze : Int
    }


constants : TimingConstants
constants =
    { minTakenOnSuccess = 5
    , minTakenOnFail = 10
    , minTakenOnSchmooze = 20
    }



---- UPDATE ----


schmoozeCustomerMessage : Customer -> String
schmoozeCustomerMessage customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They are impressed and are willing to pay 50% more for the item. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes"

    else
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They snap at you that you've said that "
            ++ String.fromInt customer.maxSchmoozes
            ++ " times already. They seem annoyed. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes"


schmoozeCurrentCustomer : ClienteleDetails -> ClienteleDetails
schmoozeCurrentCustomer clientele =
    { clientele | currentCustomer = Maybe.map schmoozeCustomer clientele.currentCustomer }


kickOutCurrentCustomer : ClienteleDetails -> ClienteleDetails
kickOutCurrentCustomer clientele =
    { clientele | currentCustomer = Nothing }


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        { customer | maxPrice = customer.maxPrice + (customer.maxPrice // 2), schmoozeCount = customer.schmoozeCount + 1 }

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
            , maxPrice = 30
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        1 ->
            { name = "Bob"
            , maxPrice = 40
            , schmoozeCount = 0
            , maxSchmoozes = 2
            }

        2 ->
            { name = "Carol"
            , maxPrice = 50
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        3 ->
            { name = "Dennis"
            , maxPrice = 30
            , schmoozeCount = 0
            , maxSchmoozes = 1
            }

        4 ->
            { name = "Erica"
            , maxPrice = 25
            , schmoozeCount = 0
            , maxSchmoozes = 5
            }

        5 ->
            { name = "Frank"
            , maxPrice = 40
            , schmoozeCount = 0
            , maxSchmoozes = 3
            }

        _ ->
            { name = "Wrong"
            , maxPrice = 10
            , schmoozeCount = 0
            , maxSchmoozes = 1
            }


customerFuckOffMessage : ClienteleDetails -> String
customerFuckOffMessage clientele =
    case clientele.currentCustomer of
        Nothing ->
            "Who are you kicking out?"

        Just customer ->
            "You tell "
                ++ customer.name
                ++ " to fuck off. They get angry, and leave the store, taking "
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
        , waitingCustomers = switchCustomer clientele.waitingCustomers clientele.currentCustomer customer
    }


switchCustomer : List Customer -> Maybe Customer -> Customer -> List Customer
switchCustomer waitingCustomers maybeCurrentCustomer calledCustomer =
    List.filter (\x -> x /= calledCustomer) waitingCustomers
        ++ (case maybeCurrentCustomer of
                Just currentCustomer ->
                    [ currentCustomer ]

                Nothing ->
                    []
           )


addCustomer : ClienteleDetails -> Customer -> ClienteleDetails
addCustomer clientele customer =
    { clientele
        | waitingCustomers = calculateWaitingCustomers clientele.waitingCustomers clientele.currentCustomer customer clientele.customerIndex
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
