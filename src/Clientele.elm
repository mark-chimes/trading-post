module Clientele exposing (..)

import Html exposing (Attribute, Html, button, text)



---- MODEL ----


type alias ClienteleDetails =
    { maxCustomers : Int
    , waitingCustomers : List Customer
    , customerPool : CustomerPool
    , currentCustomer : Maybe Customer
    , kickTime : Int
    }


type alias Customer =
    { name : String
    , maxPrice : Int
    , schmoozeCount : Int
    , maxSchmoozes : Int
    , introMessage : String
    , descriptionMessage : String
    }


type alias CustomerPool =
    List Customer


generateNextCustomer : CustomerPool -> ( Customer, CustomerPool )
generateNextCustomer customerPool =
    case customerPool of
        [] ->
            ( defaultCustomer, [] )

        x :: [] ->
            ( x, [] )

        x :: xs ->
            ( x, xs )


addCustomerToPool : CustomerPool -> Customer -> CustomerPool
addCustomerToPool customerPool customer =
    customerPool ++ [ customer ]


initCustomers : ClienteleDetails
initCustomers =
    { maxCustomers = 6
    , waitingCustomers = initWaitingCustomers
    , currentCustomer = Just initFirstCustomer
    , customerPool = initCustomerPool
    , kickTime = 2
    }


defaultCustomer : Customer
defaultCustomer =
    { name = "Wrong"
    , maxPrice = 10
    , schmoozeCount = 0
    , maxSchmoozes = 1
    , introMessage = "You sense a bizarre otherworldly presence."
    , descriptionMessage = "They seem wrong, somehow. Like something that shouldn't exist."
    }


initFirstCustomer : Customer
initFirstCustomer =
    { name = "Abby"
    , maxPrice = 30
    , schmoozeCount = 0
    , maxSchmoozes = 3
    , introMessage = "She greets you with a smile."
    , descriptionMessage = "Such a sweet young woman."
    }


initWaitingCustomers : List Customer
initWaitingCustomers =
    [ { name = "Bob"
      , maxPrice = 25
      , schmoozeCount = 0
      , maxSchmoozes = 2
      , introMessage = "He eyes your store shiftily."
      , descriptionMessage = "Sleazy looking guy."
      }
    ]


initCustomerPool : CustomerPool
initCustomerPool =
    [ { name = "Carol"
      , maxPrice = 40
      , schmoozeCount = 0
      , maxSchmoozes = 3
      , introMessage = "She browses your product line."
      , descriptionMessage = "She has wonderful hair!"
      }
    , { name = "Dennis"
      , maxPrice = 20
      , schmoozeCount = 0
      , maxSchmoozes = 1
      , introMessage = "He keeps his hands in his pockets."
      , descriptionMessage = "Just a teenage dirtbag, baby."
      }
    , { name = "Erica"
      , maxPrice = 50
      , schmoozeCount = 0
      , maxSchmoozes = 5
      , introMessage = "She strides up to your counter confidently in full plate."
      , descriptionMessage = "An iron maiden."
      }
    , { name = "Frank"
      , maxPrice = 40
      , schmoozeCount = 0
      , maxSchmoozes = 3
      , introMessage = "He seems like he couldn't care less."
      , descriptionMessage = "Frankly, my dear, I don't give a damn."
      }
    ]


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


exitCurrentCustomer : ClienteleDetails -> ClienteleDetails
exitCurrentCustomer clientele =
    case clientele.currentCustomer of
        Just exitingCustomer ->
            { clientele
                | currentCustomer = Nothing
                , customerPool = addCustomerToPool clientele.customerPool exitingCustomer
            }

        Nothing ->
            clientele


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < customer.maxSchmoozes then
        { customer | maxPrice = customer.maxPrice + (customer.maxPrice // 2), schmoozeCount = customer.schmoozeCount + 1 }

    else
        { customer | schmoozeCount = customer.schmoozeCount + 1 }


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
        ++ ". "
        ++ customer.introMessage


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


newWaitingCustomer : ClienteleDetails -> ClienteleDetails
newWaitingCustomer clientele =
    let
        ( newCustomer, newCustomerPool ) =
            generateNextCustomer clientele.customerPool
    in
    { clientele
        | waitingCustomers = clientele.waitingCustomers ++ [ newCustomer ]
        , customerPool = newCustomerPool
    }



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
