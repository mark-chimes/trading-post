module Clientele exposing (Customer, Customers, generateCustomer, incrementCustomer, initCustomers, updateCustomers)

---- MODEL ----


type alias Customers =
    { maxCustomers : Int
    , customerIndex : Int
    , customer : Customer
    }


type alias Customer =
    { name : String
    , maxPrice : Int
    , minTakenOnSuccess : Int
    , minTakenOnFail : Int
    }


initCustomers : Customers
initCustomers =
    { maxCustomers = 6
    , customerIndex = 0
    , customer = generateCustomer 0
    }


updateCustomers : Customers -> Customers
updateCustomers customers =
    { customers | customer = generateCustomer <| getNewCustomerIndex customers, customerIndex = getNewCustomerIndex customers }


getNewCustomerIndex : Customers -> Int
getNewCustomerIndex customers =
    remainderBy customers.maxCustomers (customers.customerIndex + 1)


incrementCustomer : Customers -> Customers
incrementCustomer customers =
    { customers | customer = generateCustomer <| getNewCustomerIndex customers, customerIndex = getNewCustomerIndex customers }


generateCustomer : Int -> Customer
generateCustomer index =
    case index of
        0 ->
            { name = "Susan"
            , maxPrice = 50
            , minTakenOnSuccess = 5
            , minTakenOnFail = 10
            }

        1 ->
            { name = "Jeremy"
            , maxPrice = 60
            , minTakenOnSuccess = 5
            , minTakenOnFail = 15
            }

        2 ->
            { name = "Samantha"
            , maxPrice = 30
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            }

        3 ->
            { name = "Gertrude"
            , maxPrice = 80
            , minTakenOnSuccess = 5
            , minTakenOnFail = 20
            }

        4 ->
            { name = "Samson"
            , maxPrice = 25
            , minTakenOnSuccess = 5
            , minTakenOnFail = 5
            }

        _ ->
            { name = "Pink"
            , maxPrice = 1200
            , minTakenOnSuccess = 5
            , minTakenOnFail = 60
            }
