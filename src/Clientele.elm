module Clientele exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, text)
import Html.Attributes as Attr
import Stock exposing (..)



---- MODEL ----
-- TODO Maybe the basket should be part of some offer class along with the offerInfo


type alias BasketInfo =
    List OfferInfo


type alias ClienteleDetails =
    { maxCustomers : Int
    , waitingCustomers : List Customer
    , customerPool : CustomerPool
    , currentCustomer : Maybe Customer
    , kickTime : Int
    }


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
    customerPool ++ [ resetCustomer customer ]


resetCustomer : Customer -> Customer
resetCustomer customer =
    { customer
        | moneyInPurse = calculateMoneyInPurse customer.wealthLevel
        , schmoozeCount = 0
        , basket = []
    }


initCustomers : ClienteleDetails
initCustomers =
    { maxCustomers = 6
    , waitingCustomers = initWaitingCustomers
    , customerPool = initCustomerPool
    , currentCustomer = Just initFirstCustomer
    , kickTime = 2
    }


type alias TimingConstants =
    { minTakenOnSpeakingTo : Int
    , minTakenOnSuccess : Int
    , minTakenOnFail : Int
    , minTakenOnSchmooze : Int
    , minTakenOnInspect : Int
    , maxSchmoozes : Int
    }


constants : TimingConstants
constants =
    { minTakenOnSpeakingTo = 1
    , minTakenOnSuccess = 5
    , minTakenOnFail = 10
    , minTakenOnSchmooze = 20
    , minTakenOnInspect = 10
    , maxSchmoozes = 3
    }



---- UPDATE ----


updateCurrentCustomerBasket : OfferInfo -> ClienteleDetails -> ClienteleDetails
updateCurrentCustomerBasket offerInfo clientele =
    { clientele
        | currentCustomer =
            Maybe.map (updateCustomerBasket offerInfo) clientele.currentCustomer
    }


updateCustomerBasket : OfferInfo -> Customer -> Customer
updateCustomerBasket offerInfo customer =
    { customer | basket = customer.basket ++ [ offerInfo ], numItemsInBasket = incrementCustomerNumItemsInBasket offerInfo.item.itemType customer.numItemsInBasket }


incrementCustomerNumItemsInBasket : ItemType -> (ItemType -> Int) -> (ItemType -> Int)
incrementCustomerNumItemsInBasket incrementedItem numItemsInBasket =
    \itemType ->
        if itemType == incrementedItem then
            numItemsInBasket itemType + 1

        else
            numItemsInBasket itemType


updateCurrentCustomerGold : Int -> ClienteleDetails -> ClienteleDetails
updateCurrentCustomerGold offer clientele =
    { clientele | currentCustomer = updateCustomerGold offer clientele.currentCustomer }


updateCustomerGold : Int -> Maybe Customer -> Maybe Customer
updateCustomerGold offer maybeCustomer =
    Maybe.map (\customer -> { customer | moneyInPurse = customer.moneyInPurse - offer }) maybeCustomer


schmoozeCustomerMessage : Customer -> String
schmoozeCustomerMessage customer =
    if customer.schmoozeCount < constants.maxSchmoozes then
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They are impressed and are willing to pay more for the item. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."

    else
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They snap at you that you've said that "
            ++ String.fromInt constants.maxSchmoozes
            ++ " times already. They seem annoyed. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."


inspectCustomerMessage : Customer -> String
inspectCustomerMessage customer =
    "You inspect "
        ++ customer.name
        ++ " for "
        ++ String.fromInt constants.minTakenOnInspect
        ++ " minutes. "
        ++ customer.descriptionMessage
        ++ " "
        ++ wealthMessageFromWealth customer.wealthLevel
        ++ " "
        ++ customer.template.description


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


exitAllCustomers : ClienteleDetails -> ClienteleDetails
exitAllCustomers clientele =
    (\clt ->
        { clt
            | waitingCustomers = []
            , customerPool = clt.customerPool ++ clt.waitingCustomers
        }
    )
    <|
        exitCurrentCustomer clientele


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < constants.maxSchmoozes then
        { customer | schmoozeCount = customer.schmoozeCount + 1 }

    else
        customer


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


callCustomerFromPool : ClienteleDetails -> ClienteleDetails
callCustomerFromPool clientele =
    let
        ( newCustomer, newCustomerPool ) =
            generateNextCustomer clientele.customerPool
    in
    { clientele
        | currentCustomer = Just newCustomer
        , customerPool = newCustomerPool
    }


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


markCurrentCustomerAsInspected : ClienteleDetails -> ClienteleDetails
markCurrentCustomerAsInspected clientele =
    { clientele
        | currentCustomer =
            case clientele.currentCustomer of
                Just customer ->
                    Just { customer | inspectedState = Inspected }

                Nothing ->
                    Nothing
    }



---- VIEW ----


customerEntryButtons :
    (Customer -> Attribute msg)
    -> ClienteleDetails
    -> List (Html msg)
customerEntryButtons command customers =
    List.map
        (\c ->
            if List.length c.basket > 0 then
                button
                    [ Attr.attribute "aria-label" (c.name ++ " (with items in basket) speak to")
                    , command c
                    ]
                    [ text <| c.name ++ " (*)" ]

            else
                button
                    [ Attr.attribute "aria-label" (c.name ++ " speak to")
                    , command c
                    ]
                    [ text c.name ]
        )
        customers.waitingCustomers



---- Customers ----


type WealthLevel
    = Destitute
    | Poor
    | Average
    | WellOff
    | Rich


type alias Customer =
    { name : String
    , wealthLevel : WealthLevel
    , moneyInPurse : Int
    , basket : BasketInfo
    , schmoozeCount : Int
    , introMessage : String
    , descriptionMessage : String
    , inspectedState : InspectedState
    , template : CustomerTemplate
    , numItemsInBasket : ItemType -> Int
    }


type InspectedState
    = Inspected
    | Uninspected


type alias CustomerInit =
    { name : String
    , wealthLevel : WealthLevel
    , introMessage : String
    , descriptionMessage : String
    , template : CustomerTemplate
    }


createCustomer : CustomerInit -> Customer
createCustomer ci =
    { name = ci.name
    , wealthLevel = ci.wealthLevel
    , moneyInPurse = calculateMoneyInPurse ci.wealthLevel
    , basket = []
    , schmoozeCount = 0
    , introMessage = ci.introMessage
    , descriptionMessage = ci.descriptionMessage
    , inspectedState = Uninspected
    , template = ci.template
    , numItemsInBasket = \_ -> 0
    }


calculateMoneyInPurse : WealthLevel -> Int
calculateMoneyInPurse wealthLevel =
    round (maxPriceFromWealth wealthLevel * 200)


maxPrice : Item -> Customer -> Int
maxPrice item customer =
    round (toFloat item.itemWorth * paymentForItemType item.itemType customer)


paymentForItemType : ItemType -> Customer -> Float
paymentForItemType itemType customer =
    customer.template.itemPreferences itemType * maxPriceFromWealth customer.wealthLevel * (1 + 0.5 * toFloat customer.schmoozeCount) * (1.0 / (1.0 + (toFloat <| customer.numItemsInBasket itemType)))


maxPriceFromWealth : WealthLevel -> Float
maxPriceFromWealth wealthLevel =
    case wealthLevel of
        Destitute ->
            0.8

        Poor ->
            1.0

        Average ->
            1.5

        WellOff ->
            2.0

        Rich ->
            3.0


wealthMessageFromWealth : WealthLevel -> String
wealthMessageFromWealth wealthLevel =
    wealthDescriptionFromWealth wealthLevel
        ++ "They'd probably pay about "
        ++ String.fromInt (round (100 * maxPriceFromWealth wealthLevel))
        ++ "% of the item's value without being schmoozed."


wealthDescriptionFromWealth : WealthLevel -> String
wealthDescriptionFromWealth wealthLevel =
    case wealthLevel of
        Destitute ->
            "They seem pretty much destitute. "

        Poor ->
            "They seem quite poor. "

        Average ->
            "They seem to be of average wealth. "

        WellOff ->
            "They seem to be quite well-off. "

        Rich ->
            "They appear to be rather wealthy. "


customerDisplay : Customer -> List String
customerDisplay customer =
    customer.name
        :: ("Schmoozed: "
                ++ String.fromInt customer.schmoozeCount
                ++ " times."
           )
        :: (case customer.inspectedState of
                Inspected ->
                    [ "Gold in purse: " ++ String.fromInt customer.moneyInPurse ++ "gp"
                    , " - Max prices - "

                    -- , "Base: " ++ String.fromInt (round (maxPriceFromWealth customer.wealthLevel * (1 + 0.5 * toFloat customer.schmoozeCount) * 100)) ++ "%"
                    , "Food: (" ++ String.fromInt (customer.numItemsInBasket Stock.foodType) ++ ") " ++ percentageForDisplay Stock.foodType customer
                    , "Weapons: (" ++ String.fromInt (customer.numItemsInBasket Stock.weaponType) ++ ") " ++ percentageForDisplay Stock.weaponType customer
                    ]

                Uninspected ->
                    [ "" ]
           )
        {- ++ [ "DEBUG"
           , "SwordItem: "
           , String.fromInt <| maxPrice swordItem customer
           , "item.itemWorth: "
           , String.fromInt <| swordItem.itemWorth
           , "paymentForItemType item.itemType customer: "
           , String.fromFloat <| paymentForItemType WeaponType customer
           , "customer.template.itemPreferences itemType"
           , String.fromFloat <| customer.template.itemPreferences WeaponType
           , "maxPriceFromWealth customer.wealthLevel"
           , String.fromFloat <| maxPriceFromWealth customer.wealthLevel
           , "(1 + 0.5 * toFloat customer.schmoozeCount)"
           , String.fromFloat <| 1 + 0.5 * toFloat customer.schmoozeCount
           ]
        -}
        ++ "-\n"
        :: (case customer.inspectedState of
                Inspected ->
                    [ customer.template.name
                    , customer.introMessage ++ " " ++ customer.descriptionMessage
                    , wealthDescriptionFromWealth customer.wealthLevel
                    ]

                Uninspected ->
                    [ customer.introMessage ]
           )


percentageForDisplay : Stock.ItemType -> Customer -> String
percentageForDisplay itemType customer =
    String.fromInt (round (paymentForItemType itemType customer * 100)) ++ "%"


defaultCustomer : Customer
defaultCustomer =
    createCustomer
        { name = "WRONG"
        , wealthLevel = Destitute
        , introMessage = "You sense a bizarre otherworldly presence."
        , descriptionMessage = "They seem wrong, somehow. Like something that shouldn't exist."
        , template = templateWeird
        }


initFirstCustomer : Customer
initFirstCustomer =
    createCustomer
        { name = "Abby Aubergine"
        , wealthLevel = Poor
        , introMessage = "She greets you with a smile."
        , descriptionMessage = "The smile was endearing at first, but it starts to get creepy after awhile."
        , template = templateTraveller
        }


initWaitingCustomers : List Customer
initWaitingCustomers =
    [ createCustomer
        { name = "Bob Bucket"
        , wealthLevel = Poor
        , introMessage = "He eyes your store shiftily."
        , descriptionMessage = "Sleazy looking guy. You'd be willing to sell to him, but probably shouldn't trust anything he sold you."
        , template = templateTraveller
        }
    ]


type alias CustomerPool =
    List Customer


type alias CustomerPoolInit =
    List CustomerInit


initCustomerPool : CustomerPool
initCustomerPool =
    List.map createCustomer initCustomerPoolInit


type alias ItemPreferences =
    ItemType -> Float


type alias CustomerTemplate =
    { name : String
    , description : String
    , itemPreferences : ItemPreferences
    }


templateKnight : CustomerTemplate
templateKnight =
    { name = "Knight"
    , description = "They are a knight! They'll pay normal price for food and double for weapons."
    , itemPreferences =
        \itemType ->
            if itemType == weaponType then
                2.0

            else if itemType == foodType then
                1.0

            else
                0.0
    }


templateTraveller : CustomerTemplate
templateTraveller =
    { name = "Traveller"
    , description = "They are a traveller! They'll pay half price for weapons and double for food."
    , itemPreferences =
        \itemType ->
            if itemType == weaponType then
                0.5

            else if itemType == foodType then
                2.0

            else
                0.0
    }


templateWeird : CustomerTemplate
templateWeird =
    { name = "Weird"
    , description = "They are weird! You can't tell what they want."
    , itemPreferences = \_ -> 0.0
    }


initCustomerPoolInit : CustomerPoolInit
initCustomerPoolInit =
    [ { name = "Carol Cooper-Iardlynoer"
      , wealthLevel = Average
      , introMessage = "She browses your product line."
      , descriptionMessage = "She has wonderful hair!"
      , template = templateTraveller
      }
    , { name = "Dennis Demacia"
      , wealthLevel = Poor
      , introMessage = "He keeps his hands in his pockets."
      , descriptionMessage = "Just a teenage dirtbag baby."
      , template = templateTraveller
      }
    , { name = "Erica Earful"
      , wealthLevel = Rich
      , introMessage = "She strides up to your counter confidently in full plate."
      , descriptionMessage = "An iron maiden."
      , template = templateKnight
      }
    , { name = "Frank Mann-Free"
      , wealthLevel = Average
      , introMessage = "He seems like he couldn't care less."
      , descriptionMessage = "Frankly, my dear, he doesn't give a damn."
      , template = templateKnight
      }
    , { name = "Gertrude Ganderstudies"
      , wealthLevel = Average
      , introMessage = "A prim and proper older lady."
      , descriptionMessage = "Her clothes are expensive, but well-worn - a wealthy woman who has fallen on harder times."
      , template = templateKnight
      }
    , { name = "Harold Harbinger"
      , wealthLevel = Rich
      , introMessage = "A dark and mysterious cloaked figure."
      , descriptionMessage = "He growls when he speaks, and strange jewellery flashes from under his cloak. He keeps muttering about broken swords and half-things."
      , template = templateKnight
      }
    , { name = "Ingrid Isntmael"
      , wealthLevel = WellOff
      , introMessage = "A middle-aged woman with surprisingly pointy ears."
      , descriptionMessage = "On closer inspection, the ears appear to be a form of costume jewellery."
      , template = templateKnight
      }
    , { name = "Jerome Jackinthebox"
      , wealthLevel = Poor
      , introMessage = "A an attractive, confident man who'll flirt with anyone in the store."
      , descriptionMessage = "It is quickly obvious the confidence is just a ruse to hide deep-seated insecurities."
      , template = templateKnight
      }
    , { name = "Kyla Killthemall"
      , wealthLevel = WellOff
      , introMessage = "Why is it so chilly in here all of a sudden?"
      , descriptionMessage = "She speaks in dark and gravelly voice that tends to make people uncomfortable."
      , template = templateKnight
      }
    , { name = "Liam Lemonmeringue"
      , wealthLevel = WellOff
      , introMessage = "An athletic figure."
      , descriptionMessage = "His clothes are tighter than they need to be."
      , template = templateKnight
      }
    , { name = "Marion Mansion"
      , wealthLevel = Rich
      , introMessage = "Very stylish and fashionable"
      , descriptionMessage = "Looking to make an impression."
      , template = templateTraveller
      }
    , { name = "Noddy Noboddy"
      , wealthLevel = Destitute
      , introMessage = "A beggar has made his way into your store."
      , descriptionMessage = "A friendly chap who manages to maintain a positive attitude despite his unfortunate conditions."
      , template = templateTraveller
      }
    , { name = "Olivia Oldbutgold"
      , wealthLevel = WellOff
      , introMessage = "A kindly looking old lady."
      , descriptionMessage = "She speaks at length about her grandchildren in the East."
      , template = templateTraveller
      }
    , { name = "Patrick Pleasepassthepepper"
      , wealthLevel = Average
      , introMessage = "A fragrant and spicy smell wafts through the store."
      , descriptionMessage = "He has numerous bags of colourful spices arrayed inside his jacket."
      , template = templateTraveller
      }
    , { name = "Quinette Qualityquilt"
      , wealthLevel = WellOff
      , introMessage = "A dappled and eye-catching array of clothing."
      , descriptionMessage = "Dressed thick and warm, as if expecting a cold winter."
      , template = templateKnight
      }
    , { name = "Rawry Ragna-Rock"
      , wealthLevel = Average
      , introMessage = "Creeping in with back hunched, eying the racks suspiciously."
      , descriptionMessage = "Actually quite friendly and well-mannered, just has a hunch back and a lazy eye."
      , template = templateKnight
      }
    , { name = "Samantha Saltoftheearth"
      , wealthLevel = Poor
      , introMessage = "A plump middle-aged woman."
      , descriptionMessage = "She keeps offering you to try some of her quadruple-ginger cookies."
      , template = templateTraveller
      }
    , { name = "Toby Tell-Noboddy"
      , wealthLevel = Poor
      , introMessage = "A quiet gentelman standing off on his own."
      , descriptionMessage = "His responses are brief and his mind appears to be elsewhere."
      , template = templateKnight
      }
    , { name = "Ursula Ur"
      , wealthLevel = Rich
      , introMessage = "A large and confident lady dressed in enormous robes of purple and strutting in with purpose."
      , descriptionMessage = "She gives the impression that she always gets what she wants, sooner or later..."
      , template = templateKnight
      }
    , { name = "Vaughn Vatofacid"
      , wealthLevel = Average
      , introMessage = "A burn mark scars his face."
      , descriptionMessage = "He tells all about his hobby as a beekeeper and offers to let you try some honey."
      , template = templateKnight
      }
    , { name = "Wendy Mann-Woo"
      , wealthLevel = Average
      , introMessage = "An attractive young woman."
      , descriptionMessage = "She's wearing quite a lot of makeup, and flirts with everyone in store."
      , template = templateTraveller
      }
    , { name = "Xavier Xtraspicy"
      , wealthLevel = Average
      , introMessage = "A big, burly man covered in tattoos"
      , descriptionMessage = "He seems incredibly eager to kill goblins."
      , template = templateTraveller
      }
    , { name = "Yennefer Yodalayheehoo"
      , wealthLevel = WellOff
      , introMessage = "Her muscles ripple as she walks."
      , descriptionMessage = "Her voice booms loudly, and yet remains pleasant to the ears."
      , template = templateKnight
      }
    , { name = "Zander Zoinkies"
      , wealthLevel = Poor
      , introMessage = "An attractive young man with a scraggly beard and reddish eyes."
      , descriptionMessage = "He speaks slowly and often seems to be a bit lost in his own world."
      , template = templateTraveller
      }
    ]
