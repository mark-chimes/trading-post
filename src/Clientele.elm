module Clientele exposing (..)

import Html exposing (Attribute, Html, button, text)
import Html.Attributes as Attr
import Item exposing (Item)
import ItemType exposing (ItemType)



---- MODEL ----
-- TODO Maybe the basket should be part of some offer class along with the offerInfo


type alias BasketInfo =
    List Item.OfferInfo


type alias ClienteleDetails =
    { playerName : String
    , storeName : String
    , maxCustomers : Int
    , waitingCustomers : List Customer
    , customerPool : CustomerPool
    , currentCustomer : Maybe Customer
    , kickTime : Int
    }


generateNextCustomer : String -> String -> CustomerPool -> ( Customer, CustomerPool )
generateNextCustomer playerName storeName customerPool =
    case customerPool of
        [] ->
            ( defaultCustomer playerName storeName, [] )

        x :: [] ->
            ( x, [] )

        x :: xs ->
            ( x, xs )


addCustomerToPool : String -> String -> CustomerPool -> Customer -> CustomerPool
addCustomerToPool playerName storeName customerPool customer =
    customerPool ++ [ resetCustomer playerName storeName customer ]


resetCustomer : String -> String -> Customer -> Customer
resetCustomer playerName storeName customer =
    { customer
        | moneyInPurse = calculateMoneyInPurse customer.wealthLevel
        , schmoozeCount = 0
        , basket = []
        , conversation = resetConversation playerName storeName customer.name
    }


resetConversation : String -> String -> String -> List ( String, String )
resetConversation playerName storeName name =
    [ ( "Hello, and welcome to " ++ storeName ++ "! What's your name?", "My name is " ++ name ++ ", what's yours?" )
    , ( "Why, good to meet you " ++ name ++ ", I am " ++ playerName ++ ". And what brings you here today? "
      , "Thank you! I'd like to buy some items. What have you got for me?"
      )
    ]


initCustomers : String -> String -> ClienteleDetails
initCustomers playerName storeName =
    { maxCustomers = 6
    , playerName = playerName
    , storeName = storeName
    , waitingCustomers = initWaitingCustomers playerName storeName
    , customerPool = initCustomerPool playerName storeName
    , currentCustomer = Just <| initFirstCustomer playerName storeName
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


updateCurrentCustomerBasket : Item.OfferInfo -> ClienteleDetails -> ClienteleDetails
updateCurrentCustomerBasket offerInfo clientele =
    { clientele
        | currentCustomer =
            Maybe.map (updateCustomerBasket offerInfo) clientele.currentCustomer
    }


updateCustomerBasket : Item.OfferInfo -> Customer -> Customer
updateCustomerBasket offerInfo customer =
    { customer
        | basket = customer.basket ++ [ offerInfo ]
        , conversation = customer.conversation ++ [ customerSaleConvoMessage offerInfo ]
    }


customerSaleConvoMessage : Item.OfferInfo -> ( String, String )
customerSaleConvoMessage offerInfo =
    ( "One " ++ offerInfo.item.displayName ++ "! For you, only " ++ String.fromInt offerInfo.pcOffer ++ " gold!"
    , "A " ++ offerInfo.item.displayName ++ " for " ++ String.fromInt offerInfo.pcOffer ++ " sounds like a good deal."
    )


updateCurrentCustomerGold : Int -> ClienteleDetails -> ClienteleDetails
updateCurrentCustomerGold offer clientele =
    { clientele | currentCustomer = updateCustomerGold offer clientele.currentCustomer }


updateCustomerGold : Int -> Maybe Customer -> Maybe Customer
updateCustomerGold offer maybeCustomer =
    Maybe.map (\customer -> { customer | moneyInPurse = customer.moneyInPurse - offer }) maybeCustomer


schmoozeCustomerMessage : Customer -> String
schmoozeCustomerMessage customer =
    if customer.schmoozeCount >= constants.maxSchmoozes then
        "Before you give "
            ++ customer.name
            ++ " another compliment, they snap that "
            ++ String.fromInt constants.maxSchmoozes
            ++ " compliments is enough already and that you should get to the point. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."

    else if customer.schmoozeCount == 0 then
        "You tell "
            ++ customer.name
            ++ " that they have lovely hair. They are impressed and are willing to pay more for the item. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."

    else if customer.schmoozeCount == 1 then
        "You admire "
            ++ customer.name
            ++ "'s clothing. They blush and are willing to pay more for the item. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."

    else if customer.schmoozeCount == 2 then
        "You tell "
            ++ customer.name
            ++ " that their voice is like a song carried on the wind. The flutter their eyelashes and are willing to pay more for the item. This takes "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes."

    else
        "You give "
            ++ customer.name
            ++ " a generic compliment, taking "
            ++ String.fromInt constants.minTakenOnSchmooze
            ++ " minutes to do so."


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


exitCurrentCustomer : String -> String -> ClienteleDetails -> ClienteleDetails
exitCurrentCustomer playerName storeName clientele =
    case clientele.currentCustomer of
        Just exitingCustomer ->
            { clientele
                | currentCustomer = Nothing
                , customerPool = addCustomerToPool playerName storeName clientele.customerPool exitingCustomer
            }

        Nothing ->
            clientele


exitAllCustomers : String -> String -> ClienteleDetails -> ClienteleDetails
exitAllCustomers playerName storeName clientele =
    (\clt ->
        { clt
            | waitingCustomers = []
            , customerPool = clt.customerPool ++ clt.waitingCustomers
        }
    )
    <|
        exitCurrentCustomer playerName storeName clientele


schmoozeCustomer : Customer -> Customer
schmoozeCustomer customer =
    if customer.schmoozeCount < constants.maxSchmoozes then
        { customer
            | schmoozeCount = customer.schmoozeCount + 1
            , conversation = updateConvoWithSchmooze customer.schmoozeCount customer.conversation
        }

    else
        customer


updateConvoWithSchmooze : Int -> List ( String, String ) -> List ( String, String )
updateConvoWithSchmooze schmoozeCount oldCovo =
    case schmoozeCount of
        0 ->
            oldCovo ++ [ ( "Your hair is really lovely, you know!", "Oh, why thank you!" ) ]

        1 ->
            oldCovo ++ [ ( "Those garments of yours are rather fine!", "Haha, flattery will get you nowhere, you know!" ) ]

        2 ->
            oldCovo ++ [ ( "Your voice is like a song carried on the wind!", "*Flutters Eyelashes* Oh but you are such a charmer!" ) ]

        _ ->
            oldCovo ++ [ ( "Gives generic compliment.", "Did you just literally say \"gives generic compliment\"? That's a bit lazy." ) ]


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
            generateNextCustomer clientele.playerName clientele.storeName clientele.customerPool
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
            generateNextCustomer clientele.playerName clientele.storeName clientele.customerPool
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
    -> WealthLevel
    -> ClienteleDetails
    -> List (Html msg)
customerEntryButtons command wealthLevel customers =
    List.map
        (\c ->
            case customers.currentCustomer of
                Just currentCustomer ->
                    currentCustomerButton currentCustomer command c

                Nothing ->
                    nonCurrentCustomerButton command c
        )
    <|
        List.sortBy
            (\c -> c.name)
        <|
            List.filter (\c -> c.wealthLevel == wealthLevel) <|
                customers.waitingCustomers
                    ++ (case customers.currentCustomer of
                            Just cust ->
                                [ cust ]

                            Nothing ->
                                []
                       )


currentCustomerButton : Customer -> (Customer -> Attribute msg) -> Customer -> Html msg
currentCustomerButton currentCustomer command customer =
    if currentCustomer == customer then
        if List.length customer.basket > 0 then
            button
                [ Attr.disabled True
                , Attr.attribute "aria-label" (customer.name ++ " currently spoken to (with items in basket) ")
                ]
                [ text <| customer.name ++ " (*)" ]

        else
            button
                [ Attr.disabled True
                , Attr.attribute "aria-label" (customer.name ++ " currently spoken to")
                ]
                [ text customer.name ]

    else
        nonCurrentCustomerButton command customer


nonCurrentCustomerButton : (Customer -> Attribute msg) -> Customer -> Html msg
nonCurrentCustomerButton command customer =
    if List.length customer.basket > 0 then
        button
            [ Attr.attribute "aria-label" (customer.name ++ " speak to (with items in basket) ")
            , command customer
            ]
            [ text <| customer.name ++ " (*)" ]

    else
        button
            [ Attr.attribute "aria-label" (customer.name ++ " speak to")
            , command customer
            ]
            [ text customer.name ]



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
    , conversation : List ( String, String )
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


createCustomer : String -> String -> CustomerInit -> Customer
createCustomer playerName storeName ci =
    { name = ci.name
    , wealthLevel = ci.wealthLevel
    , moneyInPurse = calculateMoneyInPurse ci.wealthLevel
    , basket = []
    , schmoozeCount = 0
    , introMessage = ci.introMessage
    , descriptionMessage = ci.descriptionMessage
    , inspectedState = Inspected
    , template = ci.template
    , conversation = resetConversation playerName storeName ci.name
    }


calculateMoneyInPurse : WealthLevel -> Int
calculateMoneyInPurse wealthLevel =
    priceCapFromWealth wealthLevel * 20


optimalPrice : Item -> Customer -> Int
optimalPrice item customer =
    min customer.moneyInPurse <| maxPrice item customer


maxPrice : Item -> Customer -> Int
maxPrice item customer =
    round <| min (priceCapForItemType item.itemType customer) (toFloat item.itemWorth * paymentForItemType item.itemType customer)


paymentForItemType : ItemType -> Customer -> Float
paymentForItemType itemType customer =
    customer.template.itemNeed itemType
        * priceMultiplierFromWealth customer.wealthLevel
        * (1 + 0.1 * toFloat customer.schmoozeCount)
        * (1.0 / (1.0 + 0.25 * (toFloat <| numItemsInBasket itemType customer)))


numItemsInBasket : ItemType -> Customer -> Int
numItemsInBasket itemType customer =
    List.length <|
        List.filter (\i -> i.item.itemType == itemType) customer.basket



-- TODO


priceCapForItemType : ItemType -> Customer -> Float
priceCapForItemType itemType customer =
    (1.0 + 0.2 * toFloat customer.schmoozeCount) * toFloat (priceCapFromWealth customer.wealthLevel * customer.template.itemQualityDesire itemType)


priceMultiplierFromWealth : WealthLevel -> Float
priceMultiplierFromWealth wealthLevel =
    case wealthLevel of
        Destitute ->
            0.8

        Poor ->
            1.0

        Average ->
            1.2

        WellOff ->
            1.5

        Rich ->
            2.0


priceCapFromWealth : WealthLevel -> Int
priceCapFromWealth wealthLevel =
    case wealthLevel of
        Destitute ->
            1

        Poor ->
            2

        Average ->
            4

        WellOff ->
            8

        Rich ->
            16


mainInfo : Customer -> String
mainInfo customer =
    (case customer.wealthLevel of
        Destitute ->
            "Destitute "

        Poor ->
            "Poor "

        Average ->
            "Average "

        WellOff ->
            "Well-off "

        Rich ->
            "Rich "
    )
        ++ customer.template.name


wealthMessageFromWealth : WealthLevel -> String
wealthMessageFromWealth wealthLevel =
    wealthDescriptionFromWealth wealthLevel
        ++ "They'd probably pay about "
        ++ String.fromInt (round (100 * priceMultiplierFromWealth wealthLevel))
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
    [ "Gold in purse: " ++ String.fromInt customer.moneyInPurse ++ "gp"
    , customer.introMessage ++ " " ++ customer.descriptionMessage
    , wealthDescriptionFromWealth customer.wealthLevel
    ]


percentageForDisplay : ItemType -> Customer -> String
percentageForDisplay itemType customer =
    String.fromInt (round (paymentForItemType itemType customer * 100)) ++ "%"


defaultCustomer : String -> String -> Customer
defaultCustomer playerName storeName =
    createCustomer playerName
        storeName
        { name = "WRONG"
        , wealthLevel = Destitute
        , introMessage = "You sense a bizarre otherworldly presence."
        , descriptionMessage = "They seem wrong, somehow. Like something that shouldn't exist."
        , template = templateWeird
        }


initFirstCustomer : String -> String -> Customer
initFirstCustomer playerName storeName =
    createCustomer playerName
        storeName
        { name = "Abby Aubergine"
        , wealthLevel = Poor
        , introMessage = "She greets you with a smile."
        , descriptionMessage = "The smile was endearing at first, but it starts to get creepy after awhile."
        , template = templateTraveller
        }


initWaitingCustomers : String -> String -> List Customer
initWaitingCustomers playerName storeName =
    [ createCustomer playerName
        storeName
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


initCustomerPool : String -> String -> CustomerPool
initCustomerPool playerName storeName =
    List.map (createCustomer playerName storeName) initCustomerPoolInit


type alias ItemPreferences =
    ItemType -> Float


type alias BasePriceByItemType =
    ItemType -> Int


type alias CustomerTemplate =
    { name : String
    , description : String
    , itemNeed : ItemPreferences
    , itemQualityDesire : BasePriceByItemType
    }



-- itemNeed affects their willingness to pay more than the item's base worth
-- itemQualityDesire affects their willingness to buy a more expensive item of that type (price cap)


templateKnight : CustomerTemplate
templateKnight =
    { name = "Knight"
    , description = "They are a knight! They're really on the lookout for weapon items but might also buy some food."
    , itemNeed =
        \itemType ->
            if itemType == ItemType.weapon then
                1.5

            else if itemType == ItemType.food then
                1.2

            else
                0.0
    , itemQualityDesire =
        \itemType ->
            if itemType == ItemType.weapon then
                5

            else if itemType == ItemType.food then
                2

            else
                0
    }


templateTraveller : CustomerTemplate
templateTraveller =
    { name = "Traveller"
    , description = "They are a traveller! They're looking for food but might need some weapons for the road."
    , itemNeed =
        \itemType ->
            if itemType == ItemType.weapon then
                1.0

            else if itemType == ItemType.food then
                1.8

            else
                0.0
    , itemQualityDesire =
        \itemType ->
            if itemType == ItemType.weapon then
                2

            else if itemType == ItemType.food then
                5

            else
                0
    }


templateWeird : CustomerTemplate
templateWeird =
    { name = "Weird"
    , description = "They are weird! You can't tell what they want."
    , itemNeed = \_ -> 0.0
    , itemQualityDesire = \_ -> 0
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
      , template = templateTraveller
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
