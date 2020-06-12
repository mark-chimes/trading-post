module Item exposing (Item, OfferInfo, PcOfferInfo, axe, cabbage, dagger, fancyChocolate, itemForName, itemsList, sword, trailMix)

import Dict
import ItemType


type alias Item =
    { uniqueName : String
    , displayName : String
    , itemWorth : Int
    , itemType : ItemType.ItemType
    }


type alias PcOfferInfo =
    { pcOffer : Int
    , maybeItem : Maybe Item
    }


type alias OfferInfo =
    { pcOffer : Int
    , item : Item
    }


sword : Item
sword =
    { uniqueName = "sword"
    , displayName = "sword"
    , itemWorth = 50
    , itemType = ItemType.weapon
    }


axe : Item
axe =
    { uniqueName = "axe"
    , displayName = "axe"
    , itemWorth = 20
    , itemType = ItemType.weapon
    }


dagger : Item
dagger =
    { uniqueName = "dagger"
    , displayName = "dagger"
    , itemWorth = 10
    , itemType = ItemType.weapon
    }


fancyChocolate : Item
fancyChocolate =
    { uniqueName = "fancyChocolate"
    , displayName = "fancy bar of chocolate"
    , itemWorth = 50
    , itemType = ItemType.food
    }


trailMix : Item
trailMix =
    { uniqueName = "trailMix"
    , displayName = "packet of trail mix"
    , itemWorth = 20
    , itemType = ItemType.food
    }


cabbage : Item
cabbage =
    { uniqueName = "cabbage"
    , displayName = "head of cabbage"
    , itemWorth = 10
    , itemType = ItemType.food
    }


itemsList : List Item
itemsList =
    [ sword, axe, dagger, fancyChocolate, trailMix, cabbage ]



-- TODO do with a Dict


itemsDict : Dict.Dict String Item
itemsDict =
    Dict.fromList
        [ ( "sword", sword )
        , ( "axe", axe )
        , ( "dagger", dagger )
        , ( "fancyChocolate", fancyChocolate )
        , ( "trailMix", trailMix )
        , ( "cabbage", cabbage )
        ]


itemForName : String -> Maybe Item
itemForName uniqueName =
    Dict.get uniqueName itemsDict
