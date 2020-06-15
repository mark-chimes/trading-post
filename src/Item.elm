module Item exposing (..)

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
    , itemWorth = 60
    , itemType = ItemType.weapon
    }


shortsword : Item
shortsword =
    { uniqueName = "shortsword"
    , displayName = "shortsword"
    , itemWorth = 40
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
    , itemWorth = 15
    , itemType = ItemType.weapon
    }


club : Item
club =
    { uniqueName = "club"
    , displayName = "club"
    , itemWorth = 5
    , itemType = ItemType.weapon
    }


porterhouse : Item
porterhouse =
    { uniqueName = "porterhouse"
    , displayName = "porterhouse steak"
    , itemWorth = 50
    , itemType = ItemType.food
    }


fancyChocolate : Item
fancyChocolate =
    { uniqueName = "fancyChocolate"
    , displayName = "fancy bar of chocolate"
    , itemWorth = 30
    , itemType = ItemType.food
    }


trailMix : Item
trailMix =
    { uniqueName = "trailMix"
    , displayName = "packet of trail mix"
    , itemWorth = 20
    , itemType = ItemType.food
    }


vegetables : Item
vegetables =
    { uniqueName = "vegetables"
    , displayName = "packet of mixed vegetables"
    , itemWorth = 10
    , itemType = ItemType.food
    }


cabbage : Item
cabbage =
    { uniqueName = "cabbage"
    , displayName = "head of cabbage"
    , itemWorth = 3
    , itemType = ItemType.food
    }


itemsList : List Item
itemsList =
    Dict.values itemsDict



-- TODO do with a Dict


itemsDict : Dict.Dict String Item
itemsDict =
    Dict.fromList
        [ ( "sword", sword )
        , ( "shortsword", shortsword )
        , ( "axe", axe )
        , ( "dagger", dagger )
        , ( "club", club )
        , ( "porterhouse", porterhouse )
        , ( "fancyChocolate", fancyChocolate )
        , ( "trailMix", trailMix )
        , ( "vegetables", vegetables )
        , ( "cabbage", cabbage )
        ]


itemForName : String -> Maybe Item
itemForName uniqueName =
    Dict.get uniqueName itemsDict
