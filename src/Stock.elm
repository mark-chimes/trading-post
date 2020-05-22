module Stock exposing (..)


type ItemType
    = ItemType String


itemTypeEnum : List String
itemTypeEnum =
    [ "weapon", "food" ]


weaponType : ItemType
weaponType =
    ItemType "weapon"


foodType : ItemType
foodType =
    ItemType "food"


toString : ItemType -> String
toString (ItemType string) =
    string


type alias Item =
    { uniqueName : String
    , displayName : String
    , itemWorth : Int
    , itemType : ItemType
    }


type alias PcOfferInfo =
    { pcOffer : Int
    , maybeItem : Maybe Item
    }


type alias OfferInfo =
    { pcOffer : Int
    , item : Item
    }


swordItem : Item
swordItem =
    { uniqueName = "sword"
    , displayName = "sword"
    , itemWorth = 20
    , itemType = weaponType
    }


axeItem : Item
axeItem =
    { uniqueName = "axe"
    , displayName = "axe"
    , itemWorth = 15
    , itemType = weaponType
    }


trailMixItem : Item
trailMixItem =
    { uniqueName = "trail mix"
    , displayName = "packet of trail mix"
    , itemWorth = 5
    , itemType = foodType
    }


itemForName : String -> Maybe Item
itemForName uniqueName =
    if uniqueName == "sword" then
        Just swordItem

    else if uniqueName == "axe" then
        Just axeItem

    else if uniqueName == "trail mix" then
        Just trailMixItem

    else
        Nothing
