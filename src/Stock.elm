module Stock exposing (..)


type ItemType
    = ItemType String


itemTypeNamesEnum : List String
itemTypeNamesEnum =
    [ "weapon", "food" ]


itemTypesEnum : List ItemType
itemTypesEnum =
    List.map fromString itemTypeNamesEnum


weaponType : ItemType
weaponType =
    ItemType "weapon"


foodType : ItemType
foodType =
    ItemType "food"


toString : ItemType -> String
toString (ItemType string) =
    string


fromString : String -> ItemType
fromString string =
    ItemType string


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


daggerItem : Item
daggerItem =
    { uniqueName = "dagger"
    , displayName = "dagger"
    , itemWorth = 10
    , itemType = weaponType
    }


fancyChocolateItem : Item
fancyChocolateItem =
    { uniqueName = "fancy chocolate"
    , displayName = "fancy bar of chocolate"
    , itemWorth = 20
    , itemType = foodType
    }


trailMixItem : Item
trailMixItem =
    { uniqueName = "trail mix"
    , displayName = "packet of trail mix"
    , itemWorth = 10
    , itemType = foodType
    }


cabbageItem : Item
cabbageItem =
    { uniqueName = "cabbage"
    , displayName = "head of cabbage"
    , itemWorth = 4
    , itemType = foodType
    }


itemForName : String -> Maybe Item
itemForName uniqueName =
    if uniqueName == "sword" then
        Just swordItem

    else if uniqueName == "axe" then
        Just axeItem

    else if uniqueName == "dagger" then
        Just daggerItem

    else if uniqueName == "fancy chocolate" then
        Just fancyChocolateItem

    else if uniqueName == "trail mix" then
        Just trailMixItem

    else if uniqueName == "cabbage" then
        Just cabbageItem

    else
        Nothing
