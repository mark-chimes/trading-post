module Stock exposing (..)


type ItemType
    = WeaponType
    | FoodType


type alias Item =
    { itemName : String
    , itemWorth : Int
    , itemType : ItemType
    }


type alias OfferInfo =
    { pcOffer : Int
    , item : Item
    }


itemTypeName : ItemType -> String
itemTypeName itemType =
    case itemType of
        WeaponType ->
            "weapon"

        FoodType ->
            "food"


swordItem : Item
swordItem =
    { itemName = "sword"
    , itemWorth = 20
    , itemType = WeaponType
    }


trailMixItem : Item
trailMixItem =
    { itemName = "packet of trail mix"
    , itemWorth = 5
    , itemType = FoodType
    }
