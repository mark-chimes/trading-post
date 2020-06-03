module ItemType exposing (ItemType(..), food, fromString, itemTypeNamesEnum, itemTypesEnum, toString, weapon)


type ItemType
    = ItemType String


itemTypeNamesEnum : List String
itemTypeNamesEnum =
    [ "weapon", "food" ]


itemTypesEnum : List ItemType
itemTypesEnum =
    List.map fromString itemTypeNamesEnum


weapon : ItemType
weapon =
    ItemType "weapon"


food : ItemType
food =
    ItemType "food"


toString : ItemType -> String
toString (ItemType string) =
    string


fromString : String -> ItemType
fromString string =
    ItemType string
