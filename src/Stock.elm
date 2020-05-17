module Stock exposing (..)


type alias Item =
    { itemName : String
    , itemWorth : Int
    }


type alias OfferInfo =
    { pcOffer : Int
    , item : Item
    }


type alias BasketInfo =
    List OfferInfo
