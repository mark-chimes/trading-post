module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes as Attr exposing (placeholder, src, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { content : String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Change newContent ->
            ( { model | content = newContent }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App still working!" ]
        , input [ Attr.type_ "number", Attr.min "-50000", Attr.max "50000", placeholder "Value", value model.content, onInput Change ] []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
