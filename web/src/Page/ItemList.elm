module Page.ItemList exposing (..)

import Data exposing (Item, itemDecoder)
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Views exposing (itemsTable)

type alias Model = 
    { session : Session
    , items : Maybe (List Item)
    }

type Msg 
    = GotItems ( Result Http.Error (List Item))

toSession : Model -> Session
toSession model = 
    model.session

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing
    , Cmd.batch
        [ fetchItems ]
    )

fetchItems : Cmd Msg
fetchItems =
    Http.get
        { url = "/items"
        , expect = Http.expectJson GotItems (Decode.list itemDecoder) }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItems result ->
            case result of
                Ok items -> ( { model | items = Just items }, Cmd.none )
                Err _ -> ( model, Cmd.none )

view : Model -> { title : String, content : Html Msg }
view model =
    let
        iTable = case model.items of
                        Nothing -> []
                        Just items -> [ itemsTable items ]
    in
    { title = "Items" 
    , content = div [ class "container" ] 
                    iTable
    }
