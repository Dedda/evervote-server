module Page.ItemList exposing (..)

import Data exposing (Item, itemDecoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Session exposing (Session)
import Views exposing (itemsTable)

type alias NewItem =
    { title : String
    , description : String
    }

type alias Model = 
    { session : Session
    , items : Maybe (List Item)
    , newItem : NewItem
    }

type Msg 
    = GotItems ( Result Http.Error (List Item))
    | PostNewItem
    | PostedItem ( Result Http.Error () )
    | NewItemTitleChanged String
    | NewItemDescChanged String

toSession : Model -> Session
toSession model = model.session

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing emptyNewItem
    , Cmd.batch
        [ fetchItems ]
    )

emptyNewItem : NewItem
emptyNewItem =
    { title = ""
    , description = ""
    }

fetchItems : Cmd Msg
fetchItems =
    Http.get
        { url = "/items"
        , expect = Http.expectJson GotItems (Decode.list itemDecoder)
        }

postItem : NewItem -> Cmd Msg
postItem newItem =
    Http.post
        { url = "/items"
        , body = Http.jsonBody (postRequestEncoder newItem)
        , expect = Http.expectWhatever PostedItem
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItems result ->
            case result of
                Ok items -> ( { model | items = Just items }, Cmd.none )
                Err _ -> ( model, Cmd.none )
        PostNewItem ->
            ( model, postItem model.newItem )
        PostedItem result ->
            case result of
                Ok _ -> ( { model | newItem = emptyNewItem }, fetchItems )
                Err _ -> ( model, Cmd.none )
        NewItemTitleChanged newTitle ->
            let
                newItem = model.newItem
            in
            ( { model | newItem = { newItem | title = newTitle } }, Cmd.none )
        NewItemDescChanged newDesc ->
            let
                newItem = model.newItem
            in
            ( { model | newItem = { newItem | description = newDesc } }, Cmd.none )

view : Model -> { title : String, content : Html Msg }
view model =
    let
        iTable = case model.items of
                        Nothing -> []
                        Just items -> [ itemsTable items ]
    in
    { title = "Items" 
    , content = div [ class "container" ] 
                    [ div [ class "row" ] [ newItemForm model ]
                    , div [ class "row" ] iTable
                    ]
    }

newItemForm : Model -> Html Msg
newItemForm model =
    div []
        [ label [ for "newTitle" ] [ text "Title" ]
        , input [ id "newTitle", type_ "text", value model.newItem.title, onInput NewItemTitleChanged ] []
        , label [ for "newDesc" ] [ text "Description" ]
        , input [ id "newDesc", type_ "text", value model.newItem.description, onInput NewItemDescChanged ] []
        , button [ onClick PostNewItem ] [ text "Save" ]
        ]

-- HELPERS

postRequestEncoder : NewItem -> Encode.Value
postRequestEncoder newItem =
    Encode.object [ ( "title", Encode.string newItem.title ), ( "description", Encode.string newItem.description ) ]