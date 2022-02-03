module Page.ItemList exposing (..)

import Data exposing (Item)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Views exposing (itemsTable)

type alias Model = 
    { session : Session
    , items : Maybe (List Item)
    }

type Msg 
    = GotItems (List Item)

toSession : Model -> Session
toSession model = 
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

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
