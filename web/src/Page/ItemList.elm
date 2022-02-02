module Page.ItemList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Session exposing (Session)

type alias Model = 
    { session : Session 
    }

type alias Msg = {}

toSession : Model -> Session
toSession model = 
    model.session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Items" 
    , content = div [ class "container" ] [ a [ Route.href Route.Index ] [text "To Index"] ]
    }