module Page.Index exposing (..)

import Html exposing (..)
import Session exposing (Session)
import Route exposing (Route)

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
    { title = "Index"
    , content = a [ Route.href Route.ItemList ] [text "To Items"]
    }