module Page.ItemList exposing (..)
import Session exposing (Session)
import Html exposing (..)
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
    { title = "Items" 
    , content = a [ Route.href Route.Index ] [text "To Index"]
    }