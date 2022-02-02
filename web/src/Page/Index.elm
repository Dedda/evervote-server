module Page.Index exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    , content = div [ class "container" ] 
                    [ div [ class "jumbotron" ] 
                        [ div [ class "row" ] 
                            [ a [ Route.href Route.ItemList ] [text "To Items"] 
                            ]
                        ]
                    ]
    }