module Page.Index exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Session exposing (Session)
import Route exposing (Route)
import Data exposing (Stats, statsDecoder)
import Views exposing (..)

type alias Model = 
    { session : Session
    , stats : Maybe Stats
    }

type Msg =
    GotStats (Result Http.Error Stats)

toSession : Model -> Session
toSession model =
    model.session

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing
    , Cmd.batch
        [ fetchStats ]
    )

fetchStats : Cmd Msg
fetchStats =
    Http.get
        { url = "/stats"
        , expect = Http.expectJson GotStats statsDecoder }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        GotStats result ->
            case result of
                Ok stats -> ( { model | stats = Just stats }, Cmd.none )
                Err _ -> ( model, Cmd.none )    

view : Model -> { title : String, content : Html Msg }
view model =
    let
        sTable = case model.stats of
                Nothing -> []
                Just stats -> [ statsTable stats ]
    in
    { title = "Index"
    , content = div [ class "container" ] 
                    (h1 [] [ text "Stats" ] :: sTable)
    }