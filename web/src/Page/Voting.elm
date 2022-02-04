module Page.Voting exposing (..)

import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Session exposing (..)

type alias Model =
    { session : Session
    , items: Maybe ( Item, Item )}

type Msg
    = GotItems ( Result Http.Error ( Item, Item ) )
    | VoteForItem Item Item
    | VoteCast ( Result Http.Error () )

toSession : Model -> Session
toSession model = model.session

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session Nothing, fetchItems )

fetchItems : Cmd Msg
fetchItems =
    Http.get
            { url = "/items/random_pair"
            , expect = Http.expectJson GotItems itemPairDecoder
            }

sendVote : Item -> Item -> Cmd Msg
sendVote winner loser =
    let
        wId = String.fromInt winner.id
        lId = String.fromInt loser.id
    in
    Http.get
            { url = "/vote?winner=" ++ wId ++ "&loser=" ++ lId
            , expect = Http.expectWhatever VoteCast }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItems result ->
            case result of
                Ok ( item1, item2 ) -> ( { model | items = Just ( item1, item2 ) }, Cmd.none )
                Err _ -> ( model, Cmd.none )
        VoteForItem winner loser ->
            ( model, sendVote winner loser )
        VoteCast result ->
            case result of
                Ok _ -> ( { model | items = Nothing }, fetchItems )
                Err _ -> ( model, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Voting"
    , content = div [ class "container" ]
                    [ div [ class "row" ] [viewVoting model] ]
    }

viewVoting : Model -> Html Msg
viewVoting model =
    case model.items of
        Just items -> viewItems items
        Nothing -> div [] []

viewItems : ( Item, Item ) -> Html Msg
viewItems ( item1, item2 ) =
    div []
        [ itemCol item1 item2
        , itemCol item2 item1
        ]

itemCol : Item -> Item -> Html Msg
itemCol item opponent =
    div [ classList [("col-lg-6", True), ("vote-item", True)] ] [ itemView item opponent ]

itemView : Item -> Item -> Html Msg
itemView item opponent = div []
                    [ h4 [] [ text item.title ]
                    , p [] [ text item.description ]
                    , button [ onClick (VoteForItem item opponent) ] [ text "Vote!" ]
                    ]

itemPairDecoder : Decoder ( Item, Item )
itemPairDecoder =
    Decode.map2 Tuple.pair
        ( Decode.index 0 itemDecoder )
        ( Decode.index 1 itemDecoder )