module Page.Voting exposing (..)

import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Session exposing (..)
import Page.Voting.Views exposing (..)

type ItemsState
    = ItemsFine (Item, Item)
    | ItemsEmpty
    | ItemsFailed

type ResourcesState
    = ResourceFine (List ItemResource)
    | ResourceEmpty
    | ResourceFailed

type alias Model =
    { session : Session
    , items: ItemsState
    , resources: (ResourcesState, ResourcesState)
    }

type Msg
    = GotItems ( Result Http.Error ( Item, Item ) )
    | GotResourcesLeftItem ( Result Http.Error ( List ItemResource ) )
    | GotResourcesRightItem ( Result Http.Error ( List ItemResource ) )
    | VoteForItem Item Item
    | VoteCast ( Result Http.Error () )
    | FetchItems

toSession : Model -> Session
toSession model = model.session

init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session ItemsEmpty (ResourceEmpty, ResourceEmpty), fetchItems )

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
    let
        (leftRes, rightRes) = model.resources
    in
    case msg of
        GotItems result ->
            case result of
                Ok ( item1, item2 ) -> ( { model | items = ItemsFine ( item1, item2 ) }, Cmd.none )
                Err _ -> ( { model | items = ItemsFailed }, Cmd.none )
        VoteForItem winner loser ->
            ( model, sendVote winner loser )
        VoteCast result ->
            case result of
                Ok _ -> ( { model | items = ItemsEmpty }, fetchItems )
                Err _ -> ( model, Cmd.none )
        FetchItems ->
            ( model, fetchItems )
        GotResourcesLeftItem result ->
            case result of
                Ok ( res ) -> ( { model | resources = ( ResourceFine res, rightRes ) }, Cmd.none )
                Err _ -> ( { model | resources = ( ResourceFailed, rightRes ) }, Cmd.none )
        GotResourcesRightItem result ->
            case result of
                Ok ( res ) -> ( { model | resources = ( leftRes, ResourceFine res ) }, Cmd.none )
                Err _ -> ( { model | resources = ( leftRes, ResourceFailed ) }, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Voting"
    , content = div [ class "container" ]
                    [ div [ class "row" ] [viewVoting model] ]
    }

viewVoting : Model -> Html Msg
viewVoting model =
    let
        (resLeft, resRight) = model.resources
    in
    case model.items of
        ItemsFine (left, right) -> viewItems (left, resLeft ) ( right, resRight)
        ItemsEmpty -> div [] []
        ItemsFailed -> itemsFetchErrorView

viewItems : ( Item, ResourcesState ) -> ( Item, ResourcesState ) -> Html Msg
viewItems ( item1, resources1 )  ( item2, resources2 ) =
    div []
        [ itemCol item1 resources1 item2
        , itemCol item2 resources2 item1
        ]

itemCol : Item -> ResourcesState -> Item -> Html Msg
itemCol item itemResources opponent =
    div [ classList [("col-lg-6", True), ("vote-item", True)] ] [ itemView item itemResources opponent ]

itemView : Item -> ResourcesState -> Item -> Html Msg
itemView item itemResources opponent = div []
                    [ h4 [] [ text item.title ]
                    , itemResourcesView itemResources
                    , p [] [ text item.description ]
                    , button [ onClick (VoteForItem item opponent) ] [ text "Vote!" ]
                    ]

itemResourcesView : ResourcesState -> Html Msg
itemResourcesView resources =
    case resources of
        ResourceFine res -> itemResourceCarousel res
        ResourceEmpty -> div [] []
        ResourceFailed -> div [] []

itemsFetchErrorView : Html Msg
itemsFetchErrorView =
    div [ classList [("error-msg", True), ("alert", True), ("alert-danger", True)] ]
        [ p [] [ text "Error loading items!" ]
        , button [ onClick FetchItems ] [ text "Retry!" ]
        ]

itemPairDecoder : Decoder ( Item, Item )
itemPairDecoder =
    Decode.map2 Tuple.pair
        ( Decode.index 0 itemDecoder )
        ( Decode.index 1 itemDecoder )