module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Index as Index
import Page.ItemList as ItemList
import Page as Page
import Page.Blank as Blank
import Url exposing (Url)
import Browser exposing (Document)
import Route exposing (..)
import Json.Decode exposing (Value)
import Session exposing (..)
import Html.Events exposing (onClick)

type Model
    = Redirect Session
    | Index Index.Model
    | ItemList ItemList.Model

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest
  | GotIndexMsg Index.Msg
  | GotItemListMsg ItemList.Msg

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
  changeRouteTo (Route.fromUrl url) (Redirect (Guest navKey))

view : Model -> Document Msg
view model =
  let
    viewPage page toMsg config =
      let
        { title, body } =
          Page.view page config
      in
      { title = title 
      , body = List.map (Html.map toMsg) body
      }
  in
  case model of
    Redirect _ ->
      Page.view Page.Other Blank.view
    Index index ->
      viewPage Page.Other GotIndexMsg (Index.view index)
    ItemList itemList ->
      viewPage Page.Other GotItemListMsg (ItemList.view itemList)

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
  let
    session =
      toSession model
  in
  case maybeRoute of
     Nothing -> ( Index { session = session }, Cmd.none )
     Just Route.Index -> ( Index { session = session }, Cmd.none )
     Just Route.ItemList -> ( ItemList { session = session }, Cmd.none )    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case (msg, model) of
    ( ClickedLink urlRequest, _) ->
      case urlRequest of
        Browser.Internal url ->
          case url.fragment of
              Nothing ->
                ( model, Cmd.none )
              Just _ ->
                ( model
                , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                )
        Browser.External href ->
          ( model
          , Nav.load href
          )
    ( ChangedUrl url, _ ) ->
        changeRouteTo (Route.fromUrl url) model
    ( GotIndexMsg subMsg, Index index ) ->
        Index.update subMsg index
          |> updateWith Index GotIndexMsg model
    ( GotItemListMsg subMsg, ItemList itemList ) ->
        ItemList.update subMsg itemList
          |> updateWith ItemList GotItemListMsg model
    ( _, _ ) ->
        ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
  ( toModel subModel
  , Cmd.map toMsg subCmd
  )

toSession : Model -> Session
toSession model =
  case model of
    Redirect session ->
      session
    Index index ->
      Index.toSession index
    ItemList itemList ->
      ItemList.toSession itemList

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    }
