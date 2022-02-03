module Page.ItemList exposing (..)

import Data exposing (Item)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Session exposing (Session)

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
        itemsTable = case model.items of
                        Nothing -> []
                        Just items -> [ viewItemsTable items ]
    in
    { title = "Items" 
    , content = div [ class "container" ] 
                    ( a [ Route.href Route.Index ] [text "To Index"] :: itemsTable )                    
    }

viewItemsTable : List Item -> Html Msg
viewItemsTable items =
    table [ class "table" ]
        [ itemsTableHeader
        , tbody [] (List.map viewItem items) ]

itemsTableHeader : Html Msg
itemsTableHeader =
    thead [] 
        [ tr [] 
            [ th [ scope "col" ] [ text "ID" ]
            , th [ scope "col" ] [ text "Title" ]
            , th [ scope "col" ] [ text "Description" ]
            ]
        ]        

viewItem : Item -> Html Msg
viewItem item =
    tr [] 
        [ th [ scope "row" ] [ text (String.fromInt item.id) ] 
        , td [] [ text item.title ]
        , td [] [ text item.description ]
        ]