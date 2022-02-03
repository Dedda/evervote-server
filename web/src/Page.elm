module Page exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)

type Page
    = Other

view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Page"
    , body = headerBar :: [content]}

headerBar : Html msg
headerBar =
    nav []
        [ nav 
            [ classList 
                [ ("navbar", True)
                , ("navbar-default", True)
                , ("navbar-inverse", True)
                , ("navbar-fixed-top", True) 
                ]        
            ]
            [ div [ class "container" ]
                [ div [ class "navbar-header" ] 
                    [ a [ class "navbar-brand", Route.href Route.Index ] [ text "Evervote" ]
                    ]
                , div [ classList [ ("collapse", True), ("navbar-collapse", True) ] ] 
                    [ ul [ classList [ ("nav", True), ("navbar-nav", True) ]]
                        [ navLi Route.ItemList "Items"
                        ]
                    ]
                ]
            ]
        ]

navLi : Route -> String -> Html msg
navLi route label =
    li [] [ a [ Route.href route ] [ text label ] ]