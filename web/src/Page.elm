module Page exposing (..)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)

type Page
    = Other

view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Page"
    , body = headerBar :: [content]}

headerBar : Html msg
headerBar =
    div [] 
        [ div [ class "header-item" ] [text "Home"] ]