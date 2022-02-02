module Page exposing (..)
import Browser exposing (Document)
import Html exposing (Html)

type Page
    = Other

view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Page"
    , body = [content]}