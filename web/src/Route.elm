module Route exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)
import Html exposing (Attribute)
import Html.Attributes as Attr

type Route
    = Index
    | ItemList

parser : Parser ( Route -> a ) a
parser =
    oneOf
        [ Parser.map Index (Parser.top)
        , Parser.map ItemList (s "items")
        ]

fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)

routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)

routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Index ->
            []
        ItemList ->
            [ "items" ]