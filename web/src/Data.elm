module Data exposing (..)

import Json.Decode as Decode exposing (Decoder, decodeString, int, nullable, string)
import Json.Decode.Pipeline exposing (required, optional)

type alias Item =
    { id : Int
    , title : String
    , description : String }

itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" int
        |> required "title" string
        |> optional "description" string ""

type alias Stats =
    { item_count : Int }

statsDecoder : Decoder Stats
statsDecoder =
    Decode.succeed Stats
        |> required "item_count" int