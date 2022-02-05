module Data exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Field as Field
import Json.Decode.Pipeline exposing (required, optional)

type alias Item =
    { id : Int
    , title : String
    , description : String
    }

itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" int
        |> required "title" string
        |> optional "description" string ""

type alias Stats =
    { item_count : Int
    , aggregated_votes_count : Int
    , unaggregated_votes_count : Int
    }

statsDecoder : Decoder Stats
statsDecoder =
    Decode.succeed Stats
        |> required "item_count" int
        |> required "aggregated_votes_count" int
        |> required "unaggregated_votes_count" int

type ItemResource
    = Image ImageResource
    | Text String
    | Audio AudioResource

type alias ImageResource =
    { url : String }

type alias AudioResource =
    { url : String }

itemResourceDecoder : Decoder ItemResource
itemResourceDecoder =
    Field.require "type" Decode.string
    (\type_ -> case type_ of
                "image" ->
                    Field.require "data" imageResourceDecoder
                        (\data -> Image data |> Decode.succeed)
                "text" ->
                    Field.require "data" string
                        (\data -> Text data |> Decode.succeed)
                "audio" ->
                    Field.require "data" audioResourceDecoder
                        (\data -> Audio data |> Decode.succeed)
                _ -> Decode.fail "No type given"
    )

imageResourceDecoder : Decoder ImageResource
imageResourceDecoder =
    Decode.succeed ImageResource
        |> required "url" string

audioResourceDecoder : Decoder AudioResource
audioResourceDecoder =
    Decode.succeed AudioResource
        |> required "url" string