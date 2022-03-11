module Decoding exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Json.Decode exposing (..)

import Data exposing (..)

itemJson : String
itemJson =
    """{
        "id": 123,
        "title": "Title",
        "description": "Description"
    }"""

textResourceJson : String
textResourceJson =
    """{
        "type": "text",
        "data": "Some sample text."
    }"""

imageResourceJson : String
imageResourceJson =
    """{
        "type": "image",
        "data": {
            "url": "https://localhost/image.png"
        }
    }"""

suite : Test
suite =
    describe "Decoder tests"
        [ test "Decode item" <|
            \_ -> decodeString itemDecoder itemJson |> Expect.equal (Ok (Item 123 "Title" "Description"))
        , test "Decode text resource" <|
            \_ -> decodeString itemResourceDecoder textResourceJson |> Expect.equal (Ok (Text "Some sample text."))
        , test "Decode image resource" <|
            \_ -> decodeString itemResourceDecoder imageResourceJson |> Expect.equal (Ok (Image (ImageResource "https://localhost/image.png")))
        ]