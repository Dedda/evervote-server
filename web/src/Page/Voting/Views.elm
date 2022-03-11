module Page.Voting.Views exposing (..)

import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

itemResourceCarousel : List ItemResource -> Html a
itemResourceCarousel resources =
    div [] (List.map presentItemResource resources)

presentItemResource : ItemResource -> Html a
presentItemResource resource =
    case resource of
        Image imageResource ->
            presentImageResource imageResource
        Text text ->
            presentTextResource text
        Audio audioResource ->
            presentAudioResource audioResource

presentImageResource : ImageResource -> Html a
presentImageResource image = div [] []

presentTextResource : String -> Html a
presentTextResource text = div [] []

presentAudioResource : AudioResource -> Html a
presentAudioResource audio = div [] []