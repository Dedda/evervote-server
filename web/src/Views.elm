module Views exposing (..)

import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

tableHeader : List String -> Html msg
tableHeader labels =
    thead [] 
        [ tr [] (List.map (\label -> th [ scope "col" ] [ text label ]) labels) ]

itemsTable : List Item -> Html msg
itemsTable items =
    table [ class "table" ]
        [ tableHeader [ "ID", "title", "description" ]
        , tbody [] (List.map itemRow items) ]

itemRow : Item -> Html msg
itemRow item = 
    tr [] 
        [ th [ scope "row" ] [ text (String.fromInt item.id) ] 
        , td [] [ text item.title ]
        , td [] [ text item.description ]
        ]

statsTable : Stats -> Html msg
statsTable stats =
    table [ class "table" ]
        [ tableHeader [ "Key", "Value" ]
        , tbody []
            [ tr []
                [ td [] [ text "Item Count" ] 
                , td [] [ text (String.fromInt stats.item_count) ]
                ]
            , tr []
                [ td [] [ text "Aggregated Votes Count" ]
                , td [] [ text (String.fromInt stats.aggregated_votes_count) ]
                ]
            , tr []
                [ td [] [ text "Unaggregated Votes Count" ]
                , td [] [ text (String.fromInt stats.unaggregated_votes_count) ]
                ]
            ]
        ]