%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2022 17:33
%%%-------------------------------------------------------------------
-author("dedda").

-include("items.hrl").

-record(aggregated_vote, { item :: item_id(), votes = 0 :: pos_integer(), score :: integer()}).
-type aggregated_vote() :: #aggregated_vote{}.

-record(aggregated_votes, {}).
-type aggregated_votes() :: #aggregated_votes{}.