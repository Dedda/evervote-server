%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2022 7:01 PM
%%%-------------------------------------------------------------------
-author("dedda").

-ifndef(VOTING_HRL).
-define(VOTING_HRL, 1).

-include("items.hrl").

-record(vote, { item_id :: item_id(), score :: integer(), cast = calendar:universal_time() :: calendar:datetime()}).
-type vote() :: #vote{}.

-type vote_list() :: [vote()].

-endif.