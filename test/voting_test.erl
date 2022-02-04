%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 14:22
%%%-------------------------------------------------------------------
-module(voting_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/items.hrl").
-include("../src/voting.hrl").

votes_count_test() ->
  voting:start_link(),
  voting:add_votes([vote(1, 1), vote(3, -1), vote(2, 1), vote(3, 1)]),
  ?assertEqual(4, voting:votes_count()),
  voting:stop().

vote(ItemId, Score) ->
  #vote{item_id = ItemId, score = Score}.