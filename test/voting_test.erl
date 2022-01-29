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

add_votes_test() ->
  voting:start_link(),
  voting:add_votes({1, 1}, {3, -1}),
  voting:add_votes({2, 1}, {3, 2}),
  ?assertEqual(1, voting:get_votes(3)),
  voting:stop().