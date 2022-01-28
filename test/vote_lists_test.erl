%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 20:22
%%%-------------------------------------------------------------------
-module(vote_lists_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

cannot_find_votes_test() ->
  Votes = #{},
  Expected = 0,
  Expected = vote_lists:find_votes(1, Votes).

find_votes_test() ->
  Votes = #{
    1 => 4,
    2 => 3,
    3 => 5
  },
  Expected = 3,
  Expected = vote_lists:find_votes(2, Votes).

total_votes_test() ->
  Votes = #{
    2 => 3,
    4 => 5,
    5 => 4
  },
  {3, 12} = vote_lists:total_votes(Votes).