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
  ?assertEqual(Expected, vote_lists:find_votes(1, Votes)).

add_vote_test() ->
  Votes = #{
    1 => 2,
    2 => 3
  },
  Expected = #{
    1 => 2,
    2 => 4
  },
  ?assertEqual(Expected, vote_lists:add_vote({2, 1}, Votes)).

find_votes_test() ->
  Votes = #{
    1 => 4,
    2 => 3,
    3 => 5
  },
  Expected = 3,
  ?assertEqual(Expected, vote_lists:find_votes(2, Votes)).

total_votes_test() ->
  Votes = #{
    2 => 3,
    4 => 5,
    5 => 4
  },
  ?assertEqual({3, 12}, vote_lists:total_votes(Votes)).