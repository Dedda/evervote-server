%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 21:28
%%%-------------------------------------------------------------------
-module(aggregate_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/aggregate.hrl").
-include("../src/voting.hrl").

aggregate_incremental_test() ->
  Before = #{
    1 => #aggregated_vote{item = 1, votes = 5, score = -1},
    3 => #aggregated_vote{item = 3, votes = 2, score = 0}
  },
  Votes = [
    #vote{item_id = 2, score = 1},
    #vote{item_id = 1, score = 1}
  ],
  Expected = #{
    1 => #aggregated_vote{item = 1, votes = 6, score = 0},
    2 => #aggregated_vote{item = 2, votes = 1, score = 1},
    3 => #aggregated_vote{item = 3, votes = 2, score = 0}
  },
  {ok, Actual} = aggregate:aggregate_incremental(Votes, Before),
  ?assertEqual(Expected, Actual).
