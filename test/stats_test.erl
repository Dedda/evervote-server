%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 21:28
%%%-------------------------------------------------------------------
-module(stats_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/items.hrl").
-include("../src/stats.hrl").

item_count_test() ->
  item_cache:start_link(),
  item_cache:set_votes(#{
    1 => #item{id = 1, title = "Title 1", description = "Description 1"},
    2 => #item{id = 2, title = "Title 2", description = "Description 2"}
  }),
  Stats = stats:current(),
  ?assertEqual(2, length(maps:keys(item_cache:get_all()))),
  ?assertEqual(2, Stats#stats.item_count),
  item_cache:stop().