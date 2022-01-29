%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 13:52
%%%-------------------------------------------------------------------
-module(item_cache_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/items.hrl").

add_items_test() ->
  Id = 3,
  item_cache:start_link(),
  item_cache:add(#item{id = Id, title = "Title", description = "Description"}),
  Items = item_cache:get_all(),
  ?assertEqual([Id], maps:keys(Items)),
  item_cache:stop().

get_random_from_empty_test() ->
  item_cache:start_link(),
  ?assertEqual({error, no_items}, item_cache:get_random()),
  item_cache:stop().

get_random_test() ->
  item_cache:start_link(),
  item_cache:set_votes(#{
    1 => #item{id = 1, title = "Title 1", description = "Description 1"},
    2 => #item{id = 2, title = "Title 2", description = "Description 2"}
  }),
  ?assertEqual(2, length(maps:keys(item_cache:get_all()))),
  {Status, _Item} = item_cache:get_random(),
  ?assertEqual(ok, Status),
  item_cache:stop().