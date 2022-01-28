%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 20:11
%%%-------------------------------------------------------------------
-module(item_lists_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

find_by_id_in_empty_list_test() ->
  Expected = {error, no_items},
  Expected = item_lists:find_by_id(1, []).

find_by_id_test() ->
  Item = {3, "Item 3", "This is expected"},
  List = [{1, "Item 1", "First"}, {2, "Item 2", "Second"}, Item, {4, "Item 4", "Forth"}],
  Expected = {ok, Item},
  Expected = item_lists:find_by_id(3, List).

cannot_find_item_by_id_test() ->
  List = [{1, "Item 1", "First"}, {2, "Item 2", "Second"}, {4, "Item 4", "Forth"}],
  Expected = {error, not_found},
  Expected = item_lists:find_by_id(3, List).
