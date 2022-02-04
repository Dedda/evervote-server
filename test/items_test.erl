%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Feb 2022 12:01
%%%-------------------------------------------------------------------
-module(items_test).
-author("dedda").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/items.hrl").

to_map_test() ->
  Item = #item{
    id = 2,
    title = "Test",
    description = "Test description"
  },
  Expected = #{
    id => 2,
    title => "Test",
    description => "Test description"
  },
  ?assertEqual(Expected, items:to_map(Item)).

to_binary_map_test() ->
  Item = #item{
    id = 2,
    title = "Test",
    description = "Test description"
  },
  Expected = #{
    id => 2,
    title => <<"Test">>,
    description => <<"Test description">>
  },
  ?assertEqual(Expected, items:to_binary_map(Item)).

item_from_binary_map_test() ->
  BinaryMap = #{
    <<"id">> => 2,
    <<"title">> => <<"Test">>,
    <<"description">> => <<"Test description">>
  },
  Expected = #item{
    id = 2,
    title = "Test",
    description = "Test description"
  },
  ?assertEqual(Expected, items:item_from_binary_map(BinaryMap)).

item_from_binary_map_optional_id_test() ->
  BinaryMap = #{
    <<"title">> => <<"Test">>,
    <<"description">> => <<"Test description">>
  },
  Expected = #item{
    id = 2,
    title = "Test",
    description = "Test description"
  },
  ?assertEqual(Expected, items:item_from_binary_map_optional_id(BinaryMap, 2)).