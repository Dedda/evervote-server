%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 16:20
%%%-------------------------------------------------------------------
-module(items).
-author("dedda").

%% API
-export([to_map/1, to_binary_map/1, item_from_binary_map/1, item_from_binary_map_optional_id/2]).
-include("items.hrl").

-spec to_map(item()) -> #{ atom() => pos_integer() | string() }.
to_map(#item{id = Id, title = Title, description = Description}) ->
  #{
    id => Id,
    title => Title,
    description => Description
  }.

-spec to_binary_map(item()) -> #{ atom() => pos_integer() | binary() }.
to_binary_map(#item{id = Id, title = Title, description = Description}) ->
  #{
    id => Id,
    title => list_to_binary(Title),
    description => list_to_binary(Description)
  }.

-spec item_from_binary_map(#{ binary() => binary() }) -> {ok, item()}.
item_from_binary_map(Data) ->
  #item{
    id = maps:get(<<"id">>, Data),
    title = binary_to_list(maps:get(<<"title">>, Data)),
    description = binary_to_list(maps:get(<<"description">>, Data, <<"">>))
  }.

-spec item_from_binary_map_optional_id(#{ binary() => binary() }, pos_integer()) -> {ok, item()}.
item_from_binary_map_optional_id(Data, DefaultId) ->
  #item{
    id = maps:get(<<"id">>, Data, DefaultId),
    title = binary_to_list(maps:get(<<"title">>, Data)),
    description = binary_to_list(maps:get(<<"description">>, Data, <<"">>))
  }.
