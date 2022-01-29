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
-export([to_map/1, to_binary_map/1]).
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
