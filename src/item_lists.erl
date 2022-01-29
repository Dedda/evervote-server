%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 20:14
%%%-------------------------------------------------------------------
-module(item_lists).
-author("dedda").

%% API
-export([find_by_id/2]).

-include("items.hrl").

-spec find_by_id(item_id(), items_map()) -> {error, not_found} | {ok, item()}.
find_by_id(ItemId, Items) ->
  case maps:get(ItemId, Items, not_found) of
    not_found -> {error, not_found};
    Found -> {ok, Found}
  end.