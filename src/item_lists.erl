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

find_by_id(_ItemId, []) ->
  {error, no_items};
find_by_id(ItemId, Items) ->
  Pred = fun({CurrentId, _, _}) -> CurrentId =:= ItemId end,
  case lists:filter(Pred, Items) of
    [] -> {error, not_found};
    [Item | _] -> {ok, Item}
  end.